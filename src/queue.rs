use std::{
    cmp::{max, min},
    collections::{hash_map::Entry, HashMap, VecDeque},
    convert::TryInto,
    sync::Arc,
    time::{Duration, Instant},
};

use shakmaty::{uci::IllegalUciError, variant::VariantPosition, PositionError};
use tokio::{
    sync::{mpsc, oneshot, Mutex, Notify},
    time,
};
use url::Url;

use crate::{
    api::{AcquireQuery, AcquireResponseBody, Acquired, AnalysisPart, ApiStub, BatchId, Work},
    assets::{EngineFlavor, EvalFlavor},
    configure::{BacklogOpt, Endpoint},
    ipc::{Position, PositionFailed, PositionId, PositionResponse, Pull},
    logger::{Logger, ProgressAt, QueueStatusBar},
    notation::{normalize_moves, NormalizeError, Uci, Variant},
    stats::{NpsRecorder, Stats, StatsRecorder},
    util::{NevermindExt as _, RandomizedBackoff},
};

pub fn channel(
    opt: BacklogOpt,
    cores: usize,
    api: ApiStub,
    max_backoff: Duration,
    logger: Logger,
) -> (QueueStub, QueueActor) {
    let (tx, rx) = mpsc::unbounded_channel();
    let interrupt = Arc::new(Notify::new());
    let state = Arc::new(Mutex::new(QueueState::new(cores, logger.clone())));
    let stub = QueueStub {
        tx: Some(tx),
        interrupt: interrupt.clone(),
        state: state.clone(),
        api: api.clone(),
    };
    let actor = QueueActor {
        rx,
        interrupt,
        state,
        api,
        opt,
        logger,
        backoff: RandomizedBackoff::new(max_backoff),
    };
    (stub, actor)
}

#[derive(Clone)]
pub struct QueueStub {
    tx: Option<mpsc::UnboundedSender<QueueMessage>>,
    interrupt: Arc<Notify>,
    state: Arc<Mutex<QueueState>>,
    api: ApiStub,
}

impl QueueStub {
    pub async fn pull(&mut self, pull: Pull) {
        let mut state = self.state.lock().await;
        let (response, callback) = pull.split();
        if let Some(response) = response {
            state.handle_position_response(self.clone(), response);
        }
        if let Err(callback) = state.try_pull(callback) {
            if let Some(ref mut tx) = self.tx {
                tx.send(QueueMessage::Pull { callback })
                    .nevermind("queue dropped");
            }
        }
    }

    fn move_submitted(&mut self) {
        if let Some(ref tx) = self.tx {
            tx.send(QueueMessage::MoveSubmitted).nevermind("too late");

            // Skip the queue backoff.
            self.interrupt.notify_one();
        }
    }

    pub async fn shutdown_soon(&mut self) {
        let mut state = self.state.lock().await;
        state.shutdown_soon = true;
        self.tx.take();
        self.interrupt.notify_one();
    }

    pub async fn shutdown(mut self) {
        self.shutdown_soon().await;

        let mut state = self.state.lock().await;
        for (k, _) in state.pending.drain() {
            self.api.abort(k);
        }
    }

    pub async fn stats(&self) -> (Stats, NpsRecorder) {
        let state = self.state.lock().await;
        (
            state.stats_recorder.stats.clone(),
            state.stats_recorder.nnue_nps.clone(),
        )
    }
}

struct QueueState {
    shutdown_soon: bool,
    cores: usize,
    incoming: VecDeque<Position>,
    pending: HashMap<BatchId, PendingBatch>,
    move_submissions: VecDeque<CompletedBatch>,
    stats_recorder: StatsRecorder,
    logger: Logger,
}

impl QueueState {
    fn new(cores: usize, logger: Logger) -> QueueState {
        QueueState {
            shutdown_soon: false,
            cores,
            incoming: VecDeque::new(),
            pending: HashMap::new(),
            move_submissions: VecDeque::new(),
            stats_recorder: StatsRecorder::open(cores),
            logger,
        }
    }

    fn status_bar(&self) -> QueueStatusBar {
        QueueStatusBar {
            pending: self.pending.values().map(|p| p.pending()).sum(),
            cores: self.cores,
        }
    }

    fn add_incoming_batch(&mut self, batch: IncomingBatch) {
        match self.pending.entry(batch.work.id()) {
            Entry::Occupied(entry) => self.logger.error(&format!(
                "Dropping duplicate incoming batch {}",
                entry.key()
            )),
            Entry::Vacant(entry) => {
                let progress_at = ProgressAt::from(&batch);

                // Reversal only for cosmetics when displaying progress.
                let mut positions = Vec::with_capacity(batch.positions.len());
                for pos in batch.positions.into_iter().rev() {
                    positions.insert(
                        0,
                        match pos {
                            Skip::Present(pos) => {
                                self.incoming.push_back(pos);
                                None
                            }
                            Skip::Skip => Some(Skip::Skip),
                        },
                    );
                }

                entry.insert(PendingBatch {
                    work: batch.work,
                    flavor: batch.flavor,
                    variant: batch.variant,
                    url: batch.url,
                    positions,
                    started_at: Instant::now(),
                });

                self.logger.progress(self.status_bar(), progress_at);
            }
        }
    }

    fn handle_position_response(
        &mut self,
        queue: QueueStub,
        res: Result<PositionResponse, PositionFailed>,
    ) {
        match res {
            Ok(res) => {
                let progress_at = ProgressAt::from(&res);
                let batch_id = res.work.id();
                if let Some(pending) = self.pending.get_mut(&batch_id) {
                    if let Some(pos) = pending.positions.get_mut(res.position_id.0) {
                        *pos = Some(Skip::Present(res));
                    }
                }
                self.logger.progress(self.status_bar(), progress_at);
                self.maybe_finished(queue, batch_id);
            }
            Err(failed) => {
                // Just forget about batches with failed positions,
                // intentionally letting them time out, instead of handing
                // them to the next client.
                self.pending.remove(&failed.batch_id);
                self.incoming.retain(|p| p.work.id() != failed.batch_id);
            }
        }
    }

    fn try_pull(
        &mut self,
        callback: oneshot::Sender<Position>,
    ) -> Result<(), oneshot::Sender<Position>> {
        if let Some(position) = self.incoming.pop_front() {
            if let Err(err) = callback.send(position) {
                self.incoming.push_front(err);
            }
            Ok(())
        } else {
            Err(callback)
        }
    }

    fn maybe_finished(&mut self, mut queue: QueueStub, batch: BatchId) {
        if let Some(pending) = self.pending.remove(&batch) {
            match pending.try_into_completed() {
                Ok(completed) => {
                    let mut extra = Vec::new();
                    extra.extend(completed.variant.short_name());
                    if completed.flavor.eval_flavor().is_hce() {
                        extra.push("hce".to_owned());
                    }
                    extra.push(match completed.nps() {
                        Some(nps) => {
                            let nnue_nps = if completed.flavor.eval_flavor() == EvalFlavor::Nnue {
                                Some(nps)
                            } else {
                                None
                            };
                            self.stats_recorder.record_batch(
                                completed.total_positions(),
                                completed.total_nodes(),
                                nnue_nps,
                            );
                            format!("{} knps", nps / 1000)
                        }
                        None => "? nps".to_owned(),
                    });
                    let log = match completed.url {
                        Some(ref url) => format!(
                            "{} {} finished ({})",
                            self.status_bar(),
                            url,
                            extra.join(", ")
                        ),
                        None => format!(
                            "{} {} finished ({})",
                            self.status_bar(),
                            batch,
                            extra.join(", ")
                        ),
                    };
                    match completed.work {
                        Work::Analysis { id, .. } => {
                            self.logger.info(&log);
                            queue.api.submit_analysis(
                                id,
                                completed.flavor.eval_flavor(),
                                completed.into_analysis(),
                            );
                        }
                        Work::Move { .. } => {
                            self.logger.debug(&log);
                            self.move_submissions.push_back(completed);
                            queue.move_submitted();
                        }
                    }
                }
                Err(pending) => {
                    if !pending.work.matrix_wanted() {
                        // Send partially analysis as progress report.
                        let progress_report = pending.progress_report();
                        if progress_report.iter().filter(|p| p.is_some()).count() % (self.cores * 2)
                            == 0
                        {
                            queue.api.submit_analysis(
                                pending.work.id(),
                                pending.flavor.eval_flavor(),
                                progress_report,
                            );
                        }
                    }

                    self.pending.insert(pending.work.id(), pending);
                }
            }
        }
    }
}

#[derive(Debug)]
enum QueueMessage {
    Pull { callback: oneshot::Sender<Position> },
    MoveSubmitted,
}

pub struct QueueActor {
    rx: mpsc::UnboundedReceiver<QueueMessage>,
    interrupt: Arc<Notify>,
    state: Arc<Mutex<QueueState>>,
    api: ApiStub,
    opt: BacklogOpt,
    backoff: RandomizedBackoff,
    logger: Logger,
}

impl QueueActor {
    pub async fn run(self) {
        self.logger.debug("Queue actor started");
        self.run_inner().await;
    }

    pub async fn backlog_wait_time(&mut self) -> (Duration, AcquireQuery) {
        let sec = Duration::from_secs(1);
        let min_user_backlog = {
            let state = self.state.lock().await;
            state.stats_recorder.min_user_backlog()
        };
        let user_backlog = max(
            min_user_backlog,
            self.opt.user.map(Duration::from).unwrap_or_default(),
        );
        let system_backlog = self.opt.system.map(Duration::from).unwrap_or_default();

        if user_backlog >= sec || system_backlog >= sec {
            if let Some(status) = self.api.status().await {
                let user_wait = user_backlog
                    .checked_sub(status.user.oldest)
                    .unwrap_or_default();
                let system_wait = system_backlog
                    .checked_sub(status.system.oldest)
                    .unwrap_or_default();
                self.logger.debug(&format!("User wait: {:?} due to {:?} for oldest {:?}, system wait: {:?} due to {:?} for oldest {:?}",
                       user_wait, user_backlog, status.user.oldest,
                       system_wait, system_backlog, status.system.oldest));
                let slow = user_wait >= system_wait + sec;
                (min(user_wait, system_wait), AcquireQuery { slow })
            } else {
                self.logger
                    .debug("Queue status not available. Will not delay acquire.");
                let slow = user_backlog >= system_backlog + sec;
                (Duration::default(), AcquireQuery { slow })
            }
        } else {
            (Duration::default(), AcquireQuery { slow: false })
        }
    }

    async fn handle_acquired_response_body(&mut self, body: AcquireResponseBody) {
        let context = ProgressAt {
            batch_id: body.work.id(),
            batch_url: body.batch_url(self.api.endpoint()),
            position_id: None,
        };

        match IncomingBatch::from_acquired(self.api.endpoint(), body) {
            Ok(incoming) => {
                let mut state = self.state.lock().await;
                state.add_incoming_batch(incoming);
            }
            Err(IncomingError::AllSkipped(completed)) => {
                self.logger
                    .warn(&format!("Completed empty batch {}.", context));
                self.api.submit_analysis(
                    completed.work.id(),
                    completed.flavor.eval_flavor(),
                    completed.into_analysis(),
                );
            }
            Err(err) => {
                self.logger
                    .warn(&format!("Ignoring invalid batch {}: {:?}", context, err));
            }
        }
    }

    async fn handle_move_submissions(&mut self) {
        loop {
            let next = {
                let mut state = self.state.lock().await;
                if state.shutdown_soon {
                    // Each move submision can come with a follow-up task,
                    // so we might never finish if we keep submitting.
                    // Just drop some. They are short-lived anyway.
                    break;
                }

                state.move_submissions.pop_front()
            };

            if let Some(completed) = next {
                if let Some(Acquired::Accepted(body)) = self
                    .api
                    .submit_move_and_acquire(completed.work.id(), completed.into_best_move())
                    .await
                {
                    self.handle_acquired_response_body(body).await;
                }
            } else {
                break;
            }
        }
    }

    async fn run_inner(mut self) {
        while let Some(msg) = self.rx.recv().await {
            match msg {
                QueueMessage::Pull { mut callback } => loop {
                    self.handle_move_submissions().await;

                    {
                        let mut state = self.state.lock().await;
                        callback = match state.try_pull(callback) {
                            Ok(()) => break,
                            Err(not_done) => not_done,
                        };

                        if state.shutdown_soon {
                            break;
                        }
                    }

                    let (wait, query) = tokio::select! {
                        _ = callback.closed() => break,
                        res = self.backlog_wait_time() => res,
                    };

                    if wait >= Duration::from_secs(1) {
                        if wait >= Duration::from_secs(40) {
                            self.logger.info(&format!("Going idle for {:?}.", wait));
                        } else {
                            self.logger.debug(&format!("Going idle for {:?}.", wait));
                        }

                        tokio::select! {
                            _ = callback.closed() => break,
                            _ = self.interrupt.notified() => continue,
                            _ = time::sleep(wait) => continue,
                        }
                    }

                    match self.api.acquire(query).await {
                        Some(Acquired::Accepted(body)) => {
                            self.backoff.reset();
                            self.handle_acquired_response_body(body).await;
                        }
                        Some(Acquired::NoContent) => {
                            let backoff = self.backoff.next();
                            self.logger
                                .debug(&format!("No job received. Backing off {:?}.", backoff));
                            tokio::select! {
                                _ = callback.closed() => break,
                                _ = self.interrupt.notified() => (),
                                _ = time::sleep(backoff) => (),
                            }
                        }
                        Some(Acquired::Rejected) => {
                            self.logger.error("Client update or reconfiguration might be required. Stopping queue.");
                            let mut state = self.state.lock().await;
                            state.shutdown_soon = true;
                        }
                        None => (),
                    }
                },
                QueueMessage::MoveSubmitted => self.handle_move_submissions().await,
            }
        }
    }
}

impl Drop for QueueActor {
    fn drop(&mut self) {
        self.logger.debug("Queue actor exited");
    }
}

#[derive(Debug, Clone)]
enum Skip<T> {
    Present(T),
    Skip,
}

impl<T> Skip<T> {
    fn is_skipped(&self) -> bool {
        matches!(self, Skip::Skip)
    }
}

#[derive(Debug, Clone)]
pub struct IncomingBatch {
    work: Work,
    flavor: EngineFlavor,
    variant: Variant,
    positions: Vec<Skip<Position>>,
    url: Option<Url>,
}

impl IncomingBatch {
    fn from_acquired(
        endpoint: &Endpoint,
        body: AcquireResponseBody,
    ) -> Result<IncomingBatch, IncomingError> {
        let url = body.batch_url(endpoint);
        let root_fen = body.position;

        let (flavor, body_moves) = normalize_moves(body.variant.clone(), &root_fen, &body.moves)?;

        Ok(IncomingBatch {
            work: body.work.clone(),
            url: url.clone(),
            flavor,
            variant: body.variant.clone(),
            positions: match body.work {
                Work::Move { .. } => {
                    vec![Skip::Present(Position {
                        work: body.work,
                        url,
                        flavor,
                        position_id: PositionId(0),
                        variant: body.variant,
                        root_fen,
                        moves: body_moves,
                    })]
                }
                Work::Analysis { .. } => {
                    let mut moves = Vec::new();
                    let mut positions = vec![Skip::Present(Position {
                        work: body.work.clone(),
                        url: url.clone().map(|mut url| {
                            url.set_fragment(Some("0"));
                            url
                        }),
                        flavor,
                        position_id: PositionId(0),
                        variant: body.variant.clone(),
                        root_fen: root_fen.clone(),
                        moves: moves.clone(),
                    })];

                    for (i, m) in body_moves.into_iter().enumerate() {
                        moves.push(m);
                        positions.push(Skip::Present(Position {
                            work: body.work.clone(),
                            url: url.clone().map(|mut url| {
                                url.set_fragment(Some(&(1 + i).to_string()));
                                url
                            }),
                            flavor,
                            position_id: PositionId(1 + i),
                            variant: body.variant.clone(),
                            root_fen: root_fen.clone(),
                            moves: moves.clone(),
                        }));
                    }

                    for skip in body.skip_positions.into_iter() {
                        if let Some(pos) = positions.get_mut(skip) {
                            *pos = Skip::Skip;
                        }
                    }

                    // Edge case: Batch is immediately completed, because all
                    // positions are skipped.
                    if positions.iter().all(Skip::is_skipped) {
                        let now = Instant::now();
                        return Err(IncomingError::AllSkipped(CompletedBatch {
                            work: body.work,
                            url,
                            flavor,
                            variant: body.variant,
                            positions: positions.into_iter().map(|_| Skip::Skip).collect(),
                            started_at: now,
                            completed_at: now,
                        }));
                    }

                    positions
                }
            },
        })
    }
}

impl From<&IncomingBatch> for ProgressAt {
    fn from(batch: &IncomingBatch) -> ProgressAt {
        ProgressAt {
            batch_id: batch.work.id(),
            batch_url: batch.url.clone(),
            position_id: None,
        }
    }
}

#[derive(Debug)]
enum IncomingError {
    Position(PositionError<VariantPosition>),
    IllegalUci(IllegalUciError),
    IllegalUciNormalization(NormalizeError),
    AllSkipped(CompletedBatch),
}

impl From<PositionError<VariantPosition>> for IncomingError {
    fn from(err: PositionError<VariantPosition>) -> IncomingError {
        IncomingError::Position(err)
    }
}

impl From<IllegalUciError> for IncomingError {
    fn from(err: IllegalUciError) -> IncomingError {
        IncomingError::IllegalUci(err)
    }
}

// TODO: handle both types of errors?
impl From<NormalizeError> for IncomingError {
    fn from(err: NormalizeError) -> IncomingError {
        IncomingError::IllegalUciNormalization(err)
    }
}

#[derive(Debug, Clone)]
struct PendingBatch {
    work: Work,
    url: Option<Url>,
    flavor: EngineFlavor,
    variant: Variant,
    positions: Vec<Option<Skip<PositionResponse>>>,
    started_at: Instant,
}

impl PendingBatch {
    fn try_into_completed(self) -> Result<CompletedBatch, PendingBatch> {
        match self.positions.clone().into_iter().collect() {
            Some(positions) => Ok(CompletedBatch {
                work: self.work,
                url: self.url,
                flavor: self.flavor,
                variant: self.variant,
                positions,
                started_at: self.started_at,
                completed_at: Instant::now(),
            }),
            None => Err(self),
        }
    }

    fn progress_report(&self) -> Vec<Option<AnalysisPart>> {
        self.positions
            .iter()
            .enumerate()
            .map(|(i, p)| match p {
                // Quirk: Lila distinguishes progress reports from complete
                // analysis by looking at the first part.
                Some(Skip::Present(pos)) if i > 0 => Some(pos.to_best()),
                _ => None,
            })
            .collect()
    }

    fn pending(&self) -> usize {
        self.positions.iter().filter(|p| p.is_none()).count()
    }
}

#[derive(Debug)]
pub struct CompletedBatch {
    work: Work,
    url: Option<Url>,
    flavor: EngineFlavor,
    variant: Variant,
    positions: Vec<Skip<PositionResponse>>,
    started_at: Instant,
    completed_at: Instant,
}

impl CompletedBatch {
    fn into_analysis(self) -> Vec<Option<AnalysisPart>> {
        self.positions
            .into_iter()
            .map(|p| {
                Some(match p {
                    Skip::Skip => AnalysisPart::Skipped { skipped: true },
                    Skip::Present(pos) if pos.work.matrix_wanted() => pos.into_matrix(),
                    Skip::Present(pos) => pos.to_best(),
                })
            })
            .collect()
    }

    fn into_best_move(self) -> Option<Uci> {
        self.positions.into_iter().next().and_then(|p| match p {
            Skip::Skip => None,
            Skip::Present(pos) => pos.best_move,
        })
    }

    fn total_positions(&self) -> u64 {
        self.positions
            .iter()
            .map(|p| match p {
                Skip::Skip => 0,
                Skip::Present(_) => 1,
            })
            .sum()
    }

    fn total_nodes(&self) -> u64 {
        self.positions
            .iter()
            .map(|p| match p {
                Skip::Skip => 0,
                Skip::Present(pos) => pos.nodes,
            })
            .sum()
    }

    fn nps(&self) -> Option<u32> {
        self.completed_at
            .checked_duration_since(self.started_at)
            .and_then(|time| (u128::from(self.total_nodes()) * 1000).checked_div(time.as_millis()))
            .and_then(|nps| nps.try_into().ok())
    }
}
