use std::{
    cmp::{max, min},
    collections::{HashMap, VecDeque, hash_map::Entry},
    error::Error,
    fmt,
    iter::{once, zip},
    num::NonZeroUsize,
    sync::Arc,
    time::Duration,
};

use shakmaty::{
    CastlingMode, EnPassantMode, Position as _, PositionError,
    fen::Fen,
    uci::{IllegalUciMoveError, UciMove},
    variant::{Variant, VariantPosition},
};
use tokio::{
    sync::{Mutex, Notify, mpsc, oneshot},
    time::{Instant, sleep},
};
use url::Url;

use crate::{
    api::{
        AcquireQuery, AcquireResponseBody, Acquired, AnalysisPart, ApiStub, BatchId, PositionIndex,
        Work,
    },
    assets::{EngineFlavor, EvalFlavor},
    configure::{BacklogOpt, Endpoint, MaxBackoff, StatsOpt},
    ipc::{Chunk, ChunkFailed, Position, PositionResponse, Pull},
    logger::{Logger, ProgressAt, QueueStatusBar, short_variant_name},
    stats::{NpsRecorder, Stats, StatsRecorder},
    util::{NevermindExt as _, RandomizedBackoff, grow_with_and_get_mut},
};

pub fn channel(
    stats_opt: StatsOpt,
    backlog_opt: BacklogOpt,
    cores: NonZeroUsize,
    api: ApiStub,
    max_backoff: MaxBackoff,
    logger: Logger,
) -> (QueueStub, QueueActor) {
    let (tx, rx) = mpsc::unbounded_channel();
    let interrupt = Arc::new(Notify::new());
    let state = Arc::new(Mutex::new(QueueState::new(
        stats_opt,
        cores,
        logger.clone(),
    )));
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
        backlog_opt,
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
        let (responses, callback) = pull.split();
        state.handle_position_responses(self, responses);
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
    cores: NonZeroUsize,
    incoming: VecDeque<Chunk>,
    pending: HashMap<BatchId, PendingBatch>,
    move_submissions: VecDeque<MoveSubmission>,
    stats_recorder: StatsRecorder,
    logger: Logger,
}

impl QueueState {
    fn new(stats_opt: StatsOpt, cores: NonZeroUsize, logger: Logger) -> QueueState {
        QueueState {
            shutdown_soon: false,
            cores,
            incoming: VecDeque::new(),
            pending: HashMap::new(),
            move_submissions: VecDeque::new(),
            stats_recorder: StatsRecorder::new(stats_opt, cores),
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

                let mut positions = Vec::with_capacity(batch.chunks.len() * Chunk::MAX_POSITIONS);
                for chunk in batch.chunks {
                    for pos in &chunk.positions {
                        if let Some(position_index) = pos.position_index {
                            *grow_with_and_get_mut(&mut positions, position_index.0, || {
                                Some(Skip::Skip)
                            }) = pos.skip.then_some(Skip::Skip);
                        }
                    }
                    self.incoming.push_back(chunk);
                }

                entry.insert(PendingBatch {
                    work: batch.work,
                    flavor: batch.flavor,
                    variant: batch.variant,
                    url: batch.url,
                    positions,
                    total_nodes: 0,
                    total_cpu_time: Duration::ZERO,
                });

                self.logger.progress(self.status_bar(), progress_at);
            }
        }
    }

    fn handle_position_responses(
        &mut self,
        queue: &QueueStub,
        responses: Result<Vec<PositionResponse>, ChunkFailed>,
    ) {
        match responses {
            Ok(responses) => {
                let mut progress_at = None;
                let mut batch_ids = Vec::new();
                for res in responses {
                    let batch_id = res.work.id();
                    let Some(pending) = self.pending.get_mut(&batch_id) else {
                        continue;
                    };
                    pending.total_nodes += res.nodes;
                    pending.total_cpu_time += res.time;
                    let Some(position_index) = res.position_index else {
                        continue;
                    };
                    let Some(pos) = pending.positions.get_mut(position_index.0) else {
                        continue;
                    };
                    progress_at = Some(ProgressAt::from(&res));
                    *pos = Some(Skip::Present(res));
                    if !batch_ids.contains(&batch_id) {
                        batch_ids.push(batch_id);
                    }
                }
                if let Some(progress_at) = progress_at {
                    self.logger.progress(self.status_bar(), progress_at);
                }
                for batch_id in batch_ids {
                    self.maybe_finished(queue.clone(), batch_id);
                }
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

    fn try_pull(&mut self, callback: oneshot::Sender<Chunk>) -> Result<(), oneshot::Sender<Chunk>> {
        if let Some(chunk) = self.incoming.pop_front() {
            if let Err(err) = callback.send(chunk) {
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
                    extra.extend(short_variant_name(completed.variant).map(|n| n.to_owned()));
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
                                completed.total_nodes,
                                nnue_nps,
                            );
                            format!("{} knps/core", nps / 1000)
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
                            "{} batch {} finished ({})",
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
                        Work::Move { id, .. } => {
                            self.logger.debug(&log);
                            self.move_submissions.push_back(MoveSubmission {
                                batch_id: id,
                                best_move: completed.into_best_move(),
                            });
                            queue.move_submitted();
                        }
                    }
                }
                Err(pending) => {
                    if !pending.work.matrix_wanted() {
                        // Send partial analysis as progress report.
                        queue.api.submit_analysis(
                            pending.work.id(),
                            pending.flavor.eval_flavor(),
                            pending.progress_report(),
                        );
                    }

                    self.pending.insert(pending.work.id(), pending);
                }
            }
        }
    }
}

#[derive(Debug)]
struct MoveSubmission {
    batch_id: BatchId,
    best_move: Option<UciMove>,
}

#[derive(Debug)]
enum QueueMessage {
    Pull { callback: oneshot::Sender<Chunk> },
    MoveSubmitted,
}

pub struct QueueActor {
    rx: mpsc::UnboundedReceiver<QueueMessage>,
    interrupt: Arc<Notify>,
    state: Arc<Mutex<QueueState>>,
    api: ApiStub,
    backlog_opt: BacklogOpt,
    backoff: RandomizedBackoff,
    logger: Logger,
}

impl QueueActor {
    pub async fn run(self) {
        self.logger.debug("Queue actor started");
        self.run_inner().await;
    }

    pub async fn backlog_wait_time(&mut self) -> (Duration, AcquireQuery) {
        let min_user_backlog = {
            let state = self.state.lock().await;
            state.stats_recorder.min_user_backlog()
        };
        let user_backlog = max(
            min_user_backlog,
            self.backlog_opt
                .user
                .map(Duration::from)
                .unwrap_or_default(),
        );
        let system_backlog = self
            .backlog_opt
            .system
            .map(Duration::from)
            .unwrap_or_default();

        if user_backlog >= Duration::from_secs(1) || system_backlog >= Duration::from_secs(1) {
            if let Some(status) = self.api.status().await {
                let user_wait = user_backlog
                    .checked_sub(status.user.oldest)
                    .unwrap_or_default();
                let system_wait = system_backlog
                    .checked_sub(status.system.oldest)
                    .unwrap_or_default();
                let slow = user_wait >= system_wait + Duration::from_secs(1);
                self.logger.debug(&format!("User wait: {:?} due to {:?} for oldest {:?}, system wait: {:?} due to {:?} for oldest {:?} -> {}",
                       user_wait, user_backlog, status.user.oldest,
                       system_wait, system_backlog, status.system.oldest, if slow { "system" } else { "user" }));
                (min(user_wait, system_wait), AcquireQuery { slow })
            } else {
                self.logger
                    .debug("Queue status not available. Will not delay acquire.");
                let slow = user_backlog >= system_backlog + Duration::from_secs(1);
                (Duration::ZERO, AcquireQuery { slow })
            }
        } else {
            (Duration::ZERO, AcquireQuery { slow: false })
        }
    }

    async fn handle_acquired_response_body(&mut self, body: AcquireResponseBody) {
        let batch_id = body.work.id();
        let context = ProgressAt {
            batch_id,
            batch_url: body.batch_url(self.api.endpoint()),
            position_index: None,
        };
        let is_move = body.work.is_move();

        match IncomingBatch::from_acquired(self.api.endpoint(), body) {
            Ok(incoming) => {
                let mut state = self.state.lock().await;
                state.add_incoming_batch(incoming);
            }
            Err(IncomingError::AllSkipped(completed)) => {
                self.logger
                    .debug(&format!("Completed empty batch {context}."));
                self.api.submit_analysis(
                    completed.work.id(),
                    completed.flavor.eval_flavor(),
                    completed.into_analysis(),
                );
            }
            Err(err) if is_move => {
                self.logger
                    .warn(&format!("Invalid move request {context}: {err}"));
                let mut state = self.state.lock().await;
                state.move_submissions.push_back(MoveSubmission {
                    batch_id,
                    best_move: None,
                });
            }
            Err(err) => {
                self.logger
                    .warn(&format!("Ignoring invalid batch {context}: {err}"));
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
                    .submit_move_and_acquire(completed.batch_id, completed.best_move)
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
                            self.logger.info(&format!("Going idle for {wait:?}."));
                        } else {
                            self.logger.debug(&format!("Going idle for {wait:?}."));
                        }

                        tokio::select! {
                            _ = callback.closed() => break,
                            _ = self.interrupt.notified() => continue,
                            _ = sleep(wait) => continue,
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
                                .debug(&format!("No job received. Backing off {backoff:?}."));
                            tokio::select! {
                                _ = callback.closed() => break,
                                _ = self.interrupt.notified() => (),
                                _ = sleep(backoff) => (),
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

#[derive(Debug)]
pub struct IncomingBatch {
    work: Work,
    flavor: EngineFlavor,
    variant: Variant,
    chunks: Vec<Chunk>,
    url: Option<Url>,
}

impl IncomingBatch {
    #[allow(clippy::result_large_err)]
    fn from_acquired(
        endpoint: &Endpoint,
        body: AcquireResponseBody,
    ) -> Result<IncomingBatch, IncomingError> {
        let url = body.batch_url(endpoint);

        let maybe_root_pos = VariantPosition::from_setup(
            body.variant,
            body.position.into_setup(),
            CastlingMode::Chess960,
        )
        .or_else(PositionError::ignore_invalid_ep_square)
        .or_else(PositionError::ignore_invalid_castling_rights);

        let (flavor, root_pos) = match maybe_root_pos {
            Ok(pos @ VariantPosition::Chess(_)) if body.work.is_analysis() => {
                (EngineFlavor::Official, pos)
            }
            Ok(pos) => (EngineFlavor::MultiVariant, pos),
            Err(pos) => (EngineFlavor::MultiVariant, pos.ignore_too_much_material()?),
        };

        let root_fen = Fen::from_position(&root_pos, EnPassantMode::Legal);

        let body_moves = {
            let mut moves = Vec::with_capacity(body.moves.len());
            let mut pos = root_pos;
            for uci in body.moves {
                let m = uci.to_move(&pos)?;
                moves.push(m.to_uci(CastlingMode::Chess960));
                pos.play_unchecked(m);
            }
            moves
        };

        Ok(IncomingBatch {
            work: body.work.clone(),
            url: url.clone(),
            flavor,
            variant: body.variant,
            chunks: match body.work {
                Work::Move { .. } => {
                    vec![Chunk {
                        work: body.work.clone(),
                        deadline: Instant::now() + body.work.timeout_per_ply(),
                        flavor,
                        variant: body.variant,
                        positions: vec![Position {
                            work: body.work,
                            url,
                            skip: false,
                            position_index: Some(PositionIndex(0)),
                            root_fen,
                            moves: body_moves,
                        }],
                    }]
                }
                Work::Analysis { .. } => {
                    // Iterate forwards to prepare positions.
                    let mut moves = Vec::new();
                    let num_positions = body_moves.len() + 1;
                    let deadline =
                        Instant::now() + body.work.timeout_per_ply() * num_positions as u32;
                    let mut positions = Vec::with_capacity(num_positions);
                    positions.push(Position {
                        work: body.work.clone(),
                        url: url.clone().map(|mut url| {
                            url.set_fragment(Some("0"));
                            url
                        }),
                        skip: body.skip_positions.contains(&PositionIndex(0)),
                        position_index: Some(PositionIndex(0)),
                        root_fen: root_fen.clone(),
                        moves: moves.clone(),
                    });
                    for (i, m) in body_moves.into_iter().enumerate() {
                        let position_index = PositionIndex(i + 1);
                        moves.push(m);
                        positions.push(Position {
                            work: body.work.clone(),
                            url: url.clone().map(|mut url| {
                                url.set_fragment(Some(&position_index.0.to_string()));
                                url
                            }),
                            skip: body.skip_positions.contains(&position_index),
                            position_index: Some(position_index),
                            root_fen: root_fen.clone(),
                            moves: moves.clone(),
                        });
                    }

                    // Reverse for backwards analysis.
                    positions.reverse();

                    // Prepare dummy positions, so the respective previous
                    // position is available when creating chunks.
                    let prev_and_current: Vec<(Option<Position>, Position)> = zip(
                        once(positions.first().cloned().map(|pos| {
                            // Repeat the last position, to offset that it
                            // is analyzed with completely fresh hash.
                            Position {
                                position_index: None,
                                ..pos
                            }
                        }))
                        .chain(positions.clone().into_iter().map(
                            |pos| {
                                Some(Position {
                                    position_index: None,
                                    ..pos
                                })
                            },
                        )),
                        positions,
                    )
                    .collect();

                    // Create chunks with overlap.
                    let mut chunks = Vec::new();
                    for prev_and_current_chunked in
                        prev_and_current.chunks(Chunk::MAX_POSITIONS - 1)
                    {
                        let mut chunk_positions = Vec::with_capacity(Chunk::MAX_POSITIONS);
                        for (prev, current) in prev_and_current_chunked {
                            if !current.skip {
                                if let Some(prev) = prev {
                                    if prev.skip || chunk_positions.is_empty() {
                                        chunk_positions.push(prev.clone());
                                    }
                                }
                                chunk_positions.push(current.clone());
                            }
                        }
                        if !chunk_positions.is_empty() {
                            chunks.push(Chunk {
                                work: body.work.clone(),
                                deadline,
                                flavor,
                                variant: body.variant,
                                positions: chunk_positions,
                            });
                        }
                    }

                    // Edge case: Batch is immediately completed, because all
                    // positions are skipped.
                    if chunks.is_empty() {
                        return Err(IncomingError::AllSkipped(CompletedBatch {
                            work: body.work,
                            url,
                            flavor,
                            variant: body.variant,
                            positions: vec![Skip::Skip; num_positions],
                            total_nodes: 0,
                            total_cpu_time: Duration::ZERO,
                        }));
                    }

                    chunks
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
            position_index: None,
        }
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
enum IncomingError {
    Position(PositionError<VariantPosition>),
    IllegalUciMove(IllegalUciMoveError),
    AllSkipped(CompletedBatch),
}

impl Error for IncomingError {}

impl fmt::Display for IncomingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IncomingError::Position(err) => err.fmt(f),
            IncomingError::IllegalUciMove(err) => err.fmt(f),
            IncomingError::AllSkipped(_) => f.write_str("all positions skipped"),
        }
    }
}

impl From<PositionError<VariantPosition>> for IncomingError {
    fn from(err: PositionError<VariantPosition>) -> IncomingError {
        IncomingError::Position(err)
    }
}

impl From<IllegalUciMoveError> for IncomingError {
    fn from(err: IllegalUciMoveError) -> IncomingError {
        IncomingError::IllegalUciMove(err)
    }
}

#[derive(Debug, Clone)]
struct PendingBatch {
    work: Work,
    url: Option<Url>,
    flavor: EngineFlavor,
    variant: Variant,
    positions: Vec<Option<Skip<PositionResponse>>>,
    total_nodes: u64,
    total_cpu_time: Duration,
}

impl PendingBatch {
    #[allow(clippy::result_large_err)]
    fn try_into_completed(self) -> Result<CompletedBatch, PendingBatch> {
        match self.positions.clone().into_iter().collect() {
            Some(positions) => Ok(CompletedBatch {
                work: self.work,
                url: self.url,
                flavor: self.flavor,
                variant: self.variant,
                positions,
                total_nodes: self.total_nodes,
                total_cpu_time: self.total_cpu_time,
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
    total_nodes: u64,
    total_cpu_time: Duration,
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

    fn into_best_move(self) -> Option<UciMove> {
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

    fn nps(&self) -> Option<u32> {
        (u128::from(self.total_nodes) * 1000)
            .checked_div(self.total_cpu_time.as_millis())
            .and_then(|nps| nps.try_into().ok())
    }
}
