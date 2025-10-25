use std::{io, mem, num::NonZeroU8, path::PathBuf, process::Stdio, time::Duration};

use shakmaty::uci::UciMove;
use tokio::{
    io::{AsyncBufReadExt as _, AsyncWriteExt as _, BufReader, BufWriter, Lines},
    process::{ChildStdin, ChildStdout, Command},
    sync::{mpsc, oneshot},
};

use crate::{
    api::{Score, Work},
    assets::EngineFlavor,
    ipc::{Chunk, ChunkFailed, Matrix, Position, PositionResponse},
    logger::Logger,
    util::NevermindExt as _,
};

pub fn channel(
    exe: PathBuf,
    engine_flavor: EngineFlavor,
    logger: Logger,
) -> (StockfishStub, StockfishActor) {
    let (tx, rx) = mpsc::channel(1);
    (
        StockfishStub { tx },
        StockfishActor {
            rx,
            exe,
            engine_flavor,
            initialized: false,
            logger,
        },
    )
}

pub struct StockfishStub {
    tx: mpsc::Sender<StockfishMessage>,
}

impl StockfishStub {
    pub async fn go_multiple(
        &mut self,
        chunk: Chunk,
    ) -> Result<Vec<PositionResponse>, ChunkFailed> {
        let (callback, responses) = oneshot::channel();
        let batch_id = chunk.work.id();
        self.tx
            .send(StockfishMessage::GoMultiple { chunk, callback })
            .await
            .map_err(|_| ChunkFailed { batch_id })?;
        responses.await.map_err(|_| ChunkFailed { batch_id })
    }
}

pub struct StockfishActor {
    rx: mpsc::Receiver<StockfishMessage>,
    exe: PathBuf,
    engine_flavor: EngineFlavor,
    initialized: bool,
    logger: Logger,
}

#[derive(Debug)]
enum StockfishMessage {
    GoMultiple {
        chunk: Chunk,
        callback: oneshot::Sender<Vec<PositionResponse>>,
    },
}

struct Stdout {
    inner: Lines<BufReader<ChildStdout>>,
}

impl Stdout {
    fn new(inner: ChildStdout) -> Stdout {
        Stdout {
            inner: BufReader::new(inner).lines(),
        }
    }

    async fn read_line(&mut self) -> io::Result<String> {
        if let Some(line) = self.inner.next_line().await? {
            Ok(line)
        } else {
            Err(io::ErrorKind::UnexpectedEof.into())
        }
    }
}

struct Stdin {
    inner: BufWriter<ChildStdin>,
}

impl Stdin {
    fn new(inner: ChildStdin) -> Stdin {
        Stdin {
            inner: BufWriter::new(inner),
        }
    }

    async fn write_line(&mut self, line: &str) -> io::Result<()> {
        self.inner.write_all(line.as_bytes()).await?;
        self.inner.write_all(b"\n").await
    }

    async fn flush(&mut self) -> io::Result<()> {
        self.inner.flush().await
    }
}

#[derive(Debug)]
enum EngineError {
    IoError(io::Error),
    Shutdown,
}

impl From<io::Error> for EngineError {
    fn from(error: io::Error) -> EngineError {
        EngineError::IoError(error)
    }
}

fn new_process_group(command: &mut Command) -> &mut Command {
    #[cfg(unix)]
    {
        // Stop SIGINT from propagating to child process.
        command.process_group(0);
    }

    #[cfg(windows)]
    {
        // Stop CTRL+C from propagating to child process:
        // https://docs.microsoft.com/en-us/windows/win32/procthread/process-creation-flags
        let create_new_process_group = 0x0000_0200;
        command.creation_flags(create_new_process_group);
    }

    command
}

impl StockfishActor {
    pub async fn run(self) {
        let logger = self.logger.clone();
        if let Err(EngineError::IoError(err)) = self.run_inner().await {
            logger.error(&format!("Engine error: {err}"));
        }
    }

    async fn run_inner(mut self) -> Result<(), EngineError> {
        let mut child = new_process_group(&mut Command::new(&self.exe))
            .current_dir(self.exe.parent().expect("absolute path"))
            .stdout(Stdio::piped())
            .stdin(Stdio::piped())
            .kill_on_drop(true)
            .spawn()?;

        let pid = child.id().expect("pid");
        let mut stdout = Stdout::new(
            child
                .stdout
                .take()
                .ok_or_else(|| io::Error::new(io::ErrorKind::BrokenPipe, "stdout closed"))?,
        );
        let mut stdin = Stdin::new(
            child
                .stdin
                .take()
                .ok_or_else(|| io::Error::new(io::ErrorKind::BrokenPipe, "stdin closed"))?,
        );

        loop {
            tokio::select! {
                msg = self.rx.recv() => {
                    if let Some(msg) = msg {
                        self.handle_message(&mut stdout, &mut stdin, msg).await?;
                    } else {
                        break;
                    }
                }
                status = child.wait() => {
                    match status? {
                        status if status.success() => {
                            self.logger.debug(&format!("Stockfish process {pid} exited with status {status}"));
                        }
                        status => {
                            self.logger.error(&format!("Stockfish process {pid} exited with status {status}"));
                        }
                    }
                    break;
                }
            }
        }

        Ok(())
    }

    async fn handle_message(
        &mut self,
        stdout: &mut Stdout,
        stdin: &mut Stdin,
        msg: StockfishMessage,
    ) -> Result<(), EngineError> {
        match msg {
            StockfishMessage::GoMultiple {
                mut callback,
                chunk,
            } => {
                tokio::select! {
                    _ = callback.closed() => Err(EngineError::Shutdown),
                    res = self.go_multiple(stdout, stdin, chunk) => {
                        callback.send(res?).nevermind("go receiver dropped");
                        Ok(())
                    }
                }
            }
        }
    }

    async fn init(&mut self, stdout: &mut Stdout, stdin: &mut Stdin) -> io::Result<()> {
        if !mem::replace(&mut self.initialized, true) {
            if self.engine_flavor == EngineFlavor::MultiVariant {
                stdin
                    .write_line(&format!(
                        "setoption name EvalFile value {}",
                        env!("FISHNET_FAIRY_STOCKFISH_EVAL_FILES"),
                    ))
                    .await?;
            }
            stdin
                .write_line("setoption name UCI_Chess960 value true")
                .await?;
            stdin.write_line("isready").await?;
            stdin.flush().await?;

            loop {
                let line = stdout.read_line().await?;
                if line.trim_end() == "readyok" {
                    self.logger.debug("Engine is ready");
                    break;
                } else if !line.starts_with("Stockfish ") && !line.starts_with("Fairy-Stockfish ") {
                    // ignore preamble
                    self.logger.warn(&format!(
                        "Unexpected engine initialization output: {}",
                        line.trim_end()
                    ));
                }
            }
        }
        Ok(())
    }

    async fn go_multiple(
        &mut self,
        stdout: &mut Stdout,
        stdin: &mut Stdin,
        chunk: Chunk,
    ) -> io::Result<Vec<PositionResponse>> {
        // Set global options (once).
        self.init(stdout, stdin).await?;

        // Clear hash.
        stdin.write_line("ucinewgame").await?;

        // Set basic options.
        if chunk.flavor == EngineFlavor::MultiVariant {
            stdin
                .write_line(&format!(
                    "setoption name UCI_AnalyseMode value {}",
                    matches!(chunk.work, Work::Analysis { .. })
                ))
                .await?;
            stdin
                .write_line(&format!(
                    "setoption name UCI_Variant value {}",
                    chunk.variant.uci()
                ))
                .await?;
        }
        stdin
            .write_line(&format!(
                "setoption name MultiPV value {}",
                chunk.work.multipv()
            ))
            .await?;
        stdin
            .write_line(&format!(
                "setoption name Skill Level value {}",
                match chunk.work {
                    Work::Analysis { .. } => 20,
                    Work::Move { level, .. } => level.skill_level(),
                }
            ))
            .await?;

        // Collect results for all positions of the chunk.
        let mut responses = Vec::with_capacity(chunk.positions.len());
        for position in chunk.positions {
            responses.push(self.go(stdout, stdin, position).await?);
        }
        Ok(responses)
    }

    async fn go(
        &mut self,
        stdout: &mut Stdout,
        stdin: &mut Stdin,
        position: Position,
    ) -> io::Result<PositionResponse> {
        // Setup position.
        let moves = position
            .moves
            .iter()
            .map(|m| m.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        stdin
            .write_line(&format!(
                "position fen {} moves {}",
                position.root_fen, moves
            ))
            .await?;

        // Go.
        let go = match &position.work {
            Work::Move { level, clock, .. } => {
                let mut go = vec![
                    "go".to_owned(),
                    "movetime".to_owned(),
                    level.time().as_millis().to_string(),
                    "depth".to_owned(),
                    level.depth().to_string(),
                ];

                if let Some(clock) = clock {
                    go.extend_from_slice(&[
                        "wtime".to_owned(),
                        Duration::from(clock.wtime).as_millis().to_string(),
                        "btime".to_owned(),
                        Duration::from(clock.btime).as_millis().to_string(),
                        "winc".to_owned(),
                        clock.inc.as_millis().to_string(),
                        "binc".to_owned(),
                        clock.inc.as_millis().to_string(),
                    ]);
                }

                go
            }
            Work::Analysis { nodes, depth, .. } => {
                let mut go = vec!["go".to_owned(), "nodes".to_owned(), nodes.get().to_string()];

                if let Some(depth) = depth {
                    go.extend_from_slice(&["depth".to_owned(), depth.to_string()]);
                }

                go
            }
        };
        stdin.write_line(&go.join(" ")).await?;
        stdin.flush().await?;

        // Process response.
        let mut scores = Matrix::new();
        let mut pvs = Matrix::new();
        let mut depth = 0;
        let mut multipv = NonZeroU8::new(1).unwrap();
        let mut time = Duration::default();
        let mut nodes = 0;
        let mut nps = None;

        loop {
            let line = stdout.read_line().await?;
            let mut parts = line.split(' ');
            match parts.next() {
                Some("bestmove") => {
                    if scores.best().is_none() {
                        return Err(io::Error::new(io::ErrorKind::InvalidData, "missing score"));
                    }

                    return Ok(PositionResponse {
                        work: position.work,
                        position_index: position.position_index,
                        url: position.url,
                        best_move: parts.next().and_then(|m| m.parse().ok()),
                        scores,
                        depth,
                        pvs,
                        time,
                        nodes,
                        nps,
                    });
                }
                Some("info") => {
                    while let Some(part) = parts.next() {
                        match part {
                            "multipv" => {
                                multipv =
                                    parts.next().and_then(|t| t.parse().ok()).ok_or_else(|| {
                                        io::Error::new(
                                            io::ErrorKind::InvalidData,
                                            "expected multipv",
                                        )
                                    })?;
                            }
                            "depth" => {
                                depth =
                                    parts.next().and_then(|t| t.parse().ok()).ok_or_else(|| {
                                        io::Error::new(io::ErrorKind::InvalidData, "expected depth")
                                    })?;
                            }
                            "nodes" => {
                                nodes =
                                    parts.next().and_then(|t| t.parse().ok()).ok_or_else(|| {
                                        io::Error::new(io::ErrorKind::InvalidData, "expected nodes")
                                    })?;
                            }
                            "time" => {
                                time = parts
                                    .next()
                                    .and_then(|t| t.parse().ok())
                                    .map(Duration::from_millis)
                                    .ok_or_else(|| {
                                        io::Error::new(io::ErrorKind::InvalidData, "expected time")
                                    })?;
                            }
                            "nps" => {
                                nps = parts.next().and_then(|n| n.parse().ok());
                            }
                            "score" => {
                                scores.set(
                                    multipv,
                                    depth,
                                    match parts.next() {
                                        Some("cp") => parts
                                            .next()
                                            .and_then(|cp| cp.parse().ok())
                                            .map(Score::Cp),
                                        Some("mate") => parts
                                            .next()
                                            .and_then(|mate| mate.parse().ok())
                                            .map(Score::Mate),
                                        _ => {
                                            return Err(io::Error::new(
                                                io::ErrorKind::InvalidData,
                                                "expected cp or mate",
                                            ));
                                        }
                                    }
                                    .ok_or_else(|| {
                                        io::Error::new(io::ErrorKind::InvalidData, "expected score")
                                    })?,
                                );
                            }
                            "pv" => {
                                pvs.set(
                                    multipv,
                                    depth,
                                    (&mut parts)
                                        .map(|part| part.parse::<UciMove>())
                                        .collect::<Result<Vec<_>, _>>()
                                        .map_err(|_| {
                                            io::Error::new(io::ErrorKind::InvalidData, "invalid pv")
                                        })?,
                                );
                            }
                            _ => (),
                        }
                    }
                }
                _ => self
                    .logger
                    .warn(&format!("Unexpected engine output: {line}")),
            }
        }
    }
}
