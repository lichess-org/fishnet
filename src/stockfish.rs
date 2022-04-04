use std::{io, num::NonZeroU8, path::PathBuf, process::Stdio, time::Duration};

use tokio::{
    io::{AsyncBufReadExt as _, AsyncWriteExt as _, BufReader, BufWriter, Lines},
    process::{ChildStdin, ChildStdout, Command},
    sync::{mpsc, oneshot},
};

use crate::{
    api::{Score, Work},
    assets::EngineFlavor,
    ipc::{Matrix, Position, PositionFailed, PositionResponse},
    logger::Logger,
    util::NevermindExt as _,
};

pub fn channel(
    exe: PathBuf,
    init: StockfishInit,
    logger: Logger,
) -> (StockfishStub, StockfishActor) {
    let (tx, rx) = mpsc::channel(1);
    (
        StockfishStub { tx },
        StockfishActor {
            rx,
            exe,
            init: Some(init),
            logger,
        },
    )
}

pub struct StockfishStub {
    tx: mpsc::Sender<StockfishMessage>,
}

impl StockfishStub {
    pub async fn go(&mut self, position: Position) -> Result<PositionResponse, PositionFailed> {
        let (callback, response) = oneshot::channel();
        let batch_id = position.work.id();
        self.tx
            .send(StockfishMessage::Go { position, callback })
            .await
            .map_err(|_| PositionFailed { batch_id })?;
        response.await.map_err(|_| PositionFailed { batch_id })
    }
}

pub struct StockfishActor {
    rx: mpsc::Receiver<StockfishMessage>,
    exe: PathBuf,
    init: Option<StockfishInit>,
    logger: Logger,
}

#[derive(Debug)]
enum StockfishMessage {
    Go {
        position: Position,
        callback: oneshot::Sender<PositionResponse>,
    },
}

#[derive(Debug)]
pub struct StockfishInit {
    pub nnue: String,
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

#[cfg(unix)]
fn new_process_group(command: &mut Command) -> &mut Command {
    // Stop SIGINT from propagating to child process.
    unsafe {
        // Safety: The closure is run in a fork, and is not allowed to break
        // invariants by using raw handles.
        command.pre_exec(|| {
            if libc::setpgid(0, 0) == -1 {
                Err(io::Error::last_os_error())
            } else {
                Ok(())
            }
        })
    }
}

#[cfg(windows)]
fn new_process_group(command: &mut Command) -> &mut Command {
    // Stop CTRL+C from propagating to child process:
    // https://docs.microsoft.com/en-us/windows/win32/procthread/process-creation-flags
    let create_new_process_group = 0x00000200;
    command.creation_flags(create_new_process_group)
}

impl StockfishActor {
    pub async fn run(self) {
        let logger = self.logger.clone();
        if let Err(EngineError::IoError(err)) = self.run_inner().await {
            logger.error(&format!("Engine error: {}", err));
        }
    }

    async fn run_inner(mut self) -> Result<(), EngineError> {
        let mut child = new_process_group(
            Command::new(&self.exe)
                .stdout(Stdio::piped())
                .stdin(Stdio::piped())
                .kill_on_drop(true),
        )
        .spawn()?;

        let pid = child.id().expect("pid");
        let mut stdout = Stdout::new(
            child
                .stdout
                .take()
                .ok_or_else(|| io::Error::new(io::ErrorKind::BrokenPipe, "stdout closed"))?,
        );
        let mut stdin = BufWriter::new(
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
                            self.logger.debug(&format!("Stockfish process {} exited with status {}", pid, status));
                        }
                        status => {
                            self.logger.error(&format!("Stockfish process {} exited with status {}", pid, status));
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
        stdin: &mut BufWriter<ChildStdin>,
        msg: StockfishMessage,
    ) -> Result<(), EngineError> {
        match msg {
            StockfishMessage::Go {
                mut callback,
                position,
            } => {
                tokio::select! {
                    _ = callback.closed() => Err(EngineError::Shutdown),
                    res = self.go(stdout, stdin, position) => {
                        callback.send(res?).nevermind("go receiver dropped");
                        Ok(())
                    }
                }
            }
        }
    }

    async fn init(
        &mut self,
        stdout: &mut Stdout,
        stdin: &mut BufWriter<ChildStdin>,
        position: &Position,
    ) -> io::Result<()> {
        if let Some(init) = self.init.take() {
            stdin
                .write_all(format!("setoption name EvalFile value {}\n", init.nnue).as_bytes())
                .await?;
            stdin
                .write_all(b"setoption name UCI_Chess960 value true\n")
                .await?;
            if position.flavor == EngineFlavor::MultiVariant {
                if let Some(f) = position
                    .clone()
                    .variants_ini_file
                    .and_then(|v| v.to_str().map(|v| v.to_owned()))
                {
                    stdin
                        .write_all(format!("setoption name VariantPath value {}\n", f).as_bytes())
                        .await?;
                }
            }

            stdin.write_all(b"isready\n").await?;
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

    async fn go(
        &mut self,
        stdout: &mut Stdout,
        stdin: &mut BufWriter<ChildStdin>,
        position: Position,
    ) -> io::Result<PositionResponse> {
        // Set global options (once).
        self.init(stdout, stdin, &position).await?;

        // Clear hash.
        stdin.write_all(b"ucinewgame\n").await?;

        // Set basic options.
        stdin
            .write_all(
                format!(
                    "setoption name Use NNUE value {}\n",
                    position.flavor.eval_flavor().is_nnue()
                )
                .as_bytes(),
            )
            .await?;
        if position.flavor == EngineFlavor::MultiVariant {
            stdin
                .write_all(
                    format!(
                        "setoption name UCI_Variant value {}\n",
                        position.variant.uci()
                    )
                    .as_bytes(),
                )
                .await?;
        }
        stdin
            .write_all(
                format!("setoption name MultiPV value {}\n", position.work.multipv()).as_bytes(),
            )
            .await?;

        // Setup position.
        let moves = position
            .moves
            .iter()
            .map(|m| m.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        stdin
            .write_all(format!("position fen {} moves {}\n", position.root_fen, moves).as_bytes())
            .await?;

        // Go.
        let go = match &position.work {
            Work::Move { level, clock, .. } => {
                stdin
                    .write_all(b"setoption name UCI_AnalyseMode value false\n")
                    .await?;
                stdin
                    .write_all(
                        format!("setoption name Skill Level value {}\n", level.skill_level())
                            .as_bytes(),
                    )
                    .await?;

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
                stdin
                    .write_all(b"setoption name UCI_AnalyseMode value true\n")
                    .await?;
                stdin
                    .write_all(b"setoption name Skill Level value 20\n")
                    .await?;

                let mut go = vec![
                    "go".to_owned(),
                    "nodes".to_owned(),
                    nodes.get(position.flavor.eval_flavor()).to_string(),
                ];

                if let Some(depth) = depth {
                    go.extend_from_slice(&["depth".to_owned(), depth.to_string()]);
                }

                go
            }
        };
        stdin.write_all(go.join(" ").as_bytes()).await?;
        stdin.write_all(b"\n").await?;
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
                    let best_move = parts.next().and_then(|m| m.parse().ok());
                    self.logger.debug(&format!("Sending bestmove: {:?}", best_move));

                    return Ok(PositionResponse {
                        work: position.work,
                        position_id: position.position_id,
                        url: position.url,
                        best_move,
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
                                            ))
                                        }
                                    }
                                    .ok_or_else(|| {
                                        io::Error::new(io::ErrorKind::InvalidData, "expected score")
                                    })?,
                                );
                            }
                            "pv" => {
                                let mut pv = Vec::new();
                                for part in &mut parts {
                                    pv.push(part.parse().map_err(|_| {
                                        io::Error::new(io::ErrorKind::InvalidData, "invalid pv")
                                    })?);
                                }
                                pvs.set(multipv, depth, pv);
                            }
                            _ => (),
                        }
                    }
                }
                _ => self
                    .logger
                    .warn(&format!("Unexpected engine output: {}", line)),
            }
        }
    }
}
