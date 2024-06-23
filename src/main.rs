#![forbid(unsafe_code)]

mod api;
mod assets;
mod configure;
mod ipc;
mod logger;
mod queue;
mod stats;
mod stockfish;
mod systemd;
mod update;
mod util;

use std::{
    env, io,
    io::IsTerminal as _,
    path::PathBuf,
    process,
    sync::Arc,
    thread,
    time::{Duration, Instant},
};

use reqwest::Client;
use shell_escape::escape;
use thread_priority::{set_current_thread_priority, ThreadPriority};
use tokio::{
    signal,
    sync::{mpsc, oneshot},
    task::JoinSet,
    time::{sleep, sleep_until},
};

use crate::{
    assets::{Assets, ByEngineFlavor, Cpu, EngineFlavor},
    configure::{Command, Cores, CpuPriority, Opt},
    ipc::{Chunk, ChunkFailed, Pull},
    logger::{Logger, ProgressAt},
    update::{auto_update, UpdateSuccess},
    util::{dot_thousands, RandomizedBackoff},
};

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let client = configure_client();
    let opt = configure::parse_and_configure(&client).await;
    let logger = Logger::new(opt.verbose, opt.command.map_or(false, Command::is_systemd));

    if opt.auto_update {
        let current_exe = env::current_exe().expect("current exe");
        match auto_update(
            !opt.command.map_or(false, Command::is_systemd),
            &client,
            &logger,
        )
        .await
        {
            Err(err) => logger.error(&format!("Failed to update: {err}")),
            Ok(UpdateSuccess::UpToDate(version)) => {
                logger.fishnet_info(&format!("Fishnet v{version} is up to date"));
            }
            Ok(UpdateSuccess::Updated(version)) => {
                logger.fishnet_info(&format!("Fishnet updated to v{version}"));
                restart_process(current_exe, &logger);
            }
        }
    }

    match opt.command {
        Some(Command::Run) | None => run(opt, &client, &logger).await,
        Some(Command::Systemd) => systemd::systemd_system(opt),
        Some(Command::SystemdUser) => systemd::systemd_user(opt),
        Some(Command::Configure) => (),
        Some(Command::License) => license(&logger),
    }
}

async fn run(opt: Opt, client: &Client, logger: &Logger) {
    logger.headline("Checking configuration ...");

    let endpoint = opt.endpoint();
    logger.info(&format!("Endpoint: {endpoint}"));

    logger.info(&format!(
        "Backlog: Join queue if user backlog >= {:?} or system backlog >= {:?}",
        Duration::from(opt.backlog.user.unwrap_or_default()),
        Duration::from(opt.backlog.system.unwrap_or_default())
    ));

    let cpu = Cpu::detect();
    logger.info(&format!("CPU features: {cpu}"));

    let assets = Assets::prepare(cpu).expect("prepared bundled stockfish");
    logger.info(&format!(
        "Engine: {} (for GPLv3, run: {} license)",
        assets.sf_name,
        escape(
            env::args_os()
                .next()
                .and_then(|exe| exe.into_string().ok())
                .unwrap_or("./fishnet".to_owned())
                .into()
        )
    ));

    let cores = opt.cores.unwrap_or(Cores::Auto).number();
    logger.info(&format!("Cores: {cores}"));

    // Install handler for SIGTERM.
    #[cfg(unix)]
    let mut sig_term = signal::unix::signal(signal::unix::SignalKind::terminate())
        .expect("install handler for sigterm");
    #[cfg(windows)]
    let mut sig_term = signal::windows::ctrl_break().expect("install handler for ctrl+break");

    // Install handler for SIGINT.
    #[cfg(unix)]
    let mut sig_int = signal::unix::signal(signal::unix::SignalKind::interrupt())
        .expect("install handler for sigint");
    #[cfg(windows)]
    let mut sig_int = signal::windows::ctrl_c().expect("install handler for ctrl+c");

    // To wait for workers and API actor before shutdown.
    let mut join_set = JoinSet::new();

    // Spawn API actor.
    let (api, api_actor) = api::channel(endpoint.clone(), opt.key, client.clone(), logger.clone());
    join_set.spawn(api_actor.run());

    let to_stop = if io::stdout().is_terminal() {
        "CTRL-C"
    } else {
        "SIGINT"
    };
    logger.headline(&format!("Running ({to_stop} to stop) ..."));

    // Spawn queue actor.
    let (mut queue, queue_actor) = queue::channel(
        opt.stats,
        opt.backlog,
        cores,
        api,
        opt.max_backoff.unwrap_or_default(),
        logger.clone(),
    );
    join_set.spawn(queue_actor.run());

    // Spawn workers. Workers handle engine processes and send their results
    // to tx, thereby requesting more work.
    let mut rx = {
        let assets = Arc::new(assets);
        let (tx, rx) = mpsc::channel::<Pull>(cores.get());
        for i in 0..cores.get() {
            let assets = assets.clone();
            let tx = tx.clone();
            let logger = logger.clone();
            join_set.spawn(worker(i, assets, tx, logger));
        }
        rx
    };

    // Set scheduling priority.
    match opt.cpu_priority.unwrap_or_default() {
        CpuPriority::Unchanged => (),
        CpuPriority::Min => {
            if let Err(err) = set_current_thread_priority(ThreadPriority::Min) {
                logger.warn(&format!("Failed to decrease CPU priority: {err:?}"));
            }
        }
    }

    let mut restart = None;
    let mut up_to_date = Instant::now();
    let mut summarized = Instant::now();
    let mut shutdown_soon = false;

    loop {
        // Check for updates from time to time.
        let now = Instant::now();
        if opt.auto_update
            && !shutdown_soon
            && now.duration_since(up_to_date) >= Duration::from_secs(60 * 60 * 5)
        {
            up_to_date = now;
            let current_exe = env::current_exe().expect("current exe");
            match auto_update(false, client, logger).await {
                Err(err) => logger.error(&format!("Failed to update in the background: {err}")),
                Ok(UpdateSuccess::UpToDate(version)) => {
                    logger.fishnet_info(&format!("Fishnet v{version} is up to date"));
                }
                Ok(UpdateSuccess::Updated(version)) => {
                    logger
                        .fishnet_info(&format!("Fishnet updated to v{version}. Will restart soon"));
                    restart = Some(current_exe);
                    shutdown_soon = true;
                    queue.shutdown_soon().await;
                }
            }
        }

        // Print summary from time to time.
        if now.duration_since(summarized) >= Duration::from_secs(120) {
            summarized = now;
            let (stats, nnue_nps) = queue.stats().await;
            logger.fishnet_info(&format!(
                "v{}: {} (nnue), {} batches, {} positions, {} total nodes",
                env!("CARGO_PKG_VERSION"),
                nnue_nps,
                dot_thousands(stats.total_batches),
                dot_thousands(stats.total_positions),
                dot_thousands(stats.total_nodes),
            ));
        }

        // Main loop. Handles signals, forwards worker results from rx to the
        // queue and responds with more work.
        tokio::select! {
            res = sig_int.recv() => {
                res.expect("sigint handler installed");
                logger.clear_echo();
                if shutdown_soon {
                    logger.fishnet_info("Stopping now.");
                    rx.close();
                } else {
                    logger.headline(&format!("Stopping soon. {to_stop} again to abort pending batches ..."));
                    queue.shutdown_soon().await;
                    shutdown_soon = true;
                }
            }
            res = sig_term.recv() => {
                res.expect("sigterm handler installed");
                logger.fishnet_info("Stopping now.");
                shutdown_soon = true;
                rx.close();
            }
            res = rx.recv() => {
                if let Some(res) = res {
                    queue.pull(res).await;
                } else {
                    logger.debug("About to exit.");
                    break;
                }
            }
            _ = sleep(Duration::from_secs(120)) => (),
        }
    }

    // Shutdown queue to abort remaining chunks.
    queue.shutdown().await;

    // Wait for all workers.
    while let Some(res) = join_set.join_next().await {
        res.expect("join");
    }

    // Restart.
    if let Some(restart) = restart.take() {
        restart_process(restart, logger);
    }
}

async fn worker(i: usize, assets: Arc<Assets>, tx: mpsc::Sender<Pull>, logger: Logger) {
    logger.debug(&format!("Started worker {i}."));

    let mut chunk: Option<Chunk> = None;
    let mut engine = ByEngineFlavor {
        official: None,
        multi_variant: None,
    };
    let mut engine_backoff = RandomizedBackoff::default();

    loop {
        let responses = if let Some(chunk) = chunk.take() {
            // Ensure engine process is ready.
            let flavor = chunk.flavor;
            let context = ProgressAt::from(&chunk);
            let (mut sf, join_handle) =
                if let Some((sf, join_handle)) = engine.get_mut(flavor).take() {
                    (sf, join_handle)
                } else {
                    // Backoff before starting engine.
                    let backoff = engine_backoff.next();
                    if backoff >= Duration::from_secs(5) {
                        logger.info(&format!(
                            "Waiting {backoff:?} before attempting to start engine"
                        ));
                    } else {
                        logger.debug(&format!(
                            "Waiting {backoff:?} before attempting to start engine"
                        ));
                    }
                    tokio::select! {
                        _ = tx.closed() => break,
                        _ = sleep(engine_backoff.next()) => (),
                    }

                    // Start engine and spawn actor.
                    let (sf, sf_actor) =
                        stockfish::channel(assets.stockfish.get(flavor).clone(), logger.clone());
                    let join_handle = tokio::spawn(sf_actor.run());
                    (sf, join_handle)
                };

            // Analyse or play.
            let batch_id = chunk.work.id();
            let res = tokio::select! {
                _ = tx.closed() => {
                    logger.debug(&format!("Worker {i} shutting down engine early"));
                    drop(sf);
                    join_handle.await.expect("join");
                    break;
                }
                _ = sleep_until(chunk.deadline) => {
                    logger.warn(&match flavor {
                        EngineFlavor::Official => format!("Official Stockfish timed out in worker {i}. If this happens frequently it is better to stop and defer to clients with better hardware. Context: {context}"),
                        EngineFlavor::MultiVariant => format!("Fairy-Stockfish timed out in worker {i}. Context: {context}"),
                    });
                    drop(sf);
                    join_handle.await.expect("join");
                    Err(ChunkFailed { batch_id })
                }
                res = sf.go_multiple(chunk) => {
                    match res {
                        Ok(res) => {
                            *engine.get_mut(flavor) = Some((sf, join_handle));
                            engine_backoff.reset();
                            Ok(res)
                        }
                        Err(failed) => {
                            drop(sf);
                            logger.warn(&format!("Worker {i} waiting for engine to shut down after error. Context: {context}"));
                            join_handle.await.expect("join");
                            Err(failed)
                        },
                    }
                }
            };

            res
        } else {
            Ok(Vec::new())
        };

        let (callback, waiter) = oneshot::channel();

        if tx
            .send(Pull {
                responses,
                callback,
            })
            .await
            .is_err()
        {
            logger.debug(&format!(
                "Worker {i} was about to send result, but shutting down"
            ));
            break;
        }

        tokio::select! {
            _ = tx.closed() => break,
            res = waiter => {
                match res {
                    Ok(next_chunk) => chunk = Some(next_chunk),
                    Err(_) => break,
                }
            }
        }
    }

    if let Some((sf, join_handle)) = engine.get_mut(EngineFlavor::Official).take() {
        logger.debug(&format!(
            "Worker {i} waiting for standard engine to shut down"
        ));
        drop(sf);
        join_handle.await.expect("join");
    }

    if let Some((sf, join_handle)) = engine.get_mut(EngineFlavor::MultiVariant).take() {
        logger.debug(&format!(
            "Worker {i} waiting for multi-variant engine to shut down"
        ));
        drop(sf);
        join_handle.await.expect("join");
    }

    logger.debug(&format!("Stopped worker {i}"));
    drop(tx);
}

fn license(logger: &Logger) {
    logger.headline("LICENSE.txt");
    println!("{}", include_str!("../LICENSE.txt"));
    logger.headline("COPYING.txt");
    print!("{}", include_str!("../COPYING.txt"));
}

fn restart_process(current_exe: PathBuf, logger: &Logger) {
    logger.headline(&format!("Waiting 5s before restarting {current_exe:?} ..."));
    thread::sleep(Duration::from_secs(5));
    let err = exec(process::Command::new(current_exe).args(std::env::args_os().skip(1)));
    panic!("Failed to restart: {err}");
}

#[cfg(unix)]
fn exec(command: &mut process::Command) -> io::Error {
    use std::os::unix::process::CommandExt as _;
    // Completely replace the current process image. If successful, execution
    // of the current process stops here.
    command.exec()
}

#[cfg(windows)]
fn exec(command: &mut process::Command) -> io::Error {
    use std::os::windows::process::CommandExt as _;
    // No equivalent for Unix exec() exists. So create a new independent
    // console instead and terminate the current one:
    // https://docs.microsoft.com/en-us/windows/win32/procthread/process-creation-flags
    let create_new_console = 0x0000_0010;
    match command.creation_flags(create_new_console).spawn() {
        Ok(_) => process::exit(0),
        Err(err) => return err,
    }
}

fn configure_client() -> Client {
    // Build TLS backend that supports SSLKEYLOGFILE.
    let mut tls = rustls::ClientConfig::builder_with_provider(Arc::new(
        rustls::crypto::ring::default_provider(),
    ))
    .with_safe_default_protocol_versions()
    .expect("default tls versions supported")
    .with_root_certificates(rustls::RootCertStore {
        roots: webpki_roots::TLS_SERVER_ROOTS.to_vec(),
    })
    .with_no_client_auth();

    tls.alpn_protocols = vec!["h2".into(), "http/1.1".into()];
    tls.key_log = Arc::new(rustls::KeyLogFile::new());

    // Configure client.
    Client::builder()
        .user_agent(format!(
            "{}-{}-{}/{}",
            env!("CARGO_PKG_NAME"),
            env::consts::OS,
            env::consts::ARCH,
            env!("CARGO_PKG_VERSION")
        ))
        .timeout(Duration::from_secs(30))
        .pool_idle_timeout(Duration::from_secs(25))
        .use_preconfigured_tls(tls)
        .build()
        .expect("client")
}
