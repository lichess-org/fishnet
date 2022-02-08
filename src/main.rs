#![forbid(unsafe_op_in_unsafe_fn)]

mod api;
mod assets;
mod configure;
mod ipc;
mod logger;
mod notation;
mod queue;
mod stats;
mod stockfish;
mod systemd;
mod util;

use std::{
    cmp::min,
    env,
    path::PathBuf,
    ptr,
    sync::Arc,
    thread,
    time::{Duration, Instant},
};

use atty::Stream;
use thousands::Separable as _;
use tokio::{
    signal,
    sync::{mpsc, oneshot},
    time,
};

use crate::{
    assets::{Assets, ByEngineFlavor, Cpu, EngineFlavor},
    configure::{Command, Cores, Opt},
    ipc::{Position, PositionFailed, Pull},
    logger::{Logger, ProgressAt},
    stockfish::StockfishInit,
    util::RandomizedBackoff,
};

static COMPRESSED_DEPENDENCY_LIST: &[u8] = auditable::inject_dependency_list!();

#[tokio::main(flavor = "current_thread")]
async fn main() {
    unsafe {
        // Work around https://github.com/Shnatsel/rust-audit/issues/9.
        // Safety: u8 is Copy.
        ptr::read_volatile(&COMPRESSED_DEPENDENCY_LIST[0]);
    }

    let opt = configure::parse_and_configure().await;
    let logger = Logger::new(opt.verbose, opt.command.map_or(false, Command::is_systemd));

    if opt.auto_update {
        let current_exe = env::current_exe().expect("current exe");
        match auto_update(
            !opt.command.map_or(false, Command::is_systemd),
            logger.clone(),
        )
        .await
        {
            Err(err) => logger.error(&format!("Failed to update: {}", err)),
            Ok(self_update::Status::UpToDate(version)) => {
                logger.fishnet_info(&format!("Fishnet {} is up to date", version));
            }
            Ok(self_update::Status::Updated(version)) => {
                logger.fishnet_info(&format!("Fishnet updated to {}", version));
                restart_process(current_exe, &logger);
            }
        }
    }

    match opt.command {
        Some(Command::Run) | None => run(opt, &logger).await,
        Some(Command::Systemd) => systemd::systemd_system(opt),
        Some(Command::SystemdUser) => systemd::systemd_user(opt),
        Some(Command::Configure) => (),
        Some(Command::License) => license(&logger),
    }
}

async fn run(opt: Opt, logger: &Logger) {
    logger.headline("Checking configuration ...");

    let endpoint = opt.endpoint();
    logger.info(&format!("Endpoint: {}", endpoint));

    logger.info(&format!(
        "Backlog: Join queue if user backlog >= {:?} or system backlog >= {:?}",
        Duration::from(opt.backlog.user.unwrap_or_default()),
        Duration::from(opt.backlog.system.unwrap_or_default())
    ));

    let cpu = Cpu::detect();
    logger.info(&format!("CPU features: {:?}", cpu));

    let assets = Assets::prepare(cpu).expect("prepared bundled stockfish");
    logger.info(&format!(
        "Engine: {} (for GPLv3, run: {} license)",
        assets.sf_name,
        env::args().next().unwrap_or_else(|| "./fishnet".to_owned())
    ));

    let cores = usize::from(opt.cores.unwrap_or(Cores::Auto));
    logger.info(&format!("Cores: {}", cores));

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
    let mut join_handles = Vec::new();

    // Spawn API actor.
    let api = {
        let (api, api_actor) = api::channel(endpoint.clone(), opt.key, logger.clone());
        join_handles.push(tokio::spawn(async move {
            api_actor.run().await;
        }));
        api
    };

    let to_stop = if atty::is(Stream::Stdout) {
        "CTRL-C"
    } else {
        "SIGINT"
    };
    logger.headline(&format!("Running ({} to stop) ...", to_stop));

    // Spawn queue actor.
    let mut queue = {
        let (queue, queue_actor) = queue::channel(
            opt.backlog,
            cores,
            api,
            opt.max_backoff.into(),
            logger.clone(),
        );
        join_handles.push(tokio::spawn(async move {
            queue_actor.run().await;
        }));
        queue
    };

    // Spawn workers. Workers handle engine processes and send their results
    // to tx, thereby requesting more work.
    let mut rx = {
        let assets = Arc::new(assets);
        let (tx, rx) = mpsc::channel::<Pull>(cores);
        for i in 0..cores {
            let assets = assets.clone();
            let tx = tx.clone();
            let logger = logger.clone();
            join_handles.push(tokio::spawn(async move {
                worker(i, assets, tx, logger).await;
            }));
        }
        rx
    };

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
            match auto_update(false, logger.clone()).await {
                Err(err) => logger.error(&format!("Failed to update in the background: {}", err)),
                Ok(self_update::Status::UpToDate(version)) => {
                    logger.fishnet_info(&format!("Fishnet {} is up to date", version));
                }
                Ok(self_update::Status::Updated(version)) => {
                    logger.fishnet_info(&format!(
                        "Fishnet updated to {}. Will restart soon",
                        version
                    ));
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
                "fishnet/{}: {} (nnue), {} batches, {} positions, {} total nodes",
                env!("CARGO_PKG_VERSION"),
                nnue_nps,
                stats.total_batches.separate_with_dots(),
                stats.total_positions.separate_with_dots(),
                stats.total_nodes.separate_with_dots()
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
                    logger.headline(&format!("Stopping soon. {} again to abort pending batches ...", to_stop));
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
            _ = time::sleep(Duration::from_secs(120)) => (),
        }
    }

    // Shutdown queue to abort remaining jobs.
    queue.shutdown().await;

    // Wait for all workers.
    for join_handle in join_handles.into_iter() {
        join_handle.await.expect("join");
    }

    // Restart.
    if let Some(restart) = restart.take() {
        restart_process(restart, logger);
    }
}

async fn worker(i: usize, assets: Arc<Assets>, tx: mpsc::Sender<Pull>, logger: Logger) {
    logger.debug(&format!("Started worker {}.", i));

    let mut job: Option<Position> = None;
    let mut engine = ByEngineFlavor {
        official: None,
        multi_variant: None,
    };
    let mut engine_backoff = RandomizedBackoff::default();

    let default_budget = Duration::from_secs(60);
    let mut budget = default_budget;

    loop {
        let response = if let Some(job) = job.take() {
            // Ensure engine process is ready.
            let flavor = job.flavor;
            let context = ProgressAt::from(&job);
            let (mut sf, join_handle) =
                if let Some((sf, join_handle)) = engine.get_mut(flavor).take() {
                    (sf, join_handle)
                } else {
                    // Backoff before starting engine.
                    let backoff = engine_backoff.next();
                    if backoff >= Duration::from_secs(5) {
                        logger.info(&format!(
                            "Waiting {:?} before attempting to start engine",
                            backoff
                        ));
                    } else {
                        logger.debug(&format!(
                            "Waiting {:?} before attempting to start engine",
                            backoff
                        ));
                    }
                    tokio::select! {
                        _ = tx.closed() => break,
                        _ = time::sleep(engine_backoff.next()) => (),
                    }

                    // Reset budget, start engine and spawn actor.
                    budget = default_budget;
                    let (sf, sf_actor) = stockfish::channel(
                        assets.stockfish.get(flavor).clone(),
                        StockfishInit {
                            nnue: assets.nnue.clone(),
                        },
                        logger.clone(),
                    );
                    let join_handle = tokio::spawn(async move {
                        sf_actor.run().await;
                    });
                    (sf, join_handle)
                };

            // Provide time budget.
            budget = min(default_budget, budget) + job.work.timeout();

            // Analyse or play.
            let timer = Instant::now();
            let batch_id = job.work.id();
            let res = tokio::select! {
                _ = tx.closed() => {
                    logger.debug(&format!("Worker {} shutting down engine early", i));
                    drop(sf);
                    join_handle.await.expect("join");
                    break;
                }
                res = sf.go(job) => {
                    match res {
                        Ok(res) => {
                            *engine.get_mut(flavor) = Some((sf, join_handle));
                            engine_backoff.reset();
                            Ok(res)
                        }
                        Err(failed) => {
                            drop(sf);
                            logger.warn(&format!("Worker {} waiting for engine to shut down after error. Context: {}", i, context));
                            join_handle.await.expect("join");
                            Err(failed)
                        },
                    }
                }
                _ = time::sleep(budget) => {
                    logger.warn(&match flavor {
                        EngineFlavor::Official => format!("Official Stockfish timed out in worker {}. If this happens frequently it is better to stop and defer to clients with better hardware. Context: {}", i, context),
                        EngineFlavor::MultiVariant => format!("Fairy-Stockfish timed out in worker {}. Context: {}", i, context),
                    });
                    drop(sf);
                    join_handle.await.expect("join");
                    Err(PositionFailed { batch_id })
                }
            };

            // Update time budget.
            budget = budget.checked_sub(timer.elapsed()).unwrap_or_default();
            if budget < default_budget {
                logger.debug(&format!("Low engine timeout budget: {:?}", budget));
            }

            Some(res)
        } else {
            None
        };

        let (callback, waiter) = oneshot::channel();

        if tx.send(Pull { response, callback }).await.is_err() {
            logger.debug(&format!(
                "Worker {} was about to send result, but shutting down",
                i
            ));
            break;
        }

        tokio::select! {
            _ = tx.closed() => break,
            res = waiter => {
                match res {
                    Ok(next_job) => job = Some(next_job),
                    Err(_) => break,
                }
            }
        }
    }

    if let Some((sf, join_handle)) = engine.get_mut(EngineFlavor::Official).take() {
        logger.debug(&format!(
            "Worker {} waiting for standard engine to shut down",
            i
        ));
        drop(sf);
        join_handle.await.expect("join");
    }

    if let Some((sf, join_handle)) = engine.get_mut(EngineFlavor::MultiVariant).take() {
        logger.debug(&format!(
            "Worker {} waiting for multi-variant engine to shut down",
            i
        ));
        drop(sf);
        join_handle.await.expect("join");
    }

    logger.debug(&format!("Stopped worker {}", i));
    drop(tx);
}

fn license(logger: &Logger) {
    logger.headline("LICENSE.txt");
    println!("{}", include_str!("../LICENSE.txt"));
    logger.headline("COPYING.txt");
    print!("{}", include_str!("../COPYING.txt"));
}

#[cfg(unix)]
fn restart_process(current_exe: PathBuf, logger: &Logger) {
    use std::os::unix::process::CommandExt as _;
    logger.headline(&format!(
        "Waiting 5s before restarting {:?} ...",
        current_exe
    ));
    thread::sleep(Duration::from_secs(5));
    let err = std::process::Command::new(current_exe)
        .args(std::env::args().into_iter().skip(1))
        .exec();
    panic!("Failed to restart: {}", err);
}

#[cfg(windows)]
fn restart_process(current_exe: PathBuf, logger: &Logger) {
    logger.headline(&format!(
        "Waiting 5s before restarting {:?} ...",
        current_exe
    ));
    thread::sleep(Duration::from_secs(5));
    std::process::Command::new(current_exe)
        .args(std::env::args().into_iter().skip(1))
        .spawn()
        .expect("restarted");
}

async fn auto_update(
    verbose: bool,
    logger: Logger,
) -> Result<self_update::Status, self_update::errors::Error> {
    tokio::task::spawn_blocking(move || {
        if verbose {
            logger.headline("Updating ...");
        }
        logger.fishnet_info("Checking for updates (--auto-update) ...");
        self_update::backends::github::Update::configure()
            .repo_owner("lichess-org")
            .repo_name("fishnet")
            .bin_name("fishnet")
            .show_output(verbose)
            .show_download_progress(atty::is(Stream::Stdout) && verbose)
            .current_version(env!("CARGO_PKG_VERSION"))
            .no_confirm(true)
            .build()
            .expect("self_update config")
            .update()
    })
    .await
    .expect("spawn blocking update")
}
