use std::{
    cmp::{max, min},
    fmt,
    fs::{File, OpenOptions},
    io,
    io::{Read as _, Seek as _, Write as _},
    num::NonZeroUsize,
    path::PathBuf,
    time::Duration,
};

use serde::{Deserialize, Serialize};

use crate::configure::StatsOpt;

fn default_stats_file() -> Option<PathBuf> {
    home::home_dir().map(|dir| dir.join(".fishnet-stats"))
}

pub struct StatsRecorder {
    pub stats: Stats,
    pub official_nps: NpsRecorder,
    store: Option<(PathBuf, File)>,
    cores: NonZeroUsize,
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct Stats {
    pub total_batches: u64,
    pub total_positions: u64,
    pub total_nodes: u64,
}

impl Stats {
    fn load_from(file: &mut File) -> io::Result<Option<Stats>> {
        file.rewind()?;
        let mut buf = Vec::new();
        file.read_to_end(&mut buf)?;
        Ok(if buf.is_empty() {
            None
        } else {
            Some(
                serde_json::from_slice(&buf)
                    .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))?,
            )
        })
    }

    fn save_to(&self, file: &mut File) -> io::Result<()> {
        file.set_len(0)?;
        file.rewind()?;
        file.write_all(
            serde_json::to_string_pretty(&self)
                .expect("serialize stats")
                .as_bytes(),
        )?;
        Ok(())
    }
}

impl StatsRecorder {
    pub fn new(opt: StatsOpt, cores: NonZeroUsize) -> StatsRecorder {
        let official_nps = NpsRecorder::new();

        if opt.no_stats_file {
            return StatsRecorder {
                stats: Stats::default(),
                store: None,
                official_nps,
                cores,
            };
        }

        let path = if let Some(path) = opt.stats_file.or_else(default_stats_file) {
            path
        } else {
            eprintln!("E: Could not resolve ~/.fishnet-stats");
            return StatsRecorder {
                stats: Stats::default(),
                store: None,
                official_nps,
                cores,
            };
        };

        let (stats, store) = match OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&path)
        {
            Ok(mut file) => (
                match Stats::load_from(&mut file) {
                    Ok(Some(stats)) => {
                        println!("Resuming from {path:?} ...");
                        stats
                    }
                    Ok(None) => {
                        println!("Recording to new stats file {path:?} ...");
                        Stats::default()
                    }
                    Err(err) => {
                        eprintln!("E: Failed to resume from {path:?}: {err}. Resetting ...");
                        Stats::default()
                    }
                },
                Some((path, file)),
            ),
            Err(err) => {
                eprintln!("E: Failed to open {path:?}: {err}");
                (Stats::default(), None)
            }
        };

        StatsRecorder {
            stats,
            store,
            official_nps,
            cores,
        }
    }

    pub fn record_batch(&mut self, positions: u64, nodes: u64, official_nps: Option<u32>) {
        self.stats.total_batches += 1;
        self.stats.total_positions += positions;
        self.stats.total_nodes += nodes;

        if let Some(official_nps) = official_nps {
            self.official_nps.record(official_nps);
        }

        if let Some((ref path, ref mut stats_file)) = self.store {
            if let Err(err) = self.stats.save_to(stats_file) {
                eprintln!("E: Failed to write stats to {path:?}: {err}");
            }
        }
    }

    pub fn min_user_backlog(&self) -> Duration {
        // Estimate how long this client would take for the next batch of
        // 60 positions at 1_450_000 nodes each.
        let estimated_batch_seconds = u64::from(min(
            7 * 60, // deadline
            60 * 1_450_000 / self.cores.get() as u32 / max(1, self.official_nps.nps),
        ));

        // Top end clients take no longer than 35 seconds. Its worth joining if
        // estimated time < top client time on empty queue + queue wait time.
        let top_batch_seconds = 35;
        Duration::from_secs(estimated_batch_seconds.saturating_sub(top_batch_seconds))
    }
}

#[derive(Clone)]
pub struct NpsRecorder {
    pub nps: u32,
    pub uncertainty: f64,
}

impl NpsRecorder {
    fn new() -> NpsRecorder {
        NpsRecorder {
            nps: 400_000, // start with an optimistic estimate
            uncertainty: 1.0,
        }
    }

    fn record(&mut self, nps: u32) {
        let alpha = 0.9;
        self.uncertainty *= alpha;
        self.nps = (f64::from(self.nps) * alpha + f64::from(nps) * (1.0 - alpha)) as u32;
    }
}

impl fmt::Display for NpsRecorder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} knps/core", self.nps / 1000)?;
        if self.uncertainty > 0.1 {
            write!(f, " ?")?;
        }
        if self.uncertainty > 0.4 {
            write!(f, "?")?;
        }
        if self.uncertainty > 0.7 {
            write!(f, "?")?;
        }
        Ok(())
    }
}
