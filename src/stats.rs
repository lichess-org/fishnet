use std::{
    cmp::{max, min},
    fmt,
    fs::{File, OpenOptions},
    io,
    io::{Read as _, Seek as _, SeekFrom, Write as _},
    num::NonZeroUsize,
    path::PathBuf,
    time::Duration,
};

use serde::{Deserialize, Serialize};

const STATS_FILENAME: &str = ".fishnet-stats";

fn stats_path() -> io::Result<PathBuf> {
    home::home_dir()
        .map(|dir| dir.join(STATS_FILENAME))
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::Other,
                format!("Could not resolve ~/.{}", STATS_FILENAME),
            )
        })
}

pub struct StatsRecorder {
    pub stats: Stats,
    pub nnue_nps: NpsRecorder,
    stats_file: Option<File>,
}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct Stats {
    pub total_batches: u64,
    pub total_positions: u64,
    pub total_nodes: u64,
}

impl Stats {
    fn load_from(file: &mut File) -> io::Result<Option<Stats>> {
        file.seek(SeekFrom::Start(0))?;
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
        file.seek(SeekFrom::Start(0))?;
        file.write_all(
            serde_json::to_string_pretty(&self)
                .expect("serialize stats")
                .as_bytes(),
        )?;
        Ok(())
    }
}

impl StatsRecorder {
    pub fn open(cores: NonZeroUsize) -> StatsRecorder {
        let (stats, stats_file) = match stats_path().and_then(|path| {
            OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .open(path)
        }) {
            Ok(mut file) => (
                match Stats::load_from(&mut file) {
                    Ok(Some(stats)) => {
                        println!("Resuming from ~/{} ...", STATS_FILENAME);
                        stats
                    }
                    Ok(None) => {
                        println!("Recording to new stats file ~/{} ...", STATS_FILENAME);
                        Stats::default()
                    }
                    Err(err) => {
                        eprintln!(
                            "E: Failed to resume from ~/{}: {}. Resetting ...",
                            STATS_FILENAME, err
                        );
                        Stats::default()
                    }
                },
                Some(file),
            ),
            Err(err) => {
                eprintln!("E: Failed to open ~/{}: {}", STATS_FILENAME, err);
                (Stats::default(), None)
            }
        };

        StatsRecorder {
            stats,
            stats_file,
            nnue_nps: NpsRecorder::new(cores),
        }
    }

    pub fn record_batch(&mut self, positions: u64, nodes: u64, nnue_nps: Option<u32>) {
        self.stats.total_batches += 1;
        self.stats.total_positions += positions;
        self.stats.total_nodes += nodes;

        if let Some(nnue_nps) = nnue_nps {
            self.nnue_nps.record(nnue_nps);
        }

        if let Some(ref mut stats_file) = self.stats_file {
            if let Err(err) = self.stats.save_to(stats_file) {
                eprintln!("E: Failed to write stats to ~/{}: {}", STATS_FILENAME, err);
            }
        }
    }

    pub fn min_user_backlog(&self) -> Duration {
        // The average batch has 60 positions, analysed with 2_000_000 nodes
        // each. Top end clients take no longer than 35 seconds.
        let best_batch_seconds = 35;

        // Estimate how long this client would take for the next batch,
        // capped at timeout.
        let estimated_batch_seconds =
            u64::from(min(6 * 60, 60 * 2_000_000 / max(1, self.nnue_nps.nps)));

        // Its worth joining if queue wait time + estimated time < top client
        // time on empty queue.
        Duration::from_secs(estimated_batch_seconds.saturating_sub(best_batch_seconds))
    }
}

#[derive(Clone)]
pub struct NpsRecorder {
    pub nps: u32,
    pub uncertainty: f64,
}

impl NpsRecorder {
    fn new(cores: NonZeroUsize) -> NpsRecorder {
        NpsRecorder {
            nps: 400_000 * cores.get() as u32, // start with a low estimate
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
        write!(f, "{} knps", self.nps / 1000)?;
        if self.uncertainty > 0.7 {
            write!(f, "?")?;
        }
        if self.uncertainty > 0.4 {
            write!(f, "?")?;
        }
        if self.uncertainty > 0.1 {
            write!(f, "?")?;
        }
        Ok(())
    }
}
