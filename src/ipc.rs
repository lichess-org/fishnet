use std::{num::NonZeroU8, time::Duration};

use shakmaty::{Color, Outcome, fen::Fen, uci::UciMove, variant::Variant};
use tokio::{sync::oneshot, time::Instant};
use url::Url;

use crate::{
    api::{AnalysisPart, BatchId, PositionIndex, Score, Work},
    assets::EngineFlavor,
    util::grow_with_and_get_mut,
};

#[derive(Debug)]
pub struct Chunk {
    pub work: Work,
    pub deadline: Instant,
    pub variant: Variant,
    pub flavor: EngineFlavor,
    pub positions: Vec<Position>,
}

impl Chunk {
    pub const MAX_POSITIONS: usize = 6;
}

#[derive(Debug, Clone)]
pub struct Position {
    pub work: Work,
    pub position_index: Option<PositionIndex>,
    pub url: Option<Url>,
    pub skip: bool,

    pub root_fen: Fen,
    pub moves: Vec<UciMove>,

    pub outcome: Outcome,
}

impl Position {
    pub fn turn(&self) -> Color {
        self.root_fen.as_setup().turn ^ (self.moves.len() % 2 == 1)
    }
}

#[derive(Debug, Clone)]
pub struct PositionResponse {
    pub work: Work,
    pub position_index: Option<PositionIndex>,
    pub url: Option<Url>,

    pub scores: Matrix<Score>,
    pub pvs: Matrix<Vec<UciMove>>,
    pub best_move: Option<UciMove>,
    pub depth: u8,
    pub nodes: u64,
    pub time: Duration,
    pub nps: Option<u32>,
}

impl PositionResponse {
    pub fn to_best(&self) -> AnalysisPart {
        AnalysisPart::Best {
            pv: self.pvs.best().cloned().unwrap_or_default(),
            score: self.scores.best().copied().expect("got score"),
            depth: self.depth,
            nodes: self.nodes,
            time: self.time.as_millis() as u64,
            nps: self.nps,
        }
    }

    pub fn into_matrix(self) -> AnalysisPart {
        AnalysisPart::Matrix {
            pv: self.pvs.matrix,
            score: self.scores.matrix,
            depth: self.depth,
            nodes: self.nodes,
            time: self.time.as_millis() as u64,
            nps: self.nps,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Matrix<T> {
    matrix: Vec<Vec<Option<T>>>,
}

impl<T> Matrix<T> {
    pub fn new() -> Matrix<T> {
        Matrix { matrix: Vec::new() }
    }

    pub fn set(&mut self, multipv: NonZeroU8, depth: u8, v: T) {
        let row = grow_with_and_get_mut(&mut self.matrix, usize::from(multipv.get() - 1), Vec::new);
        *grow_with_and_get_mut(row, usize::from(depth), || None) = Some(v);
    }

    pub fn best(&self) -> Option<&T> {
        self.matrix
            .first()
            .and_then(|row| row.last().and_then(|v| v.as_ref()))
    }
}

#[derive(Debug)]
pub struct ChunkFailed {
    pub batch_id: BatchId,
}

#[derive(Debug)]
pub struct Pull {
    pub responses: Result<Vec<PositionResponse>, ChunkFailed>,
    pub callback: oneshot::Sender<Chunk>,
}

impl Pull {
    pub fn split(
        self,
    ) -> (
        Result<Vec<PositionResponse>, ChunkFailed>,
        oneshot::Sender<Chunk>,
    ) {
        (self.responses, self.callback)
    }
}
