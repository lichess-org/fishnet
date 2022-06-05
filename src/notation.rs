use rsffish::{availablePieceChars, availableVariants, positionFromFen, validateFEN};
use shakmaty::{
    fen::Fen as ShakmatyFen,
    uci::{IllegalUciError, ParseUciError, Uci as ShakmatyUci},
    variant::{Variant as ShakmatyVariant, VariantPosition},
    CastlingMode, Position as _, PositionError,
};
use std::convert::From;
use std::{fmt, str::FromStr};

use crate::{api::LichessVariant, assets::EngineFlavor};

/// Errors that can occur when parsing a FEN.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum VariantError {
    InvalidVariant,
}

impl fmt::Display for VariantError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match *self {
            VariantError::InvalidVariant => "invalid variant",
        })
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Variant {
    Lichess(LichessVariant),
    FairyStockfish(String),
}

impl Variant {
    pub fn short_name(&self) -> Option<String> {
        match self {
            Variant::Lichess(lv) => lv.short_name().map(|x| x.to_string()),
            Variant::FairyStockfish(s) => Some(s.to_string()),
        }
    }

    pub fn uci(&self) -> String {
        match self {
            Variant::Lichess(lv) => {
                let variant: ShakmatyVariant = ShakmatyVariant::from(*lv);
                variant.uci().to_string()
            }
            Variant::FairyStockfish(s) => s.to_string(),
        }
    }
}

impl FromStr for Variant {
    type Err = VariantError;

    fn from_str(variant_name: &str) -> Result<Variant, VariantError> {
        match &variant_name.to_lowercase()[..] {
            "antichess" => Ok(Variant::Lichess(LichessVariant::Antichess)),
            "atomic" => Ok(Variant::Lichess(LichessVariant::Atomic)),
            "chess960" => Ok(Variant::Lichess(LichessVariant::Chess960)),
            "crazyhouse" => Ok(Variant::Lichess(LichessVariant::Crazyhouse)),
            "fromposition" => Ok(Variant::Lichess(LichessVariant::FromPosition)),
            "horde" => Ok(Variant::Lichess(LichessVariant::Horde)),
            "kingofthehill" => Ok(Variant::Lichess(LichessVariant::KingOfTheHill)),
            "racingkings" => Ok(Variant::Lichess(LichessVariant::RacingKings)),
            "threecheck" => Ok(Variant::Lichess(LichessVariant::ThreeCheck)),
            "standard" => Ok(Variant::Lichess(LichessVariant::Standard)),
            variant_name => match availableVariants().iter().find(|&v| v == variant_name) {
                Some(_) => Ok(Variant::FairyStockfish(variant_name.to_string())),
                None => Err(VariantError::InvalidVariant),
            },
        }
    }
}

impl Default for Variant {
    fn default() -> Variant {
        Variant::Lichess(LichessVariant::Standard)
    }
}

/// Errors that can occur when parsing a FEN.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum FenError {
    InvalidFen,
}

impl fmt::Display for FenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match *self {
            FenError::InvalidFen => "invalid fen",
        })
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Fen {
    Shakmaty(ShakmatyFen),
    FairyStockfish(String),
}

impl FromStr for Fen {
    type Err = FenError;

    fn from_str(fen: &str) -> Result<Fen, Self::Err> {
        ShakmatyFen::from_ascii(fen.as_bytes()).map_or_else(
            |_| {
                for v in availableVariants() {
                    if validateFEN(&v, &fen.to_string(), v == "chess960") {
                        return Ok(Fen::FairyStockfish(fen.to_string()));
                    }
                }
                Err(FenError::InvalidFen)
            },
            |f| Ok(Fen::Shakmaty(f)),
        )
    }
}

impl fmt::Display for Fen {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Fen::Shakmaty(fen) => f.write_str(&format!("{}", fen)),
            Fen::FairyStockfish(fen) => f.write_str(fen),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Uci {
    notation: String,
}

fn valid_role(c: u8) -> bool {
    // TODO: this assumes all pieces types are a single u8 byte. not sure that is true.
    availablePieceChars().as_bytes().iter().any(|n| n == &c)
}

fn valid_promotable_role(c: u8) -> bool {
    c == b'+' || availablePieceChars().as_bytes().iter().any(|n| n == &c)
}

fn valid_file(c: u8) -> bool {
    (b'a'..=b'j').contains(&c)
}

fn valid_rank(c: &[u8]) -> bool {
    (c.len() == 1 && (b'0'..=b'9').contains(&c[0]))
        || (c.len() == 2 && c[0] == b'1' && c[1] == b'0')
}

fn valid_square(c: &[u8]) -> bool {
    (!c.is_empty() && valid_file(c[0]))
        && ((c.len() == 2 && valid_rank(&c[1..2])) || (c.len() == 3 && valid_rank(&c[1..3])))
}

fn valid_square_pair(c: &[u8]) -> bool {
    match c.len() {
        4 => valid_square(&c[0..2]) && valid_square(&c[2..4]), // d8d9 | d8d9
        5 => {
            valid_square(&c[0..3]) && valid_square(&c[3..5])
                || valid_square(&c[0..2]) && valid_square(&c[2..5])
        } // d8d10 | d8d10 | d10d8
        6 => valid_square(&c[0..3]) && valid_square(&c[3..6]), // d10d11
        _ => false,
    }
}

impl Uci {
    pub fn null() -> Uci {
        Uci {
            notation: "0000".to_string(),
        }
    }

    pub fn from_ascii(uci: &[u8]) -> Result<Uci, UciParseError> {
        if uci.len() < 4 || uci.len() > 7 {
            return Err(UciParseError::InvalidUci);
        }

        if uci == b"0000" {
            return Ok(Uci::null());
        }

        let is_drop = valid_role(uci[0]) && uci[1] == b'@';
        let is_promotion = valid_promotable_role(uci[uci.len() - 1]);

        if match (is_drop, is_promotion, uci.len()) {
            // Drops
            (true, false, 4 | 5) => valid_role(uci[0]) && valid_square(&uci[2..uci.len()]), // P@b4
            // Promotions
            (false, true, 5 | 6 | 7) => valid_square_pair(&uci[0..uci.len() - 1]), // d8d9+ | d8d9R | d10d11+
            // moves
            (false, false, 4 | 5 | 6) => valid_square_pair(&uci[0..uci.len()]), // d8d9 | d9d11 | d10d11

            // Bleh
            _ => false,
        } {
            unsafe {
                Ok(Uci {
                    notation: String::from_utf8_unchecked(uci.to_vec()),
                })
            }
        } else {
            Err(UciParseError::InvalidUci)
        }
    }
}

impl fmt::Display for Uci {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.notation)
    }
}

/// Errors that can occur when parsing a FEN.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum UciParseError {
    InvalidUci,
}

impl fmt::Display for UciParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match *self {
            UciParseError::InvalidUci => "invalid uci",
        })
    }
}

impl FromStr for Uci {
    type Err = UciParseError;

    fn from_str(uci: &str) -> Result<Uci, UciParseError> {
        Uci::from_ascii(uci.as_bytes())
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum NormalizeError {
    InvalidMoves,
    InvalidArgs, // TODO: yes, this means we are using an API that is not fully typesafe
}

impl fmt::Display for NormalizeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match *self {
            NormalizeError::InvalidMoves => "invalid uci moves",
            NormalizeError::InvalidArgs => "invalid Args",
        })
    }
}

impl From<PositionError<VariantPosition>> for NormalizeError {
    fn from(_e: PositionError<VariantPosition>) -> Self {
        NormalizeError::InvalidMoves
    }
}

impl From<ParseUciError> for NormalizeError {
    fn from(_e: ParseUciError) -> Self {
        NormalizeError::InvalidMoves
    }
}

impl From<IllegalUciError> for NormalizeError {
    fn from(_e: IllegalUciError) -> Self {
        NormalizeError::InvalidMoves
    }
}

// TODO: figure out why only analysis is mapped ot official stockfish?
// TODO: are the chess960 booleans indicative of  chess960 castling?
pub fn normalize_moves(
    variant: Variant,
    fen: &Fen,
    moves: &[Uci],
) -> Result<(EngineFlavor, Vec<Uci>), NormalizeError> {
    match (fen, variant) {
        (Fen::Shakmaty(fen), Variant::Lichess(variant)) => {
            let maybe_root_pos = VariantPosition::from_setup(
                variant.into(),
                fen.clone().into_setup(),
                CastlingMode::Chess960,
            );

            let (flavor, root_pos) = match maybe_root_pos {
                Ok(pos @ VariantPosition::Chess(_)) => (EngineFlavor::Official, pos),
                Ok(pos) => (EngineFlavor::MultiVariant, pos),
                Err(pos) => (
                    EngineFlavor::MultiVariant,
                    pos.ignore_impossible_material()?,
                ),
            };

            let normalized_moves = {
                let mut new_moves: Vec<Uci> = Vec::with_capacity(moves.len());
                let mut pos = root_pos;
                for uci in moves {
                    let uci = ShakmatyUci::from_ascii(uci.notation.as_bytes())?;
                    let m = uci.to_move(&pos)?;
                    new_moves.push(Uci {
                        notation: m.to_uci(CastlingMode::Chess960).to_string(),
                    });
                    pos.play_unchecked(&m);
                }
                new_moves
            };
            Ok((flavor, normalized_moves))
        }
        (Fen::FairyStockfish(fen), Variant::FairyStockfish(variant_name)) => {
            let mut new_moves: Vec<String> = Vec::with_capacity(moves.len());
            let mut pos = positionFromFen(&variant_name, fen, false);
            for uci in moves {
                if pos.getLegalMoves().iter().any(|m| m == &uci.notation) {
                    new_moves.push(uci.notation.clone());
                    pos = pos.makeMoves(&vec![uci.notation.clone()]);
                } else {
                    return Err(NormalizeError::InvalidArgs);
                }
            }
            let mut _960_moves : Vec<Uci> = Vec::with_capacity(moves.len());
            for notation in rsffish::to960Uci(&variant_name, &new_moves.to_vec()) {
                _960_moves.push(Uci{notation});
            }
            Ok((EngineFlavor::MultiVariant, _960_moves.to_vec()))
        }
        (Fen::Shakmaty(fen), Variant::FairyStockfish(variant_name)) => normalize_moves(
            Variant::FairyStockfish(variant_name),
            &Fen::FairyStockfish(fen.to_string()),
            moves,
        ),
        _ => Err(NormalizeError::InvalidArgs),
    }
}
