use std::{
    fmt,
    fs::File,
    io,
    path::{Path, PathBuf},
    str,
};

use ar::Archive;
use bitflags::bitflags;
use tempfile::TempDir;
use zstd::stream::read::Decoder as ZstdDecoder;

static ASSETS_AR_ZST: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/assets.ar.zst"));

bitflags! {
    #[derive(Debug, Copy, Clone, Eq, PartialEq)]
    pub struct Cpu: u32 {
        // x86_64
        const SSE2      = 1 << 0;
        const POPCNT    = 1 << 1;
        const SSE41     = 1 << 2;
        const AVX2      = 1 << 3;
        const FAST_BMI2 = 1 << 4;
        const AVX512    = 1 << 5;
        const VNNI512   = 1 << 6;
        const AVX512ICL = 1 << 7;

        // aarch64
        const DOTPROD = 1 << 8;

        const SF_SSE2         = Cpu::SSE2.bits();
        const SF_SSE41_POPCNT = Cpu::SSE41.bits() | Cpu::POPCNT.bits();
        const SF_AVX2         = Cpu::SF_SSE41_POPCNT.bits() | Cpu::AVX2.bits();
        const SF_BMI2         = Cpu::SF_AVX2.bits() | Cpu::FAST_BMI2.bits();
        const SF_AVX512       = Cpu::SF_BMI2.bits() | Cpu::AVX512.bits();
        const SF_VNNI512      = Cpu::SF_AVX512.bits() | Cpu::VNNI512.bits();
        const SF_AVX512ICL    = Cpu::AVX512ICL.bits();
        const SF_NEON_DOTPROD = Cpu::DOTPROD.bits();
    }
}

impl fmt::Display for Cpu {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            f.write_str("-")
        } else {
            self.0.fmt(f)
        }
    }
}

impl Cpu {
    #[cfg(target_arch = "x86_64")]
    pub fn detect() -> Cpu {
        let mut cpu = Cpu::empty();
        cpu.set(Cpu::SSE2, is_x86_feature_detected!("sse2"));
        cpu.set(Cpu::POPCNT, is_x86_feature_detected!("popcnt"));
        cpu.set(Cpu::SSE41, is_x86_feature_detected!("sse4.1"));
        cpu.set(Cpu::AVX2, is_x86_feature_detected!("avx2"));
        cpu.set(
            Cpu::FAST_BMI2,
            is_x86_feature_detected!("bmi2") && {
                // AMD was using slow software emulation for PEXT for a
                // long time. The Zen 3 family (0x19) is the first to
                // implement it in hardware.
                let cpuid = raw_cpuid::CpuId::new();
                cpuid
                    .get_vendor_info()
                    .is_none_or(|v| v.as_str() != "AuthenticAMD")
                    || cpuid
                        .get_feature_info()
                        .is_some_and(|f| f.family_id() >= 0x19)
            },
        );
        cpu.set(
            Cpu::AVX512,
            is_x86_feature_detected!("avx512f") && is_x86_feature_detected!("avx512bw"),
        );
        cpu.set(
            Cpu::VNNI512,
            is_x86_feature_detected!("avx512vnni")
                && is_x86_feature_detected!("avx512dq")
                && is_x86_feature_detected!("avx512f")
                && is_x86_feature_detected!("avx512bw")
                && is_x86_feature_detected!("avx512vl"),
        );
        cpu.set(
            Cpu::AVX512ICL,
            is_x86_feature_detected!("avx512f")
                && is_x86_feature_detected!("avx512cd")
                && is_x86_feature_detected!("avx512vl")
                && is_x86_feature_detected!("avx512dq")
                && is_x86_feature_detected!("avx512bw")
                && is_x86_feature_detected!("avx512ifma")
                && is_x86_feature_detected!("avx512vbmi")
                && is_x86_feature_detected!("avx512vbmi2")
                && is_x86_feature_detected!("avx512vpopcntdq")
                && is_x86_feature_detected!("avx512bitalg")
                && is_x86_feature_detected!("avx512vnni")
                && is_x86_feature_detected!("vpclmulqdq")
                && is_x86_feature_detected!("gfni")
                && is_x86_feature_detected!("vaes"),
        );
        cpu
    }

    #[cfg(target_arch = "aarch64")]
    pub fn detect() -> Cpu {
        let mut cpu = Cpu::empty();
        cpu.set(
            Cpu::DOTPROD,
            std::arch::is_aarch64_feature_detected!("dotprod"),
        );
        cpu
    }

    #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
    pub fn detect() -> Cpu {
        Cpu::empty()
    }

    pub fn requirements(filename: &str) -> Cpu {
        if filename.contains("-armv8-dotprod") {
            Cpu::SF_NEON_DOTPROD
        } else if filename.contains("-x86-64-avx512icl") {
            Cpu::SF_AVX512ICL
        } else if filename.contains("-x86-64-vnni512") {
            Cpu::SF_VNNI512
        } else if filename.contains("-x86-64-avx512") {
            Cpu::SF_AVX512
        } else if filename.contains("-x86-64-bmi2") {
            Cpu::SF_BMI2
        } else if filename.contains("-x86-64-avx2") {
            Cpu::SF_AVX2
        } else if filename.contains("-x86-64-sse41-popcnt") {
            Cpu::SF_SSE41_POPCNT
        } else if filename.contains("-x86-64") {
            Cpu::SF_SSE2
        } else {
            Cpu::empty()
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum EngineFlavor {
    Official,
    MultiVariant,
}

impl EngineFlavor {
    pub fn is_official(self) -> bool {
        matches!(self, EngineFlavor::Official)
    }
}

#[derive(Debug, Default)]
pub struct ByEngineFlavor<T> {
    pub official: T,
    pub multi_variant: T,
}

impl<T> ByEngineFlavor<T> {
    pub fn get(&self, flavor: EngineFlavor) -> &T {
        match flavor {
            EngineFlavor::Official => &self.official,
            EngineFlavor::MultiVariant => &self.multi_variant,
        }
    }

    pub fn get_mut(&mut self, flavor: EngineFlavor) -> &mut T {
        match flavor {
            EngineFlavor::Official => &mut self.official,
            EngineFlavor::MultiVariant => &mut self.multi_variant,
        }
    }
}

#[derive(Debug)]
pub struct Stockfish {
    pub name: String,
    pub path: PathBuf,
}

#[derive(Debug)]
pub struct Assets {
    pub stockfish: ByEngineFlavor<Stockfish>,
    _dir: TempDir, // Will be deleted when dropped
}

impl Assets {
    pub fn prepare(cpu: Cpu) -> io::Result<Assets> {
        let mut stockfish = ByEngineFlavor::<Option<Stockfish>>::default();
        let dir = tempfile::Builder::new().prefix("fishnet-").tempdir()?;

        let mut archive = Archive::new(ZstdDecoder::new(ASSETS_AR_ZST)?);
        while let Some(entry) = archive.next_entry() {
            let mut entry = entry?;
            let filename = str::from_utf8(entry.header().identifier()).expect("utf-8 filename");
            let target_path = dir.path().join(filename); // Trusted
            if filename.starts_with("stockfish-") {
                if stockfish.official.is_none() && cpu.contains(Cpu::requirements(filename)) {
                    stockfish.official = Some(Stockfish {
                        name: filename.to_owned(),
                        path: target_path.clone(),
                    });
                } else {
                    continue;
                }
            }
            if filename.starts_with("fairy-stockfish-") {
                if stockfish.multi_variant.is_none() && cpu.contains(Cpu::requirements(filename)) {
                    stockfish.multi_variant = Some(Stockfish {
                        name: filename.to_owned(),
                        path: target_path.clone(),
                    });
                } else {
                    continue;
                }
            }
            let mode = entry.header().mode();
            io::copy(&mut entry, &mut create_file(&target_path, mode)?)?;
        }

        Ok(Assets {
            stockfish: ByEngineFlavor {
                official: stockfish.official.expect("compatible stockfish"),
                multi_variant: stockfish
                    .multi_variant
                    .expect("compatible multi-variant stockfish"),
            },
            _dir: dir,
        })
    }
}

#[cfg(unix)]
fn create_file(path: &Path, mode: u32) -> io::Result<File> {
    use std::os::unix::fs::OpenOptionsExt as _;
    File::options()
        .create_new(true)
        .write(true)
        .mode(mode)
        .open(path)
}

#[cfg(not(unix))]
fn create_file(path: &Path, _mode: u32) -> io::Result<File> {
    File::options().create_new(true).write(true).open(path)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prepare_assets() {
        Assets::prepare(Cpu::detect()).expect("assets");
    }
}
