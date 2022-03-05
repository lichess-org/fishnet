use std::{env, fs, fs::File, io, path::Path, process::Command};

use glob::glob;

const EVAL_FILE: &str = "nn-13406b1dcbe0.nnue";

fn has_target_feature(feature: &str) -> bool {
    env::var("CARGO_CFG_TARGET_FEATURE")
        .unwrap()
        .split(',')
        .any(|f| f == feature)
}

macro_rules! has_builder_feature {
    ($feature:tt) => {{
        #[cfg(target_arch = "x86_64")]
        {
            is_x86_feature_detected!($feature)
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            false
        }
    }};
}

#[cfg(target_arch = "x86_64")]
fn cross_compiling() -> bool {
    env::var("CARGO_CFG_TARGET_ARCH").unwrap() != "x86_64"
}

#[cfg(target_arch = "aarch64")]
fn cross_compiling() -> bool {
    env::var("CARGO_CFG_TARGET_ARCH").unwrap() != "aarch64"
}

struct Target {
    arch: &'static str,
    pgo: bool,
}

impl Target {
    fn build(&self, src_dir: &'static str, name: &'static str) {
        let release = env::var("PROFILE").unwrap() == "release";
        let windows = env::var("CARGO_CFG_TARGET_FAMILY").unwrap() == "windows";
        let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap();
        let pgo = release && !cross_compiling() && (self.pgo || env::var("SDE_PATH").is_ok());

        let exe = format!(
            "{}-{}{}",
            name,
            self.arch,
            if windows { ".exe" } else { "" }
        );
        if release && !pgo {
            println!(
                "cargo:warning=Building {} without profile-guided optimization",
                exe
            );
        }

        let make = if target_os == "freebsd" {
            "gmake"
        } else {
            "make"
        };

        assert!(
            Command::new(make)
                .arg("--version")
                .status()
                .unwrap_or_else(|err| panic!(
                    "{}. Is `{}` installed?\n\
                    * Debian: sudo apt install build-essential\n\
                    * Arch: sudo pacman -S base-devel\n",
                    err, make
                ))
                .success(),
            "make --version"
        );

        assert!(
            Command::new(make)
                .current_dir(src_dir)
                .env("MAKEFLAGS", env::var("CARGO_MAKEFLAGS").unwrap())
                .env(
                    "CXXFLAGS",
                    format!(
                        "{} -DNNUE_EMBEDDING_OFF -DLARGEBOARDS",
                        env::var("CXXFLAGS").unwrap_or_default()
                    )
                )
                .arg("-B")
                .args(env::var("CXX").ok().map(|cxx| format!("CXX={}", cxx)))
                .arg(format!(
                    "COMP={}",
                    if windows {
                        "mingw"
                    } else if target_os == "linux" {
                        "gcc"
                    } else {
                        "clang"
                    }
                ))
                .arg(format!("ARCH={}", self.arch))
                .arg(format!("EXE={}", exe))
                .arg(if pgo { "profile-build" } else { "build" })
                .status()
                .unwrap()
                .success(),
            "make build"
        );

        assert!(
            Command::new(make)
                .current_dir(src_dir)
                .env("MAKEFLAGS", env::var("CARGO_MAKEFLAGS").unwrap())
                .arg(format!("EXE={}", exe))
                .arg("strip")
                .status()
                .unwrap()
                .success(),
            "make strip"
        );

        compress(src_dir, &exe);

        assert!(
            Command::new(make)
                .current_dir(src_dir)
                .env("MAKEFLAGS", env::var("CARGO_MAKEFLAGS").unwrap())
                .arg("clean")
                .status()
                .unwrap()
                .success(),
            "make clean"
        );

        println!(
            "cargo:rustc-cfg={}",
            exe.replace(|ch| ch == '.' || ch == '-', "_")
        );
    }

    fn build_official(&self) {
        self.build("Stockfish/src", "stockfish");
    }

    fn build_fairy(&self) {
        self.build("Fairy-Stockfish/src", "fairy-stockfish");
    }

    fn build_both(&self) {
        self.build_official();
        self.build_fairy();
    }
}

fn stockfish_build() {
    // Note: The target arch of the build script is the architecture of the
    // builder and decides if pgo is possible. It is not necessarily the same
    // as CARGO_CFG_TARGET_ARCH, the target arch of the fishnet binary.
    //
    // Can skip building more broadly compatible Stockfish binaries and return
    // early when building with something like -C target-cpu=native.

    match env::var("CARGO_CFG_TARGET_ARCH").unwrap().as_str() {
        "x86_64" => {
            Target {
                arch: "x86-64-vnni512",
                pgo: has_builder_feature!("avx512dq")
                    && has_builder_feature!("avx512vl")
                    && has_builder_feature!("avx512vnni"),
            }
            .build_both();

            if has_target_feature("avx512dq")
                && has_target_feature("avx512vl")
                && has_target_feature("avx512vnni")
            {
                return;
            }

            Target {
                arch: "x86-64-avx512",
                pgo: has_builder_feature!("avx512f") && has_builder_feature!("avx512bw"),
            }
            .build_both();

            if has_target_feature("avx512f") && has_target_feature("avx512bw") {
                return;
            }

            Target {
                arch: "x86-64-bmi2",
                pgo: has_builder_feature!("bmi2"),
            }
            .build_both();

            if has_target_feature("bmi2") {
                // Fast bmi2 can not be detected at compile time.
            }

            Target {
                arch: "x86-64-avx2",
                pgo: has_builder_feature!("avx2"),
            }
            .build_both();

            if has_target_feature("avx2") {
                return;
            }

            Target {
                arch: "x86-64-sse41-popcnt",
                pgo: has_builder_feature!("sse4.1") && has_builder_feature!("popcnt"),
            }
            .build_both();

            if has_target_feature("sse4.1") && has_target_feature("popcnt") {
                return;
            }

            Target {
                arch: "x86-64",
                pgo: cfg!(target_arch = "x86_64"),
            }
            .build_both();
        }
        "aarch64" => {
            if env::var("CARGO_CFG_TARGET_OS").unwrap() == "macos" {
                Target {
                    arch: "apple-silicon",
                    pgo: cfg!(target_arch = "aarch64"),
                }
                .build_both();
            } else {
                Target {
                    arch: "armv8",
                    pgo: cfg!(target_arch = "aarch64"),
                }
                .build_both();
            }
        }
        target_arch => {
            unimplemented!("Stockfish build for {} not supported", target_arch);
        }
    }
}

fn compress(dir: &str, file: &str) {
    let compressed =
        File::create(Path::new(&env::var("OUT_DIR").unwrap()).join(&format!("{}.xz", file)))
            .unwrap();
    let mut encoder = xz2::write::XzEncoder::new(compressed, 6);

    let uncompressed_path = Path::new(dir).join(file);
    let mut uncompressed = File::open(&uncompressed_path).unwrap();

    io::copy(&mut uncompressed, &mut encoder).unwrap();
    encoder.finish().unwrap();
    fs::remove_file(uncompressed_path).unwrap();
}

fn hooks() {
    println!("cargo:rerun-if-changed=Cargo.lock");

    println!("cargo:rerun-if-env-changed=CXX");
    println!("cargo:rerun-if-env-changed=CXXFLAGS");
    println!("cargo:rerun-if-env-changed=SDE_PATH");

    println!("cargo:rustc-env=EVAL_FILE={}", EVAL_FILE);
    println!("cargo:rerun-if-changed=Stockfish/src/Makefile");
    for entry in glob("Stockfish/src/**/*.cpp").unwrap() {
        println!("cargo:rerun-if-changed={}", entry.unwrap().display());
    }
    for entry in glob("Stockfish/src/**/*.h").unwrap() {
        println!("cargo:rerun-if-changed={}", entry.unwrap().display());
    }

    println!("cargo:rerun-if-changed=Fairy-Stockfish/src/Makefile");
    for entry in glob("Fairy-Stockfish/src/**/*.cpp").unwrap() {
        println!("cargo:rerun-if-changed={}", entry.unwrap().display());
    }
    for entry in glob("Fairy-Stockfish/src/**/*.h").unwrap() {
        println!("cargo:rerun-if-changed={}", entry.unwrap().display());
    }
}

fn main() {
    hooks();
    stockfish_build();
    compress("Stockfish/src", EVAL_FILE);
    auditable_build::collect_dependency_list();

    // Resource compilation may fail when toolchain does not match target,
    // e.g. windows-msvc toolchain with windows-gnu target.
    #[cfg(target_family = "windows")]
    winres::WindowsResource::new()
        .set_icon("favicon.ico")
        .compile()
        .unwrap_or_else(|err| {
            println!("cargo:warning=Resource compiler not invoked: {}", err);
        });
}
