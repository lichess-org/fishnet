#![forbid(unsafe_code)]

use std::{env, fs, fs::File, io, path::Path, process::Command};

use glob::glob;

const EVAL_FILE: &str = "nn-3475407dc199.nnue";

fn has_target_feature(feature: &str) -> bool {
    env::var("CARGO_CFG_TARGET_FEATURE")
        .unwrap()
        .split(',')
        .any(|f| f == feature)
}

macro_rules! has_x86_64_builder_feature {
    ($feature:tt) => {{
        #[cfg(target_arch = "x86_64")]
        {
            std::arch::is_x86_feature_detected!($feature)
        }
        #[cfg(not(target_arch = "x86_64"))]
        {
            false
        }
    }};
}

macro_rules! has_aarch64_builder_feature {
    ($feature:tt) => {{
        #[cfg(target_arch = "aarch64")]
        {
            std::arch::is_aarch64_feature_detected!($feature)
        }
        #[cfg(not(target_arch = "aarch64"))]
        {
            false
        }
    }};
}

struct Target {
    arch: &'static str,
    native: bool,
    sde: bool,
}

#[derive(Debug, PartialEq, Eq)]
enum Flavor {
    Official,
    MultiVariant,
}

impl Target {
    fn build(&self, flavor: Flavor, src_dir: &'static str, name: &'static str) {
        let release = env::var("PROFILE").unwrap() == "release";
        let windows = env::var("CARGO_CFG_TARGET_FAMILY").unwrap() == "windows";
        let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap();
        let pgo = release && (self.native || (self.sde && env::var("SDE_PATH").is_ok()));

        let exe = format!(
            "{}-{}{}",
            name,
            self.arch,
            if windows { ".exe" } else { "" }
        );
        if release && !pgo {
            println!("cargo:warning=Building {exe} without profile-guided optimization");
        }

        let (comp, default_cxx, default_make) = if windows {
            ("mingw", "g++", "mingw32-make")
        } else if target_os == "linux" {
            ("gcc", "g++", "make")
        } else if target_os == "freebsd" {
            ("clang", "clang++", "gmake")
        } else {
            ("clang", "clang++", "make")
        };

        let make = env::var("MAKE").unwrap_or_else(|_| default_make.to_owned());

        assert!(
            Command::new(&make)
                .arg("--version")
                .status()
                .unwrap_or_else(|err| panic!(
                    "{err}. Is `{make}` installed?\n\
                    * Debian: sudo apt install build-essential\n\
                    * Arch: sudo pacman -S base-devel\n\
                    * MSYS2: pacman -S mingw32-make\n"
                ))
                .success(),
            "$(MAKE) --version"
        );

        let cxx = env::var("CXX").unwrap_or_else(|_| default_cxx.to_owned());

        assert!(
            Command::new(&cxx)
                .arg("--version")
                .status()
                .unwrap_or_else(|err| panic!("{err}. Is `{cxx}` installed?"))
                .success(),
            "$(CXX) --version"
        );

        assert!(
            Path::new(src_dir).is_dir(),
            "Directory {src_dir:?} does not exist. Try: git submodule update --init",
        );

        if flavor == Flavor::Official
            && !Command::new(&make)
                .current_dir(src_dir)
                .env("MAKEFLAGS", env::var("CARGO_MAKEFLAGS").unwrap())
                .arg("-B")
                .arg("net")
                .status()
                .unwrap()
                .success()
        {
            fs::remove_file(Path::new(src_dir).join(EVAL_FILE)).unwrap();
            println!("cargo:warning=Deleted corrupted network file {EVAL_FILE}");
        }

        let mut build_command = Command::new(&make);
        build_command
            .current_dir(src_dir)
            .env("MAKEFLAGS", env::var("CARGO_MAKEFLAGS").unwrap())
            .env(
                "CXXFLAGS",
                format!(
                    "{} -DNNUE_EMBEDDING_OFF",
                    env::var("CXXFLAGS").unwrap_or_default()
                ),
            )
            .arg("-B")
            .arg(format!("COMP={comp}"))
            .arg(format!("CXX={cxx}"))
            .arg(format!("ARCH={}", self.arch))
            .arg(format!("EXE={exe}"))
            .arg(if pgo { "profile-build" } else { "build" });
        if !pgo || self.native {
            // Avoid SDE overhead if not required.
            build_command.env_remove("SDE_PATH");
        }
        assert!(build_command.status().unwrap().success(), "$(MAKE) build");

        assert!(
            Command::new(&make)
                .current_dir(src_dir)
                .env("MAKEFLAGS", env::var("CARGO_MAKEFLAGS").unwrap())
                .arg(format!("EXE={exe}"))
                .arg("strip")
                .status()
                .unwrap()
                .success(),
            "$(MAKE) strip"
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
            "$(MAKE) clean"
        );

        println!(
            "cargo:rustc-cfg={}",
            exe.replace(|ch| ch == '.' || ch == '-', "_")
        );
    }

    fn build_official(&self) {
        self.build(Flavor::Official, "Stockfish/src", "stockfish");
    }

    fn build_multi_variant(&self) {
        self.build(
            Flavor::MultiVariant,
            "Fairy-Stockfish/src",
            "fairy-stockfish",
        );
    }

    fn build_both(&self) {
        self.build_official();
        self.build_multi_variant();
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
            let sde = cfg!(target_arch = "x86_64");

            Target {
                arch: "x86-64-vnni256",
                native: has_x86_64_builder_feature!("avx512dq")
                    && has_x86_64_builder_feature!("avx512vl")
                    && has_x86_64_builder_feature!("avx512vnni"),
                sde,
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
                native: has_x86_64_builder_feature!("avx512f")
                    && has_x86_64_builder_feature!("avx512bw"),
                sde,
            }
            .build_both();

            if has_target_feature("avx512f") && has_target_feature("avx512bw") {
                return;
            }

            Target {
                arch: "x86-64-bmi2",
                native: has_x86_64_builder_feature!("bmi2"),
                sde,
            }
            .build_both();

            if has_target_feature("bmi2") {
                // Fast bmi2 can not be detected at compile time.
            }

            Target {
                arch: "x86-64-avx2",
                native: has_x86_64_builder_feature!("avx2"),
                sde,
            }
            .build_both();

            if has_target_feature("avx2") {
                return;
            }

            Target {
                arch: "x86-64-sse41-popcnt",
                native: has_x86_64_builder_feature!("sse4.1")
                    && has_x86_64_builder_feature!("popcnt"),
                sde,
            }
            .build_both();

            if has_target_feature("sse4.1") && has_target_feature("popcnt") {
                return;
            }

            Target {
                arch: "x86-64",
                native: cfg!(target_arch = "x86_64"),
                sde,
            }
            .build_both();
        }
        "aarch64" => {
            let native = cfg!(target_arch = "aarch64");

            if env::var("CARGO_CFG_TARGET_OS").unwrap() == "macos" {
                Target {
                    arch: "apple-silicon",
                    native,
                    sde: false,
                }
                .build_both();
            } else {
                Target {
                    arch: "armv8-dotprod",
                    native: native && has_aarch64_builder_feature!("dotprod"),
                    sde: false,
                }
                .build_official();

                Target {
                    arch: "armv8",
                    native,
                    sde: false,
                }
                .build_multi_variant();

                if has_target_feature("dotprod") {
                    return;
                }

                Target {
                    arch: "armv8",
                    native,
                    sde: false,
                }
                .build_official();
            }
        }
        target_arch => {
            unimplemented!("Stockfish build for {} not supported", target_arch);
        }
    }
}

fn compress(dir: &str, file: &str) {
    let compressed =
        File::create(Path::new(&env::var("OUT_DIR").unwrap()).join(format!("{file}.xz"))).unwrap();
    let mut encoder = xz2::write::XzEncoder::new(compressed, 6);

    let uncompressed_path = Path::new(dir).join(file);
    let mut uncompressed = File::open(&uncompressed_path).unwrap_or_else(|err| {
        panic!("Failed to open {uncompressed_path:?} for compression: {err}",)
    });

    io::copy(&mut uncompressed, &mut encoder).unwrap();
    encoder.finish().unwrap();
    fs::remove_file(uncompressed_path).unwrap();
}

fn hooks() {
    println!("cargo:rerun-if-env-changed=CXX");
    println!("cargo:rerun-if-env-changed=CXXFLAGS");
    println!("cargo:rerun-if-env-changed=DEPENDFLAGS");
    println!("cargo:rerun-if-env-changed=LDFLAGS");
    println!("cargo:rerun-if-env-changed=MAKE");
    println!("cargo:rerun-if-env-changed=SDE_PATH");

    println!("cargo:rustc-env=EVAL_FILE={EVAL_FILE}");
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

    println!("cargo:rerun-if-changed=favicon.ico");
}

fn main() {
    hooks();
    stockfish_build();
    compress("Stockfish/src", EVAL_FILE);

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
