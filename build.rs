#![forbid(unsafe_code)]

use std::{
    env,
    fs::{self, File},
    hash::{DefaultHasher, Hash as _, Hasher as _},
    io::Write,
    path::{Path, PathBuf},
    process::Command,
    sync::LazyLock,
};

use glob::glob;
use zstd::stream::write::Encoder as ZstdEncoder;

static OUT_PATH: LazyLock<PathBuf> = LazyLock::new(|| PathBuf::from(&env::var("OUT_DIR").unwrap()));

static SF_SOURCE_FILES: LazyLock<Vec<PathBuf>> = LazyLock::new(|| {
    assert!(
        Path::new("Stockfish").join("src").is_dir(),
        "Directory Stockfish/src does not exist. Try: git submodule update --init",
    );
    assert!(
        Path::new("Fairy-Stockfish").join("src").is_dir(),
        "Directory Fairy-Stockfish/src does not exist. Try: git submodule update --init",
    );

    [
        // Stockfish
        "Stockfish/src/Makefile",
        "Stockfish/**/*.sh",
        "Stockfish/src/**/*.cpp",
        "Stockfish/src/**/*.h",
        // Fairy-Stockfish
        "Fairy-Stockfish/src/Makefile",
        "Fairy-Stockfish/src/**/*.cpp",
        "Fairy-Stockfish/src/**/*.h",
    ]
    .iter()
    .flat_map(|pattern| glob(pattern).unwrap())
    .collect::<Result<Vec<PathBuf>, _>>()
    .unwrap()
});

static SF_BUILD_PATH: LazyLock<PathBuf> = LazyLock::new(|| {
    let mut hasher = DefaultHasher::new();
    (*SF_SOURCE_FILES).hash(&mut hasher);
    OUT_PATH.join(hasher.finish().to_string())
});

fn main() {
    println!(
        "cargo:rustc-env=FISHNET_TARGET={}",
        env::var("TARGET").unwrap()
    );

    // Build Stockfish and Fairy-Stockfish and archive them
    // (along with eval files).
    let mut archive = ar::Builder::new(
        ZstdEncoder::new(File::create(OUT_PATH.join("assets.ar.zst")).unwrap(), 6).unwrap(),
    );
    stockfish_build(&mut archive);
    add_nnues(&mut archive);
    archive.into_inner().unwrap().finish().unwrap();

    add_favicon();
}

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

#[allow(clippy::nonminimal_bool, clippy::eq_op)]
fn stockfish_build<W: Write>(archive: &mut ar::Builder<W>) {
    println!("cargo:rerun-if-env-changed=CXX");
    println!("cargo:rerun-if-env-changed=CXXFLAGS");
    println!("cargo:rerun-if-env-changed=DEPENDFLAGS");
    println!("cargo:rerun-if-env-changed=LDFLAGS");
    println!("cargo:rerun-if-env-changed=MAKE");
    println!("cargo:rerun-if-env-changed=SDE_PATH");

    for source_file in &*SF_SOURCE_FILES {
        fs::create_dir_all(SF_BUILD_PATH.join(source_file.parent().unwrap())).unwrap();
        fs::copy(source_file, SF_BUILD_PATH.join(source_file)).unwrap();
        println!("cargo:rerun-if-changed={}", source_file.display());
    }

    // Note: The target arch of the build script is the architecture of the
    // builder and decides if pgo is possible. It is not necessarily the same
    // as CARGO_CFG_TARGET_ARCH, the target arch of the fishnet binary.
    //
    // Can skip building more broadly compatible Stockfish binaries and return
    // early when building with something like -C target-cpu=native.

    match env::var("CARGO_CFG_TARGET_ARCH").unwrap().as_str() {
        "x86_64" => {
            let sde = env::var("SDE_PATH")
                .ok()
                .filter(|_| cfg!(target_arch = "x86_64"));

            Target {
                arch: "x86-64-avx512icl",
                native: has_x86_64_builder_feature!("avx512f")
                    && has_x86_64_builder_feature!("avx512cd")
                    && has_x86_64_builder_feature!("avx512vl")
                    && has_x86_64_builder_feature!("avx512dq")
                    && has_x86_64_builder_feature!("avx512bw")
                    && has_x86_64_builder_feature!("avx512ifma")
                    && has_x86_64_builder_feature!("avx512vbmi")
                    && has_x86_64_builder_feature!("avx512vbmi2")
                    && has_x86_64_builder_feature!("avx512vpopcntdq")
                    && has_x86_64_builder_feature!("avx512bitalg")
                    && has_x86_64_builder_feature!("avx512vnni")
                    && has_x86_64_builder_feature!("vpclmulqdq")
                    && has_x86_64_builder_feature!("gfni")
                    && has_x86_64_builder_feature!("vaes"),
                sde: sde.clone(),
            }
            .build_official(archive);

            let vnni512 = Target {
                arch: "x86-64-vnni512",
                native: has_x86_64_builder_feature!("avx512vnni")
                    && has_x86_64_builder_feature!("avx512dq")
                    && has_x86_64_builder_feature!("avx512f")
                    && has_x86_64_builder_feature!("avx512bw")
                    && has_x86_64_builder_feature!("avx512vl"),
                sde: sde.clone(),
            };
            vnni512.build_multi_variant(archive);
            if has_target_feature("avx512f")
                && has_target_feature("avx512cd")
                && has_target_feature("avx512vl")
                && has_target_feature("avx512dq")
                && has_target_feature("avx512bw")
                && has_target_feature("avx512ifma")
                && has_target_feature("avx512vbmi")
                && has_target_feature("avx512vbmi2")
                && has_target_feature("avx512vpopcntdq")
                && has_target_feature("avx512bitalg")
                && has_target_feature("avx512vnni")
                && has_target_feature("vpclmulqdq")
                && has_target_feature("gfni")
                && has_target_feature("vaes")
            {
                return;
            }
            vnni512.build_official(archive);

            if has_target_feature("avx512vnni")
                && has_target_feature("avx512dq")
                && has_target_feature("avx512f")
                && has_target_feature("avx512bw")
                && has_target_feature("avx512vl")
            {
                return;
            }

            Target {
                arch: "x86-64-avx512",
                native: has_x86_64_builder_feature!("avx512f")
                    && has_x86_64_builder_feature!("avx512bw"),
                sde: sde.clone(),
            }
            .build_both(archive);

            if has_target_feature("avx512f") && has_target_feature("avx512bw") {
                return;
            }

            Target {
                arch: "x86-64-bmi2",
                native: has_x86_64_builder_feature!("bmi2"),
                sde: sde.clone(),
            }
            .build_both(archive);

            if has_target_feature("bmi2") {
                // Fast bmi2 can not be detected at compile time.
            }

            Target {
                arch: "x86-64-avx2",
                native: has_x86_64_builder_feature!("avx2"),
                sde: sde.clone(),
            }
            .build_both(archive);

            if has_target_feature("avx2") {
                return;
            }

            Target {
                arch: "x86-64-sse41-popcnt",
                native: has_x86_64_builder_feature!("sse4.1")
                    && has_x86_64_builder_feature!("popcnt"),
                sde: sde.clone(),
            }
            .build_both(archive);

            if has_target_feature("sse4.1") && has_target_feature("popcnt") {
                return;
            }

            Target {
                arch: "x86-64",
                native: cfg!(target_arch = "x86_64"),
                sde,
            }
            .build_both(archive);
        }
        "aarch64" => {
            let native = cfg!(target_arch = "aarch64");

            if env::var("CARGO_CFG_TARGET_OS").unwrap() == "macos" {
                Target {
                    arch: "apple-silicon",
                    native,
                    sde: None,
                }
                .build_both(archive);
            } else {
                Target {
                    arch: "armv8-dotprod",
                    native: native && has_aarch64_builder_feature!("dotprod"),
                    sde: None,
                }
                .build_official(archive);

                Target {
                    arch: "armv8",
                    native,
                    sde: None,
                }
                .build_multi_variant(archive);

                if has_target_feature("dotprod") {
                    return;
                }

                Target {
                    arch: "armv8",
                    native,
                    sde: None,
                }
                .build_official(archive);
            }
        }
        target_arch => {
            unimplemented!("Stockfish build for {} not supported", target_arch);
        }
    }
}

struct Target {
    arch: &'static str,
    native: bool,
    sde: Option<String>,
}

impl Target {
    fn build<W: Write>(&self, src_path: &Path, name: &'static str, archive: &mut ar::Builder<W>) {
        let release = env::var("PROFILE").unwrap() == "release";
        let windows = env::var("CARGO_CFG_TARGET_FAMILY").unwrap() == "windows";
        let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap();
        let sde = self.sde.as_ref().filter(|_| !self.native);
        let pgo = release && (self.native || sde.is_some());

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
            Command::new(&make)
                .current_dir(src_path)
                .env("MAKEFLAGS", env::var("CARGO_MAKEFLAGS").unwrap())
                .arg("clean")
                .status()
                .unwrap()
                .success(),
            "$(MAKE) clean"
        );

        assert!(
            Command::new(&make)
                .current_dir(src_path)
                .env("MAKEFLAGS", env::var("CARGO_MAKEFLAGS").unwrap())
                .env(
                    "CXXFLAGS",
                    format!(
                        "{} -DNNUE_EMBEDDING_OFF",
                        env::var("CXXFLAGS").unwrap_or_default()
                    ),
                )
                .env_remove("SDE_PATH")
                .env_remove("WINE_PATH")
                .args(sde.map(|e| format!("WINE_PATH={e} --")))
                .args(sde.map(|e| format!("SDE_PATH={e}")))
                .arg("-B")
                .arg(format!("COMP={comp}"))
                .arg(format!("CXX={cxx}"))
                .arg(format!("ARCH={}", self.arch))
                .arg(format!("EXE={exe}"))
                .arg(if pgo { "profile-build" } else { "build" })
                .status()
                .unwrap()
                .success(),
            "$(MAKE) build"
        );

        assert!(
            Command::new(&make)
                .current_dir(src_path)
                .env("MAKEFLAGS", env::var("CARGO_MAKEFLAGS").unwrap())
                .arg(format!("EXE={exe}"))
                .arg("strip")
                .status()
                .unwrap()
                .success(),
            "$(MAKE) strip"
        );

        let exe_path = Path::new(src_path).join(exe);
        append_file(archive, &exe_path, 0o755);
        fs::remove_file(&exe_path).unwrap();
    }

    fn build_official<W: Write>(&self, archive: &mut ar::Builder<W>) {
        self.build(
            &SF_BUILD_PATH.join("Stockfish").join("src"),
            "stockfish",
            archive,
        );
    }

    fn build_multi_variant<W: Write>(&self, archive: &mut ar::Builder<W>) {
        self.build(
            &SF_BUILD_PATH.join("Fairy-Stockfish").join("src"),
            "fairy-stockfish",
            archive,
        );
    }

    fn build_both<W: Write>(&self, archive: &mut ar::Builder<W>) {
        self.build_official(archive);
        self.build_multi_variant(archive);
    }
}

fn add_nnues<W: Write>(archive: &mut ar::Builder<W>) {
    assert!(
        Path::new("assets").join("README.md").is_file(),
        "assets/README.md does not exist. Try: git submodule update --init"
    );

    for nnue in glob("assets/**/*.nnue").unwrap() {
        let nnue = nnue.unwrap();
        println!("cargo:rerun-if-changed={}", nnue.display());
        append_file(archive, nnue, 0o644);
    }

    println!(
        "cargo:rustc-env=FISHNET_FAIRY_STOCKFISH_EVAL_FILES={}",
        glob("assets/Fairy-Stockfish/*.nnue")
            .unwrap()
            .map(|path| path
                .unwrap()
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_owned())
            .collect::<Vec<_>>()
            .join(if cfg!(windows) { ";" } else { ":" })
    );
}

fn append_file<W: Write, P: AsRef<Path>>(archive: &mut ar::Builder<W>, path: P, mode: u32) {
    let file = File::open(&path).unwrap();
    let metadata = file.metadata().unwrap();
    let mut header = ar::Header::new(
        path.as_ref()
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .as_bytes()
            .to_vec(),
        metadata.len(),
    );
    header.set_mode(mode);
    archive.append(&header, file).unwrap();
}

fn add_favicon() {
    #[cfg(target_family = "windows")]
    {
        println!("cargo:rerun-if-changed=favicon.ico");
        winres::WindowsResource::new()
            .set_icon("favicon.ico")
            .compile()
            .unwrap_or_else(|err| {
                // Resource compilation may fail when toolchain does not match
                // target, e.g. windows-msvc toolchain with windows-gnu target.
                // Treat as non-fatal.
                println!("cargo:warning=Resource compiler not invoked: {}", err);
            });
    }
}
