use std::{
    error::Error,
    fmt, fs, io,
    io::Write,
    num::{NonZeroUsize, ParseIntError},
    path::PathBuf,
    str::FromStr,
    thread::available_parallelism,
    time::Duration,
};

use clap::Parser;
use configparser::ini::Ini;
use url::Url;

use crate::{api, logger::Logger};

const DEFAULT_ENDPOINT: &str = "https://lichess.org/fishnet";

/// Distributed Stockfish analysis for lichess.org.
#[derive(Debug, Parser)]
#[clap(version, disable_help_subcommand = true)]
pub struct Opt {
    #[clap(flatten)]
    pub verbose: Verbose,

    /// Automatically install available updates on startup and at random
    /// intervals.
    #[clap(long, global = true)]
    pub auto_update: bool,

    /// Configuration file.
    #[clap(long, parse(from_os_str), default_value = "fishnet.ini", global = true)]
    pub conf: PathBuf,

    /// Do not use a configuration file.
    #[clap(long, conflicts_with = "conf", global = true)]
    pub no_conf: bool,

    /// Fishnet key.
    #[clap(long, alias = "apikey", short = 'k', global = true)]
    pub key: Option<Key>,

    /// Fishnet key file.
    #[clap(long, parse(from_os_str), conflicts_with = "key", global = true)]
    pub key_file: Option<PathBuf>,

    /// Lichess HTTP endpoint.
    #[clap(long, global = true)]
    pub endpoint: Option<Endpoint>,

    /// Number of logical CPU cores to use for engine processes
    /// (or auto for n - 1, or all for n).
    #[clap(long, alias = "threads", global = true)]
    pub cores: Option<Cores>,

    /// Maximum backoff time. The client will use randomized expontential
    /// backoff when repeatedly receiving no job.
    #[clap(long, default_value = "30s", global = true)]
    pub max_backoff: ParsedDuration,

    #[clap(flatten)]
    pub backlog: BacklogOpt,

    #[clap(subcommand)]
    pub command: Option<Command>,
}

impl Opt {
    pub fn endpoint(&self) -> Endpoint {
        self.endpoint.clone().unwrap_or_default()
    }
}

#[derive(Debug, Clone)]
pub struct Endpoint {
    pub url: Url,
}

impl Default for Endpoint {
    fn default() -> Endpoint {
        DEFAULT_ENDPOINT.parse().expect("default endpoint is valid")
    }
}

impl fmt::Display for Endpoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.url, f)
    }
}

impl FromStr for Endpoint {
    type Err = url::ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut url: Url = s.parse()?;
        if let Some(stripped_path) = url.path().to_owned().strip_suffix('/') {
            url.set_path(stripped_path);
        }
        Ok(Endpoint { url })
    }
}

impl Endpoint {
    fn is_development(&self) -> bool {
        self.url.host_str() != Some("lichess.org")
    }
}

#[derive(Debug, Default, Copy, Clone, Parser)]
pub struct Verbose {
    /// Increase verbosity.
    #[clap(long = "verbose", short = 'v', parse(from_occurrences), global = true)]
    pub level: usize,
}

#[derive(Debug, Clone)]
pub struct Key(pub String);

#[derive(Debug)]
pub enum KeyError {
    EmptyKey,
    InvalidKey,
    AccessDenied,
}

impl fmt::Display for KeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            KeyError::EmptyKey => "key expected to be non-empty",
            KeyError::InvalidKey => "key expected to be alphanumeric",
            KeyError::AccessDenied => "access denied",
        })
    }
}

impl Error for KeyError {}

impl FromStr for Key {
    type Err = KeyError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            Err(KeyError::EmptyKey)
        } else if s.chars().all(|c| char::is_ascii_alphanumeric(&c)) {
            Ok(Key(s.to_owned()))
        } else {
            Err(KeyError::InvalidKey)
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Cores {
    Auto,
    All,
    Number(NonZeroUsize),
}

impl Default for Cores {
    fn default() -> Cores {
        Cores::Auto
    }
}

impl FromStr for Cores {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(if s == "auto" {
            Cores::Auto
        } else if s == "all" || s == "max" {
            Cores::All
        } else {
            Cores::Number(s.parse()?)
        })
    }
}

impl fmt::Display for Cores {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Cores::Auto => f.write_str("auto"),
            Cores::All => f.write_str("all"),
            Cores::Number(n) => write!(f, "{}", n),
        }
    }
}

impl Cores {
    pub fn number(self) -> NonZeroUsize {
        let num_cpus = available_parallelism().expect("num cpus");
        match self {
            Cores::Number(n) => n,
            Cores::Auto => NonZeroUsize::new(num_cpus.get() - 1)
                .unwrap_or_else(|| NonZeroUsize::new(1).unwrap()),
            Cores::All => num_cpus,
        }
    }
}

#[derive(Debug, Clone, Parser)]
pub struct BacklogOpt {
    /// Prefer to run high-priority jobs only if older than this duration
    /// (for example 120s).
    #[clap(long = "user-backlog", global = true)]
    pub user: Option<Backlog>,

    /// Prefer to run low-priority jobs only if older than this duration
    /// (for example 2h).
    #[clap(long = "system-backlog", global = true)]
    pub system: Option<Backlog>,
}

#[derive(Debug, Copy, Clone)]
pub enum Backlog {
    Short,
    Long,
    Duration(Duration),
}

impl Default for Backlog {
    fn default() -> Backlog {
        Backlog::Duration(Duration::default())
    }
}

impl From<Backlog> for Duration {
    fn from(backlog: Backlog) -> Duration {
        match backlog {
            Backlog::Short => Duration::from_secs(30),
            Backlog::Long => Duration::from_secs(60 * 60),
            Backlog::Duration(d) => d,
        }
    }
}

impl FromStr for Backlog {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(if s == "short" {
            Backlog::Short
        } else if s == "long" {
            Backlog::Long
        } else {
            Backlog::Duration(s.parse::<ParsedDuration>()?.into())
        })
    }
}

impl fmt::Display for Backlog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Backlog::Short => f.write_str("short"),
            Backlog::Long => f.write_str("long"),
            Backlog::Duration(d) => write!(f, "{}s", d.as_secs()),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ParsedDuration(Duration);

impl FromStr for ParsedDuration {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (s, factor) = if let Some(s) = s.strip_suffix('d') {
            (s, 1000 * 60 * 60 * 24)
        } else if let Some(s) = s.strip_suffix('h') {
            (s, 1000 * 60 * 60)
        } else if let Some(s) = s.strip_suffix('m') {
            (s, 1000 * 60)
        } else if let Some(s) = s.strip_suffix("ms") {
            (s, 1)
        } else {
            (s.strip_suffix('s').unwrap_or(s), 1000)
        };
        Ok(ParsedDuration(Duration::from_millis(
            u64::from(s.trim().parse::<u32>()?) * factor,
        )))
    }
}

impl From<ParsedDuration> for Duration {
    fn from(ParsedDuration(duration): ParsedDuration) -> Duration {
        duration
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Parser)]
pub enum Command {
    /// Donate CPU time by running analysis (default).
    Run,
    /// Run interactive configuration.
    Configure,
    /// Generate a systemd service file.
    Systemd,
    /// Generate a systemd user service file.
    SystemdUser,
    /// Show GPLv3 license.
    License,
}

impl Command {
    pub fn is_systemd(self) -> bool {
        matches!(self, Command::Systemd | Command::SystemdUser)
    }
}

#[derive(Debug, Copy, Clone)]
enum Toggle {
    Yes,
    No,
    Default,
}

impl Default for Toggle {
    fn default() -> Toggle {
        Toggle::Default
    }
}

impl FromStr for Toggle {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim().to_lowercase();
        match s.as_str() {
            "y" | "j" | "yes" | "yep" | "yay" | "true" | "t" | "1" | "ok" => Ok(Toggle::Yes),
            "n" | "no" | "nop" | "nope" | "nay" | "f" | "false" | "0" => Ok(Toggle::No),
            "" => Ok(Toggle::Default),
            _ => Err(()),
        }
    }
}

#[rustfmt::skip]
fn intro() {
    println!(r#"#   _________         .    ."#);
    println!(r#"#  (..       \_    ,  |\  /|"#);
    println!(r#"#   \       O  \  /|  \ \/ /"#);
    println!(r#"#    \______    \/ |   \  /      _____ _     _     _   _      _"#);
    println!(r#"#       vvvv\    \ |   /  |     |  ___(_)___| |__ | \ | | ___| |_"#);
    println!(r#"#       \^^^^  ==   \_/   |     | |_  | / __| '_ \|  \| |/ _ \ __|"#);
    println!(r#"#        `\_   ===    \.  |     |  _| | \__ \ | | | |\  |  __/ |_"#);
    println!(r#"#        / /\_   \ /      |     |_|   |_|___/_| |_|_| \_|\___|\__| {}"#, env!("CARGO_PKG_VERSION"));
    println!(r#"#        |/   \_  \|      /"#);
    println!(r#"#               \________/      Distributed Stockfish analysis for lichess.org"#);
}

pub async fn parse_and_configure() -> Opt {
    let mut opt = Opt::parse();

    // Show intro and configure logger.
    let is_systemd = opt.command.map_or(false, Command::is_systemd);
    let logger = Logger::new(opt.verbose, is_systemd);
    if !is_systemd {
        intro();
    }

    // Handle key file.
    if !is_systemd {
        if let Some(key_file) = opt.key_file.take() {
            opt.key = Some(
                fs::read_to_string(key_file)
                    .expect("read key file")
                    .trim()
                    .parse()
                    .expect("valid key from key file"),
            );
        }
    }

    // Handle config file.
    if opt.command == Some(Command::Configure)
        || (opt.command != Some(Command::License) && !opt.no_conf)
    {
        let mut ini = Ini::new();
        ini.set_default_section("Fishnet");

        // Load ini.
        let file_found = match fs::read_to_string(&opt.conf) {
            Ok(contents) => {
                ini.read(contents).expect("parse config file");
                true
            }
            Err(err) if err.kind() == io::ErrorKind::NotFound => false,
            Err(err) => panic!("failed to open config file: {}", err),
        };

        // Configuration dialog.
        if (!file_found && opt.command != Some(Command::Run))
            || opt.command == Some(Command::Configure)
        {
            logger.headline("Configuration");

            // Step 1: Endpoint (configured with --endpoint only).
            let endpoint = opt.endpoint.clone().unwrap_or_else(|| {
                ini.get("Fishnet", "Endpoint")
                    .unwrap_or_else(|| DEFAULT_ENDPOINT.to_owned())
                    .parse()
                    .expect("valid endpoint from fishnet.ini")
            });

            // Step 2: Key.
            loop {
                let mut key = String::new();
                let required = if let Some(current) = ini.get("Fishnet", "Key") {
                    eprint!(
                        "Personal fishnet key (append ! to force, default: keep {}): ",
                        "*".repeat(current.chars().count())
                    );
                    false
                } else if endpoint.is_development() {
                    eprint!("Personal fishnet key (append ! to force, probably not required): ");
                    false
                } else {
                    eprint!("Personal fishnet key (append ! to force, https://lichess.org/get-fishnet): ");
                    true
                };

                io::stderr().flush().expect("flush stderr");
                io::stdin()
                    .read_line(&mut key)
                    .expect("read key from stdin");

                let key = key.trim();
                let (key, network) = if key.is_empty() {
                    if required {
                        eprintln!("Key required.");
                        continue;
                    }
                    break;
                } else if let Some(key) = key.strip_suffix('!') {
                    (key, false)
                } else {
                    (key, true)
                };

                let key = match Key::from_str(key) {
                    Ok(key) if !network => Ok(key),
                    Ok(key) => {
                        let mut api =
                            api::spawn(endpoint.clone(), Some(key.clone()), logger.clone());
                        match api.check_key().await {
                            Some(Ok(())) => Ok(key),
                            Some(Err(err)) => Err(err),
                            None => continue, // server/network arror already logged
                        }
                    }
                    Err(err) => Err(err),
                };

                match key {
                    Ok(Key(key)) => {
                        ini.set("Fishnet", "Key", Some(key));
                        break;
                    }
                    Err(err) => eprintln!("Invalid: {}", err),
                }
            }

            // Step 3: Cores.
            eprintln!();
            loop {
                let mut cores = String::new();
                let all = Cores::All.number();
                let auto = Cores::Auto.number();
                eprint!(
                    "Number of logical cores to use for engine threads (default {}, max {}): ",
                    auto, all
                );
                io::stderr().flush().expect("flush stderr");
                io::stdin()
                    .read_line(&mut cores)
                    .expect("read cores from stdin");

                match Some(cores.trim())
                    .filter(|c| !c.is_empty())
                    .map_or(Ok(Cores::Auto), Cores::from_str)
                {
                    Ok(Cores::Number(n)) if n > all => {
                        eprintln!("At most {} logical cores available on your machine.", all);
                    }
                    Ok(cores) => {
                        ini.set("Fishnet", "Cores", Some(cores.to_string()));
                        break;
                    }
                    Err(err) => eprintln!("Invalid: {}", err),
                }
            }

            // Step 4: Backlog.
            eprintln!();
            eprintln!("You can choose to not join unless a backlog is building up. Examples:");
            eprintln!("* Rented server exclusively for fishnet: choose no");
            eprintln!("* Running on a laptop: choose yes");
            loop {
                let mut backlog = String::new();
                eprint!("Would you prefer to keep your client idle? (default: no) ");
                io::stderr().flush().expect("flush stderr");
                io::stdin()
                    .read_line(&mut backlog)
                    .expect("read backlog from stdin");

                match Toggle::from_str(&backlog) {
                    Ok(Toggle::Yes) => {
                        ini.setstr("Fishnet", "UserBacklog", Some("short"));
                        ini.setstr("Fishnet", "SystemBacklog", Some("long"));
                        break;
                    }
                    Ok(Toggle::No | Toggle::Default) => {
                        ini.setstr("Fishnet", "UserBacklog", Some("0"));
                        ini.setstr("Fishnet", "SystemBacklog", Some("0"));
                        break;
                    }
                    Err(_) => (),
                }
            }

            // Step 5: Write config.
            eprintln!();
            loop {
                let mut write = String::new();
                eprint!(
                    "Done. Write configuration to {:?} now? (default: yes) ",
                    opt.conf
                );
                io::stderr().flush().expect("flush stderr");
                io::stdin()
                    .read_line(&mut write)
                    .expect("read confirmation from stdin");

                if let Ok(Toggle::Yes | Toggle::Default) = Toggle::from_str(&write) {
                    let contents = ini.writes();
                    fs::write(&opt.conf, contents).expect("write config");
                    break;
                }
            }

            eprintln!();
        }

        // Merge config file into command line arguments.
        if !is_systemd {
            opt.endpoint = opt.endpoint.or_else(|| {
                ini.get("Fishnet", "Endpoint")
                    .map(|e| e.parse().expect("valid endpoint"))
            });

            opt.key = opt.key.or_else(|| {
                ini.get("Fishnet", "Key")
                    .map(|k| k.parse().expect("valid key"))
            });

            opt.cores = opt.cores.or_else(|| {
                ini.get("Fishnet", "Cores")
                    .map(|c| c.parse().expect("valid cores"))
            });

            opt.backlog.user = opt.backlog.user.or_else(|| {
                ini.get("Fishnet", "UserBacklog")
                    .map(|b| b.parse().expect("valid user backlog"))
            });
            opt.backlog.system = opt.backlog.system.or_else(|| {
                ini.get("Fishnet", "SystemBacklog")
                    .map(|b| b.parse().expect("valid system backlog"))
            });
        }
    }

    // Validate number of cores.
    let all = Cores::All.number();
    match opt.cores {
        Some(Cores::Number(n)) if n > all => {
            logger.warn(&format!(
                "Requested logical {} cores, but only {} available. Capped.",
                n, all
            ));
            opt.cores = Some(Cores::All);
        }
        _ => (),
    }

    opt
}
