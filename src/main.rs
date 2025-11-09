mod client;
mod config;
mod labels;
mod profile;
use crate::client::{GitHub, LabelMakerError, Repository};
use crate::config::Config;
use crate::labels::*;
use crate::profile::*;
use anstream::AutoStream;
use anstyle::{AnsiColor, Style};
use anyhow::Context;
use clap::{Args, Parser, Subcommand, builder::ArgAction};
use ghrepo::{GHRepo, LocalRepo, is_valid_name};
use log::{Level, LevelFilter};
use minigh::RequestError;
use std::io;
use std::path::PathBuf;
use std::process::ExitCode;

/// Create & enforce sets of labels in GitHub repositories
///
/// Visit <https://github.com/jwodder/labelmaker> for more information.
#[derive(Clone, Debug, Parser, PartialEq)]
#[command(version)]
struct Arguments {
    /// Set logging level
    #[arg(
        short,
        long,
        default_value = "INFO",
        value_name = "OFF|ERROR|WARN|INFO|DEBUG|TRACE"
    )]
    log_level: LevelFilter,

    #[command(subcommand)]
    command: Command,
}

impl Arguments {
    fn run(self) -> anyhow::Result<()> {
        init_logging(self.log_level);
        let token = gh_token::get().context("unable to fetch GitHub access token")?;
        let client = GitHub::new(&token)?;
        self.command.run(client)
    }
}

#[derive(Clone, Debug, PartialEq, Subcommand)]
enum Command {
    /// Apply a set of label specifications to GitHub repositories
    Apply {
        /// Do not change anything in GitHub, but do emit log messages showing
        /// what would be changed.
        #[arg(long)]
        dry_run: bool,

        /// Specify which profile in the configuration file to use.  Defaults
        /// to the value of `defaults.profile` in the configuration file, or to
        /// `default`.
        #[arg(short = 'P', long, value_name = "NAME")]
        profile: Option<String>,

        /// Also operate on all repositories listed in the given file.
        ///
        /// Repositories must be listed one per line.  Leading & trailing
        /// whitespace is ignored.  Blank lines and lines starting with '#' are
        /// skipped.
        #[arg(short = 'F', long, value_name = "FILE")]
        repo_file: Option<patharg::InputArg>,

        /// A configuration file describing what labels to create and/or update
        /// in each repository
        config: PathBuf,

        /// The GitHub repositories to operate on.
        ///
        /// Repositories can be specified in the form `OWNER/NAME` (or, when
        /// `OWNER` is the authenticated user, just `NAME`) and/or as GitHub
        /// repository URLs.
        ///
        /// If no repositories are specified, then the GitHub repository for
        /// the local Git repository is used.
        repository: Vec<String>,
    },
    /// Dump a repository's labels as a configuration file
    Fetch {
        /// File to output the configuration to.
        ///
        /// Defaults to standard output.
        #[arg(short, long, default_value_t, hide_default_value = true)]
        outfile: patharg::OutputArg,

        /// Name of the profile to define in the generated configuration file
        #[arg(short = 'P', long, value_name = "NAME", default_value = "default")]
        profile: String,

        /// The GitHub repository to operate on.
        ///
        /// The repository can be specified in the form `OWNER/NAME` (or, when
        /// `OWNER` is the authenticated user, just `NAME`) or as a GitHub
        /// repository URL.
        ///
        /// If not specified, then the GitHub repository for the local Git
        /// repository is used.
        repository: Option<String>,
    },
    Make(Make),
}

impl Command {
    fn run(self, client: GitHub) -> anyhow::Result<()> {
        match self {
            Command::Apply {
                dry_run,
                profile,
                repo_file,
                config,
                mut repository,
            } => {
                let cfg = Config::load(&config)?;
                let profile = match profile {
                    Some(p) => cfg.get_profile(&p)?,
                    None => cfg.get_default_profile()?,
                };
                let mut repo_parser = RepoParser::new(&client);
                if let Some(p) = repo_file {
                    let lines = p.lines().with_context(|| format!("failed to open {p:#}"))?;
                    for ln in lines {
                        let ln = ln.with_context(|| format!("failed to read from {p:#}"))?;
                        let ln = ln.trim();
                        if !(ln.is_empty() || ln.starts_with('#')) {
                            repository.push(ln.to_owned());
                        }
                    }
                }
                let repos = if repository.is_empty() {
                    vec![repo_parser.default()?]
                } else {
                    let mut repos = Vec::with_capacity(repository.len());
                    for s in repository {
                        repos.push(repo_parser.parse(&s)?);
                    }
                    repos
                };
                let mut rng = rand::rng();
                for r in repos {
                    log::info!("Applying profile {:?} to repository {}", profile.name(), r);
                    let mut maker = client.get_label_maker(r, &mut rng, dry_run)?;
                    maker.make(&profile)?;
                }
            }
            Command::Fetch {
                outfile,
                profile,
                repository,
            } => {
                let mut repo_parser = RepoParser::new(&client);
                let repo = match repository {
                    Some(s) => repo_parser.parse(&s)?,
                    None => repo_parser.default()?,
                };
                log::debug!("Fetching current labels for {repo} ...");
                let labels = Repository::new(&client, repo).get_labels()?;
                let cfg = Config::from_labels(profile, labels);
                cfg.dump(outfile)?;
            }
            Command::Make(make) => make.run(client)?,
        }
        Ok(())
    }
}

/// Create or update a single label
#[derive(Args, Clone, Debug, PartialEq)]
// See <https://jwodder.github.io/kbits/posts/clap-bool-negate/> for what's
// going on with the no-* options
struct Make {
    /// The label's color.
    ///
    /// Colors can be specified as a hex RGB string "#rrggbb" (with or without
    /// leading #) or as CSS color names.
    ///
    /// This option can be specified multiple times, in which case one of the
    /// given colors will be picked at random when creating the label, and no
    /// change will be made to the label color when updating the label.
    ///
    /// Defaults to a random selection from a built-in list.
    #[arg(short = 'c', long)]
    color: Option<Vec<Color>>,

    /// Create the label if it does not already exist [default]
    #[arg(long = "create", overrides_with = "create")]
    _no_create: bool,

    /// Do not create the label
    #[arg(long = "no-create", action = ArgAction::SetFalse)]
    create: bool,

    /// The label's description
    #[arg(short = 'd', long)]
    description: Option<Description>,

    /// Do not change anything in GitHub, but do emit log messages showing what
    /// would be changed.
    #[arg(long)]
    dry_run: bool,

    /// Rename an extant label if its name differs in case from the name given
    /// on the command line [default]
    #[arg(long = "enforce-case", overrides_with = "enforce_case")]
    _no_enforce_case: bool,

    /// Do not rename an extant label if its name differs in case from the name
    /// given on the command line
    #[arg(long = "no-enforce-case", action = ArgAction::SetFalse)]
    enforce_case: bool,

    /// Specify what to do if the label exists and one or more --rename-from
    /// labels also exist.
    #[arg(
        long,
        value_enum,
        default_value_t,
        value_name = "ignore|warn|error",
        ignore_case = true
    )]
    on_rename_clash: OnRenameClash,

    /// If the given label exists, rename it to the name given on the command
    /// line.
    ///
    /// This option can be specified multiple times.  If multiple --rename-from
    /// labels exist, an error will occur.
    #[arg(long, value_name = "LABEL")]
    rename_from: Vec<LabelName>,

    /// The GitHub repository to operate on.
    ///
    /// The repository can be specified in the form `OWNER/NAME` (or, when
    /// `OWNER` is the authenticated user, just `NAME`) or as a GitHub
    /// repository URL.
    ///
    /// If not specified, then the GitHub repository for the local Git
    /// repository is used.
    #[arg(short = 'R', long)]
    repository: Option<String>,

    /// Update the label if its color and/or description do not match the
    /// values given on the command line [default]
    #[arg(long = "update", overrides_with = "update")]
    _no_update: bool,

    /// Do not update the label's color or description
    #[arg(long = "no-update", action = ArgAction::SetFalse)]
    update: bool,

    /// Name of the label
    name: LabelName,
}

impl Make {
    fn run(self, client: GitHub) -> anyhow::Result<()> {
        let MakeProfile {
            repository,
            dry_run,
            profile,
        } = self.into_profile()?;
        let mut repo_parser = RepoParser::new(&client);
        let repo = match repository {
            Some(s) => repo_parser.parse(&s)?,
            None => repo_parser.default()?,
        };
        client
            .get_label_maker(repo, rand::rng(), dry_run)?
            .make(&profile)?;
        Ok(())
    }

    fn into_profile(self) -> Result<MakeProfile, ProfileError> {
        let color = match self.color {
            None => ColorSpec::default(),
            Some(mut cs) if cs.len() == 1 => {
                ColorSpec::Fixed(cs.pop().expect("Vec of length 1 should pop a value"))
            }
            Some(cs) => ColorSpec::Random(cs),
        };
        let options = LabelOptions {
            color,
            description: self.description,
            create: self.create,
            update: self.update,
            on_rename_clash: self.on_rename_clash,
            enforce_case: self.enforce_case,
        };
        let spec = LabelSpec::new(self.name, self.rename_from, options)?;
        let profile = Profile::new("make", [spec])?;
        Ok(MakeProfile {
            repository: self.repository,
            dry_run: self.dry_run,
            profile,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
struct MakeProfile {
    repository: Option<String>,
    dry_run: bool,
    profile: Profile,
}

#[derive(Clone, Debug)]
struct RepoParser<'a> {
    client: &'a GitHub,
    whoami: Option<String>,
}

impl<'a> RepoParser<'a> {
    fn new(client: &'a GitHub) -> RepoParser<'a> {
        RepoParser {
            client,
            whoami: None,
        }
    }

    fn whoami(&mut self) -> anyhow::Result<&str> {
        if self.whoami.is_none() {
            self.whoami = Some(
                self.client
                    .whoami()
                    .context("unable to determine authenticated user's login name")?,
            );
        }
        Ok(self.whoami.as_deref().expect("whoami should be Some now"))
    }

    fn parse(&mut self, s: &str) -> anyhow::Result<GHRepo> {
        if is_valid_name(s) {
            GHRepo::new(self.whoami()?, s).map_err(Into::into)
        } else {
            s.parse().map_err(Into::into)
        }
    }

    fn default(&self) -> anyhow::Result<GHRepo> {
        LocalRepo::for_cwd()?
            .github_remote("origin")
            .context("unable to determine remote GitHub repository for local Git repository")
    }
}

fn main() -> ExitCode {
    if let Err(e) = Arguments::parse().run() {
        if let Some(RequestError::Status(stat)) = e.downcast_ref() {
            log::error!("{stat:#}");
        } else if let Some(LabelMakerError::Request(RequestError::Status(stat))) = e.downcast_ref()
        {
            log::error!("{stat:#}");
        } else {
            log::error!("{e:?}");
        }
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}

fn init_logging(log_level: LevelFilter) {
    let stderr: Box<dyn io::Write + Send> = Box::new(AutoStream::auto(io::stderr()));
    fern::Dispatch::new()
        .format(|out, message, record| {
            use AnsiColor::*;
            let style = match record.level() {
                Level::Error => Style::new().fg_color(Some(Red.into())),
                Level::Warn => Style::new().fg_color(Some(Yellow.into())),
                Level::Info => Style::new().bold(),
                Level::Debug => Style::new().fg_color(Some(Cyan.into())),
                Level::Trace => Style::new().fg_color(Some(Green.into())),
            };
            out.finish(format_args!(
                "{}[{:<5}] {}{}",
                style.render(),
                record.level(),
                message,
                style.render_reset(),
            ));
        })
        .level(LevelFilter::Info)
        .level_for("labelmaker", log_level)
        .level_for("minigh", log_level)
        .chain(stderr)
        .apply()
        .expect("no other logger should have been previously initialized");
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::CommandFactory;

    #[test]
    fn validate_cli() {
        Arguments::command().debug_assert();
    }

    mod cli_make {
        use super::*;
        use assert_matches::assert_matches;
        use rstest::rstest;

        #[test]
        fn no_opts() {
            let args = Arguments::try_parse_from(["arg0", "make", "labelname"]).unwrap();
            let Arguments {
                command: Command::Make(make),
                ..
            } = args
            else {
                panic!("`make` command not mapped to Command::Make");
            };
            assert_eq!(make.color, None);
            assert!(make.create);
            assert_eq!(make.description, None);
            assert!(!make.dry_run);
            assert!(make.enforce_case);
            assert_eq!(make.on_rename_clash, OnRenameClash::Warn);
            assert!(make.rename_from.is_empty());
            assert_eq!(make.repository, None);
            assert!(make.update);
            assert_eq!(make.name, "labelname");
            let MakeProfile {
                repository,
                dry_run,
                profile,
            } = make.into_profile().unwrap();
            assert_eq!(repository, None);
            assert!(!dry_run);
            assert_eq!(profile.name(), "make");
            assert_eq!(profile.specs().len(), 1);
            assert_eq!(profile.specs()[0].name(), "labelname");
            assert!(profile.specs()[0].rename_from().is_empty());
            assert_eq!(
                profile.specs()[0].options(),
                &LabelOptions {
                    color: ColorSpec::default(),
                    description: None,
                    create: true,
                    update: true,
                    on_rename_clash: OnRenameClash::Warn,
                    enforce_case: true,
                }
            );
        }

        #[test]
        fn true_opts() {
            let args = Arguments::try_parse_from([
                "arg0",
                "make",
                "--create",
                "--update",
                "--enforce-case",
                "labelname",
                "--dry-run",
            ])
            .unwrap();
            let Arguments {
                command: Command::Make(make),
                ..
            } = args
            else {
                panic!("`make` command not mapped to Command::Make");
            };
            assert_eq!(make.color, None);
            assert!(make.create);
            assert_eq!(make.description, None);
            assert!(make.dry_run);
            assert!(make.enforce_case);
            assert_eq!(make.on_rename_clash, OnRenameClash::Warn);
            assert!(make.rename_from.is_empty());
            assert_eq!(make.repository, None);
            assert!(make.update);
            assert_eq!(make.name, "labelname");
            let MakeProfile {
                repository,
                dry_run,
                profile,
            } = make.into_profile().unwrap();
            assert_eq!(repository, None);
            assert!(dry_run);
            assert_eq!(profile.name(), "make");
            assert_eq!(profile.specs().len(), 1);
            assert_eq!(profile.specs()[0].name(), "labelname");
            assert!(profile.specs()[0].rename_from().is_empty());
            assert_eq!(
                profile.specs()[0].options(),
                &LabelOptions {
                    color: ColorSpec::default(),
                    description: None,
                    create: true,
                    update: true,
                    on_rename_clash: OnRenameClash::Warn,
                    enforce_case: true,
                }
            );
        }

        #[test]
        fn false_opts() {
            let args = Arguments::try_parse_from([
                "arg0",
                "make",
                "--no-create",
                "--no-update",
                "--no-enforce-case",
                "labelname",
            ])
            .unwrap();
            let Arguments {
                command: Command::Make(make),
                ..
            } = args
            else {
                panic!("`make` command not mapped to Command::Make");
            };
            assert_eq!(make.color, None);
            assert!(!make.create);
            assert_eq!(make.description, None);
            assert!(!make.dry_run);
            assert!(!make.enforce_case);
            assert_eq!(make.on_rename_clash, OnRenameClash::Warn);
            assert!(make.rename_from.is_empty());
            assert_eq!(make.repository, None);
            assert!(!make.update);
            assert_eq!(make.name, "labelname");
            let MakeProfile {
                repository,
                dry_run,
                profile,
            } = make.into_profile().unwrap();
            assert_eq!(repository, None);
            assert!(!dry_run);
            assert_eq!(profile.name(), "make");
            assert_eq!(profile.specs().len(), 1);
            assert_eq!(profile.specs()[0].name(), "labelname");
            assert!(profile.specs()[0].rename_from().is_empty());
            assert_eq!(
                profile.specs()[0].options(),
                &LabelOptions {
                    color: ColorSpec::default(),
                    description: None,
                    create: false,
                    update: false,
                    on_rename_clash: OnRenameClash::Warn,
                    enforce_case: false,
                }
            );
        }

        #[test]
        fn one_color() {
            let args =
                Arguments::try_parse_from(["arg0", "make", "-c", "red", "labelname"]).unwrap();
            let Arguments {
                command: Command::Make(make),
                ..
            } = args
            else {
                panic!("`make` command not mapped to Command::Make");
            };
            assert_eq!(make.color, Some(vec!["red".parse().unwrap()]));
            assert!(make.create);
            assert_eq!(make.description, None);
            assert!(!make.dry_run);
            assert!(make.enforce_case);
            assert_eq!(make.on_rename_clash, OnRenameClash::Warn);
            assert!(make.rename_from.is_empty());
            assert_eq!(make.repository, None);
            assert!(make.update);
            assert_eq!(make.name, "labelname");
            let MakeProfile {
                repository,
                dry_run,
                profile,
            } = make.into_profile().unwrap();
            assert_eq!(repository, None);
            assert!(!dry_run);
            assert_eq!(profile.name(), "make");
            assert_eq!(profile.specs().len(), 1);
            assert_eq!(profile.specs()[0].name(), "labelname");
            assert!(profile.specs()[0].rename_from().is_empty());
            assert_eq!(
                profile.specs()[0].options(),
                &LabelOptions {
                    color: ColorSpec::Fixed("red".parse().unwrap()),
                    description: None,
                    create: true,
                    update: true,
                    on_rename_clash: OnRenameClash::Warn,
                    enforce_case: true,
                }
            );
        }

        #[test]
        fn many_colors() {
            let args = Arguments::try_parse_from([
                "arg0",
                "make",
                "-c",
                "red",
                "--color",
                "green",
                "-cblue",
                "labelname",
            ])
            .unwrap();
            let Arguments {
                command: Command::Make(make),
                ..
            } = args
            else {
                panic!("`make` command not mapped to Command::Make");
            };
            assert_eq!(
                make.color,
                Some(vec![
                    "red".parse().unwrap(),
                    "green".parse().unwrap(),
                    "blue".parse().unwrap()
                ])
            );
            assert!(make.create);
            assert_eq!(make.description, None);
            assert!(!make.dry_run);
            assert!(make.enforce_case);
            assert_eq!(make.on_rename_clash, OnRenameClash::Warn);
            assert!(make.rename_from.is_empty());
            assert_eq!(make.repository, None);
            assert!(make.update);
            assert_eq!(make.name, "labelname");
            let MakeProfile {
                repository,
                dry_run,
                profile,
            } = make.into_profile().unwrap();
            assert_eq!(repository, None);
            assert!(!dry_run);
            assert_eq!(profile.name(), "make");
            assert_eq!(profile.specs().len(), 1);
            assert_eq!(profile.specs()[0].name(), "labelname");
            assert!(profile.specs()[0].rename_from().is_empty());
            assert_eq!(
                profile.specs()[0].options(),
                &LabelOptions {
                    color: ColorSpec::Random(vec![
                        "red".parse().unwrap(),
                        "green".parse().unwrap(),
                        "blue".parse().unwrap()
                    ]),
                    description: None,
                    create: true,
                    update: true,
                    on_rename_clash: OnRenameClash::Warn,
                    enforce_case: true,
                }
            );
        }

        #[rstest]
        #[case("ignore", OnRenameClash::Ignore)]
        #[case("warn", OnRenameClash::Warn)]
        #[case("error", OnRenameClash::Error)]
        #[case("IGNORE", OnRenameClash::Ignore)]
        #[case("WARN", OnRenameClash::Warn)]
        #[case("ERROR", OnRenameClash::Error)]
        fn on_rename_clash(#[case] arg: &str, #[case] value: OnRenameClash) {
            let args =
                Arguments::try_parse_from(["arg0", "make", "--on-rename-clash", arg, "labelname"])
                    .unwrap();
            let Arguments {
                command: Command::Make(make),
                ..
            } = args
            else {
                panic!("`make` command not mapped to Command::Make");
            };
            assert_eq!(make.color, None);
            assert!(make.create);
            assert_eq!(make.description, None);
            assert!(!make.dry_run);
            assert!(make.enforce_case);
            assert_eq!(make.on_rename_clash, value);
            assert!(make.rename_from.is_empty());
            assert_eq!(make.repository, None);
            assert!(make.update);
            assert_eq!(make.name, "labelname");
            let MakeProfile {
                repository,
                dry_run,
                profile,
            } = make.into_profile().unwrap();
            assert_eq!(repository, None);
            assert!(!dry_run);
            assert_eq!(profile.name(), "make");
            assert_eq!(profile.specs().len(), 1);
            assert_eq!(profile.specs()[0].name(), "labelname");
            assert!(profile.specs()[0].rename_from().is_empty());
            assert_eq!(
                profile.specs()[0].options(),
                &LabelOptions {
                    color: ColorSpec::default(),
                    description: None,
                    create: true,
                    update: true,
                    on_rename_clash: value,
                    enforce_case: true,
                }
            );
        }

        #[test]
        fn one_rename_from() {
            let args =
                Arguments::try_parse_from(["arg0", "make", "--rename-from", "bar", "labelname"])
                    .unwrap();
            let Arguments {
                command: Command::Make(make),
                ..
            } = args
            else {
                panic!("`make` command not mapped to Command::Make");
            };
            assert_eq!(make.color, None);
            assert!(make.create);
            assert_eq!(make.description, None);
            assert!(!make.dry_run);
            assert!(make.enforce_case);
            assert_eq!(make.on_rename_clash, OnRenameClash::Warn);
            assert_eq!(make.rename_from, ["bar"]);
            assert_eq!(make.repository, None);
            assert!(make.update);
            assert_eq!(make.name, "labelname");
            let MakeProfile {
                repository,
                dry_run,
                profile,
            } = make.into_profile().unwrap();
            assert_eq!(repository, None);
            assert!(!dry_run);
            assert_eq!(profile.name(), "make");
            assert_eq!(profile.specs().len(), 1);
            assert_eq!(profile.specs()[0].name(), "labelname");
            assert_eq!(profile.specs()[0].rename_from(), ["bar"]);
            assert_eq!(
                profile.specs()[0].options(),
                &LabelOptions {
                    color: ColorSpec::default(),
                    description: None,
                    create: true,
                    update: true,
                    on_rename_clash: OnRenameClash::Warn,
                    enforce_case: true,
                }
            );
        }

        #[test]
        fn many_rename_from() {
            let args = Arguments::try_parse_from([
                "arg0",
                "make",
                "--rename-from",
                "bar",
                "--rename-from",
                "baz",
                "labelname",
                "--rename-from",
                "quux",
            ])
            .unwrap();
            let Arguments {
                command: Command::Make(make),
                ..
            } = args
            else {
                panic!("`make` command not mapped to Command::Make");
            };
            assert_eq!(make.color, None);
            assert!(make.create);
            assert_eq!(make.description, None);
            assert!(!make.dry_run);
            assert!(make.enforce_case);
            assert_eq!(make.on_rename_clash, OnRenameClash::Warn);
            assert_eq!(make.rename_from, ["bar", "baz", "quux"]);
            assert_eq!(make.repository, None);
            assert!(make.update);
            assert_eq!(make.name, "labelname");
            let MakeProfile {
                repository,
                dry_run,
                profile,
            } = make.into_profile().unwrap();
            assert_eq!(repository, None);
            assert!(!dry_run);
            assert_eq!(profile.name(), "make");
            assert_eq!(profile.specs().len(), 1);
            assert_eq!(profile.specs()[0].name(), "labelname");
            assert_eq!(profile.specs()[0].rename_from(), ["bar", "baz", "quux"]);
            assert_eq!(
                profile.specs()[0].options(),
                &LabelOptions {
                    color: ColorSpec::default(),
                    description: None,
                    create: true,
                    update: true,
                    on_rename_clash: OnRenameClash::Warn,
                    enforce_case: true,
                }
            );
        }

        #[test]
        fn duplicate_rename_from() {
            let args = Arguments::try_parse_from([
                "arg0",
                "make",
                "--rename-from",
                "bar",
                "--rename-from",
                "baz",
                "labelname",
                "--rename-from",
                "Bar",
            ])
            .unwrap();
            let Arguments {
                command: Command::Make(make),
                ..
            } = args
            else {
                panic!("`make` command not mapped to Command::Make");
            };
            assert_eq!(make.color, None);
            assert!(make.create);
            assert_eq!(make.description, None);
            assert!(!make.dry_run);
            assert!(make.enforce_case);
            assert_eq!(make.on_rename_clash, OnRenameClash::Warn);
            assert_eq!(make.rename_from, ["bar", "baz", "Bar"]);
            assert_eq!(make.repository, None);
            assert!(make.update);
            assert_eq!(make.name, "labelname");
            let MakeProfile {
                repository,
                dry_run,
                profile,
            } = make.into_profile().unwrap();
            assert_eq!(repository, None);
            assert!(!dry_run);
            assert_eq!(profile.name(), "make");
            assert_eq!(profile.specs().len(), 1);
            assert_eq!(profile.specs()[0].name(), "labelname");
            assert_eq!(profile.specs()[0].rename_from(), ["bar", "baz"]);
            assert_eq!(
                profile.specs()[0].options(),
                &LabelOptions {
                    color: ColorSpec::default(),
                    description: None,
                    create: true,
                    update: true,
                    on_rename_clash: OnRenameClash::Warn,
                    enforce_case: true,
                }
            );
        }

        #[test]
        fn rename_from_self() {
            let args = Arguments::try_parse_from([
                "arg0",
                "make",
                "--rename-from",
                "Labelname",
                "labelname",
            ])
            .unwrap();
            let Arguments {
                command: Command::Make(make),
                ..
            } = args
            else {
                panic!("`make` command not mapped to Command::Make");
            };
            assert_eq!(make.color, None);
            assert!(make.create);
            assert_eq!(make.description, None);
            assert!(!make.dry_run);
            assert!(make.enforce_case);
            assert_eq!(make.on_rename_clash, OnRenameClash::Warn);
            assert_eq!(make.rename_from, ["Labelname"]);
            assert_eq!(make.repository, None);
            assert!(make.update);
            assert_eq!(make.name, "labelname");
            let r = make.into_profile();
            assert_matches!(r, Err(ProfileError::Spec(LabelSpecError::SelfRename(name))) => {
                assert_eq!(name, "labelname");
            });
        }

        #[test]
        fn repository() {
            let args = Arguments::try_parse_from([
                "arg0",
                "make",
                "-R",
                "octocat/hello-world",
                "labelname",
            ])
            .unwrap();
            let Arguments {
                command: Command::Make(make),
                ..
            } = args
            else {
                panic!("`make` command not mapped to Command::Make");
            };
            assert_eq!(make.color, None);
            assert!(make.create);
            assert_eq!(make.description, None);
            assert!(!make.dry_run);
            assert!(make.enforce_case);
            assert_eq!(make.on_rename_clash, OnRenameClash::Warn);
            assert!(make.rename_from.is_empty());
            assert_eq!(make.repository, Some(String::from("octocat/hello-world")));
            assert!(make.update);
            assert_eq!(make.name, "labelname");
            let MakeProfile {
                repository,
                dry_run,
                profile,
            } = make.into_profile().unwrap();
            assert_eq!(repository, Some(String::from("octocat/hello-world")));
            assert!(!dry_run);
            assert_eq!(profile.name(), "make");
            assert_eq!(profile.specs().len(), 1);
            assert_eq!(profile.specs()[0].name(), "labelname");
            assert!(profile.specs()[0].rename_from().is_empty());
            assert_eq!(
                profile.specs()[0].options(),
                &LabelOptions {
                    color: ColorSpec::default(),
                    description: None,
                    create: true,
                    update: true,
                    on_rename_clash: OnRenameClash::Warn,
                    enforce_case: true,
                }
            );
        }
    }
}
