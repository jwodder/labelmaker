mod client;
mod config;
mod labels;

use crate::client::{GitHub, Repository};
use crate::config::Config;
use crate::labels::*;
use anstream::AutoStream;
use anstyle::{AnsiColor, Style};
use anyhow::Context;
use clap::{Parser, Subcommand};
use csscolorparser::Color;
use ghrepo::{GHRepo, LocalRepo};
use log::{Level, LevelFilter};
use std::io;
use std::path::PathBuf;
use std::process::ExitCode;

/// Create & enforce sets of labels in GitHub repositories
///
/// See <https://github.com/jwodder/labelmaker> for more information
#[derive(Clone, Debug, Parser, PartialEq)]
#[clap(version)]
struct Arguments {
    /// Set logging level
    #[clap(
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
    async fn run(self) -> anyhow::Result<()> {
        init_logging(self.log_level);
        let token = gh_token::get().context("unable to fetch GitHub access token")?;
        let client = GitHub::new(&token)?;
        self.command.run(client).await
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

        /// A configuration file describing what labels to create and/or update
        /// in each repository
        config: PathBuf,

        /// The GitHub repositories to operate on.
        ///
        /// Repositories can be specified in the form `OWNER/NAME` (or, when
        /// `OWNER` is the authenticating user, just `NAME`) and/or as GitHub
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
        #[clap(short, long, default_value_t, hide_default_value = true)]
        outfile: patharg::OutputArg,

        /// Name of the profile to define in the generated configuration file
        #[arg(short = 'P', long, value_name = "NAME", default_value = "default")]
        profile: String,

        /// The GitHub repository to operate on.
        ///
        /// The repository can be specified in the form `OWNER/NAME` (or, when
        /// `OWNER` is the authenticating user, just `NAME`) or as a GitHub
        /// repository URL.
        ///
        /// If not specified, then the GitHub repository for the local Git
        /// repository is used.
        repository: Option<String>,
    },
    /// Create or update a single label
    Make {
        /// The label's color.
        ///
        /// Colors can be specified as a hex RGB string "#rrggbb" (with or
        /// without leading #) or as CSS color names.
        ///
        /// This option can be specified multiple times, in which case one of
        /// the given colors will be picked at random when creating the label,
        /// and no change will be made to the label color when updating the
        /// label.
        ///
        /// Defaults to a random selection from a built-in list.
        #[clap(short = 'c', long)]
        color: Option<Vec<Color>>,

        /// Create the label if it does not already exist [default]
        #[clap(long, overrides_with = "_no_create")]
        create: bool,

        /// Do not create the label
        #[clap(long = "no-create")]
        _no_create: bool,

        /// The label's description
        #[clap(short = 'd', long)]
        description: Option<Description>,

        /// Rename an extant label if its name differs in case from the name
        /// given on the command line [default]
        #[clap(long, overrides_with = "_no_enforce_case")]
        enforce_case: bool,

        /// Do not rename an extant label if its name differs in case from the
        /// name given on the command line
        #[clap(long = "no-enforce-case")]
        _no_enforce_case: bool,

        /// Specify what to do if the label exists and one or more
        /// --rename-from labels also exist.
        #[clap(long, value_enum, default_value_t, value_name = "ignore|warn|error")]
        on_rename_clash: OnRenameClash,

        /// If the given label exists, rename it to the name given on the
        /// command line.
        ///
        /// This option can be specified multiple times.  If multiple
        /// --rename-from labels exist, an error will occur.
        #[clap(long, value_name = "LABEL")]
        rename_from: Vec<LabelName>,

        /// The GitHub repository to operate on.
        ///
        /// The repository can be specified in the form `OWNER/NAME` (or, when
        /// `OWNER` is the authenticating user, just `NAME`) or as a GitHub
        /// repository URL.
        ///
        /// If not specified, then the GitHub repository for the local Git
        /// repository is used.
        #[clap(short = 'R', long)]
        repository: Option<String>,

        /// Update the label if its color and/or description do not match the
        /// values given on the command line [default]
        #[clap(long, overrides_with = "_no_update")]
        update: bool,

        /// Do not update the label's color or description
        #[clap(long = "no-update")]
        _no_update: bool,

        /// Name of the label
        name: LabelName,
    },
}

impl Command {
    async fn run(self, client: GitHub) -> anyhow::Result<()> {
        match self {
            Command::Apply {
                dry_run,
                profile,
                config,
                repository,
            } => {
                let cfg = Config::load(&config).context("error loading config file")?;
                let profile = match profile {
                    Some(p) => cfg.get_profile(&p)?,
                    None => cfg.get_default_profile()?,
                };
                let me = client
                    .whoami()
                    .await
                    .context("unable to determine authenticating user's login name")?;
                let repos = if repository.is_empty() {
                    let r = LocalRepo::for_cwd()?.github_remote("origin").context(
                        "unable to determine remote GitHub repository for local Git repository",
                    )?;
                    vec![r]
                } else {
                    repository
                        .into_iter()
                        .map(|s| GHRepo::from_str_with_owner(&s, &me))
                        .collect::<Result<Vec<_>, _>>()?
                };
                let mut rng = rand::thread_rng();
                for r in repos {
                    log::info!("Applying profile {:?} to repository {}", profile.name(), r);
                    let mut maker = client.get_label_maker(r, &mut rng, dry_run).await?;
                    maker.make(&profile).await?;
                }
            }
            Command::Fetch {
                outfile,
                profile,
                repository,
            } => {
                let repo = match repository {
                    Some(s) => {
                        let me = client
                            .whoami()
                            .await
                            .context("unable to determine authenticating user's login name")?;
                        GHRepo::from_str_with_owner(&s, &me)?
                    }
                    None => LocalRepo::for_cwd()?.github_remote("origin").context(
                        "unable to determine remote GitHub repository for local Git repository",
                    )?,
                };
                log::debug!("Fetching current labels for {repo} ...");
                let labels = Repository::new(&client, repo).get_labels().await?;
                let cfg = Config::from_labels(profile, labels);
                cfg.dump(outfile)
                    .context("failed outputting configuration")?;
            }
            Command::Make { .. } => todo!(),
        }
        Ok(())
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> ExitCode {
    if let Err(e) = Arguments::parse().run().await {
        log::error!("{e:?}");
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
            ))
        })
        .level(LevelFilter::Info)
        .level_for("labelmaker", log_level)
        .chain(stderr)
        .apply()
        .unwrap();
}
