mod client;
mod config;
mod labels;
mod util;

use crate::client::{GitHub, Repository};
use crate::config::Config;
use anstream::AutoStream;
use anstyle::{AnsiColor, Style};
use anyhow::Context;
use clap::{Parser, Subcommand};
use ghrepo::{GHRepo, LocalRepo};
use log::{Level, LevelFilter};
use std::io;
use std::path::PathBuf;

/// Create & enforce sets of labels in GitHub repositories
///
/// See <https://github.com/jwodder/labelmaker> for more information
#[derive(Clone, Debug, Eq, Parser, PartialEq)]
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

#[derive(Clone, Debug, Eq, PartialEq, Subcommand)]
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
    /// List a repository's labels as a configuration file
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
        /// `OWNER` is the authenticating user, just `NAME`) and/or as a GitHub
        /// repository URL.
        ///
        /// If not specified, then the GitHub repository for the local Git
        /// repository is used.
        repository: Option<String>,
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
                    log::info!("Applying profile {:?} to repository {}", profile.name, r);
                    let mut maker = client.get_label_maker(r, &mut rng, dry_run).await?;
                    maker.make(&profile.specs).await?;
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
        }
        Ok(())
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> anyhow::Result<()> {
    let Arguments { log_level, command } = Arguments::parse();
    init_logging(log_level);
    let token = gh_token::get().context("unable to fetch GitHub access token")?;
    let client = GitHub::new(&token)?;
    command.run(client).await
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
