mod client;
mod config;
mod labels;
mod util;

use crate::client::GitHub;
use crate::config::Config;
use crate::labels::LabelResolution;
use anstream::AutoStream;
use anstyle::{AnsiColor, Style};
use anyhow::Context;
use clap::{Parser, Subcommand};
use ghrepo::{GHRepo, LocalRepo};
use log::{Level, LevelFilter};
use std::io;
use std::path::PathBuf;

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
    Apply {
        #[arg(long)]
        dry_run: bool,

        #[arg(short = 'P', long, value_name = "NAME")]
        profile: Option<String>,

        config: PathBuf,

        repository: Vec<String>,
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
                    let mut maker = client.get_label_maker(r, &mut rng, dry_run).await.unwrap();
                    let mut ops = Vec::with_capacity(profile.specs.len());
                    for spec in &profile.specs {
                        for res in maker.resolve(spec).unwrap() {
                            match res {
                                LabelResolution::Operation(op) => ops.push(op),
                                // TODO: Should the warning be delayed to
                                // execution time?
                                LabelResolution::Warning(wrn) => log::warn!("{wrn}"),
                            }
                        }
                    }
                    for op in ops {
                        maker.execute(op).await.unwrap();
                    }
                }
                Ok(())
            }
        }
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
