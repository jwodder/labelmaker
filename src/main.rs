#![allow(dead_code)]
#![allow(unused)]
mod client;
mod config;
mod labels;
mod util;

use crate::client::{get_github_token, GitHub};
use crate::config::Config;
use crate::labels::LabelResolution;
use anstream::AutoStream;
use anstyle::{AnsiColor, Style};
use clap::{Parser, Subcommand};
use ghrepo::{GHRepo, LocalRepo};
use log::{Level, LevelFilter};
use std::io;

#[derive(Clone, Debug, Eq, Parser, PartialEq)]
#[clap(version)]
struct Arguments {
    #[clap(
        long,
        value_name = "URL",
        env = "GITHUB_API_URL",
        default_value = "https://api.github.com"
    )]
    api_url: url::Url,

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

        config: patharg::InputArg,

        repository: Vec<String>,
    },
}

impl Command {
    async fn run(self, client: GitHub) {
        match self {
            Command::Apply {
                dry_run,
                profile,
                config,
                repository,
            } => {
                // TODO: Replace unwrap()'s with `?`
                let cfg = Config::load(config).unwrap();
                let profile = match profile {
                    Some(p) => cfg.get_profile(&p).unwrap(),
                    None => cfg.get_default_profile().unwrap(),
                };
                let me = client.whoami().await.unwrap();
                let repos = if repository.is_empty() {
                    let r = LocalRepo::for_cwd()
                        .unwrap()
                        .github_remote("origin")
                        .unwrap();
                    vec![r]
                } else {
                    repository
                        .into_iter()
                        .map(|s| GHRepo::from_str_with_owner(&s, &me).unwrap())
                        .collect::<Vec<_>>()
                };
                for r in repos {
                    log::info!("Applying profile {:?} to repository {}", profile.name, r);
                    let mut maker = client.get_label_maker(r, dry_run).await.unwrap();
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
            }
        }
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let Arguments {
        api_url,
        log_level,
        command,
    } = Arguments::parse();
    init_logging(log_level);
    // TODO: Replace unwrap() with `?`:
    let token = get_github_token(&api_url).unwrap();
    let client = GitHub::new(api_url, &token).unwrap();
    command.run(client).await;
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
