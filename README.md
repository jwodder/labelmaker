[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CI Status](https://github.com/jwodder/labelmaker/actions/workflows/test.yml/badge.svg)](https://github.com/jwodder/labelmaker/actions/workflows/test.yml)
[![codecov.io](https://codecov.io/gh/jwodder/labelmaker/branch/master/graph/badge.svg)](https://codecov.io/gh/jwodder/labelmaker)
[![Minimum Supported Rust Version](https://img.shields.io/badge/MSRV-1.70-orange)](https://www.rust-lang.org)
[![MIT License](https://img.shields.io/github/license/jwodder/labelmaker.svg)](https://opensource.org/licenses/MIT)

[GitHub](https://github.com/jwodder/labelmaker) | [crates.io](https://crates.io/crates/labelmaker) | [Issues](https://github.com/jwodder/labelmaker/issues) | [Changelog](https://github.com/jwodder/labelmaker/blob/master/CHANGELOG.md)

`labelmaker` is a Rust program for batch creation of [labels][] in GitHub
repositories, including updating and/or renaming existing labels to meet your
specifications.  Simply describe your desired labels in a configuration file
and point `labelmaker` in the direction of your repositories.

[labels]: https://docs.github.com/en/issues/using-labels-and-milestones-to-track-work/managing-labels

Installation
============

In order to install `labelmaker`, you first need to have [Rust and Cargo
installed](https://www.rust-lang.org/tools/install).  You can then build the
latest release of `labelmaker` and install it in `~/.cargo/bin` by running:

    cargo install labelmaker

Usage
=====


    labelmaker [<global options>] <subcommand> ...

The `labelmaker` command has the following subcommands, each detailed below:

- `apply` — Apply a set of label specifications to one or more GitHub
  repositories

- `fetch` — Dump a repository's labels as a configuration file

- `make` — Create or update a single label

Each subcommand takes an argument or option for indicating what GitHub
repositories to operate on.  A repository can be specified in the form
`OWNER/NAME` (or, when `OWNER` is the authenticated user, just `NAME`) or as a
GitHub repository URL.  If no repository is supplied, then the current
directory must belong to a Git repository whose `origin` remote points to a
GitHub repository; `labelmaker` will operate on this remote repository.

Global Options
--------------

- `-l <level>`, `--log-level <level>` — Set the log level to the given value.
  Possible values are "`OFF`", "`ERROR`", "`WARN`", "`INFO`", "`DEBUG`", and
  "`TRACE`" (all case-insensitive).  [default value: `INFO`]


Authentication
--------------

`labelmaker` requires a GitHub access token with appropriate permissions in
order to run.  Specify the token via the `GH_TOKEN` or `GITHUB_TOKEN`
environment variable or store it with the [`gh`](https://github.com/cli/cli)
command.

Note that, if `gh` has stored the token in a system keyring, you may be
prompted to unlock the keyring.


`labelmaker apply`
------------------

    labelmaker [<global options>] apply [<options>] <config-file> [<repository> ...]

`labelmaker apply` takes a path to a configuration file (See "[Configuration
File](#configuration-file)" below) and a list of GitHub repositories as
arguments.  It then creates and/or updates the labels of each repository based
on the specification in the configuration file.

For each repository, all changes are calculated before modifying anything, so
if an error occurs based on the state of the configuration file and/or current
repository labels, it will be caught before any changes to the repository are
made.

### Options

- `--dry-run` — Do not change anything in GitHub, but do emit log messages
  showing what would be changed.

- `-F FILE`/`--repo-file FILE` — Also operate on all repositories listed in the
  given file.  Repositories must be listed one per line.  Leading & trailing
  whitespace is ignored.  Blank lines and lines starting with `#` are skipped.

- `-P NAME`/`--profile NAME` — Specify which profile in the configuration file
  to use.  Defaults to the value of `defaults.profile` in the configuration
  file, or to `default`.


`labelmaker fetch`
------------------

    labelmaker [<global options>] fetch [<options>] [<repository>]

`labelmaker fetch` fetches the labels currently defined for the given GitHub
repository and dumps them as a `labelmaker` configuration file, ready for input
into `labelmaker apply`.

The generated configuration file lists only label names, colors, and
descriptions, no defaults, aside from the file-wide `color` setting having its
default value included for use as a reference.

### Options

- `-o FILE`/`--outfile FILE` — Output the configuration to the given file.  By
  default, output is written to standard output, which can also be selected by
  supplying `-` as the outfile name.

  The format (JSON, JSON5, TOML, or YAML) of the output is determined based on
  the file's extension.  When outputting to standard output, JSON is produced.

- `-P NAME`/`--profile NAME` — Set the name of the profile to place the labels
  under in the generated configuration file.  The configuration file's default
  profile will also be set to this value.  [default: `default`]


`labelmaker make`
-----------------

    labelmaker [<global options>] make [<options>] <label-name>

`labelmaker make` creates or updates the label with the given name in a GitHub
repository, the same as if `labelmaker apply` had been run on that repository
with a configuration profile containing a single label entry.

### Options

- `-c COLOR`/`--color COLOR` — Specify the label's color.  Colors are specified
  using the same formats as in the configuration file.

  This option can be specified multiple times, in which case one of the given
  colors will be picked at random when creating the label, and no change will
  be made to the label color when updating the label.

  The color defaults to a random selection from the same built-in list as used
  by the configuration file and `apply`.

- `--create`/`--no-create` — Whether to create the label if it doesn't already
  exist [default: `--create`]

- `-d TEXT`, `--description TEXT` — Specify the label's description.

- `--dry-run` — Do not change anything in GitHub, but do emit log messages
  showing what would be changed.

- `--enforce-case`/`--no-enforce-case` — Whether to rename an extant label if
  its name differs in case from the name given on the command line [default:
  `--enforce-case`]

- `--on-rename-clash <ignore|warn|error>` — Specify what to do if the label
  exists and one or more `--rename-from` labels also exist:
    - `ignore`: Do nothing
    - `warn` *(default)*: Emit a warning
    - `error`:  Fail with an error

- `--rename-from LABEL` — If `LABEL` exists, rename it to the label name
  provided as the argument to `make`.

  This option can be specified multiple times.  If multiple `--rename-from`
  labels exist, an error will occur.

- `-R REPO`/`--repository REPO` — Specify the GitHub repository to operate on.

- `--update`/`--no-update` — Whether to update the label if its color and/or
  description do not match the values given on the command line [default:
  `--update`]


Configuration File
------------------

`labelmaker`'s configuration file may be written in [JSON][], [JSON5][],
[TOML][], or [YAML][]; the file type is determined automatically based on the
file extension.  The file contains a top-level mapping with the following
fields:

[JSON]: https://www.json.org
[JSON5]: https://json5.org
[TOML]: https://toml.io
[YAML]: https://yaml.org

- `defaults` — A mapping of default label settings to apply to all labels in
  this configuration file.  Any field that can be set on a label can be set
  here, other than `name` and `rename-from`.  File-wide defaults can be
  overridden for specific profiles via the `profiles.*.defaults` mappings.

    - The `defaults` mapping may also contain a `profile` string field
      specifying the default profile for `apply` to use when no `--profile`
      option is given; the default profile is `default`.

- `profiles` — A mapping from profile names to profile definitions.  A
  *profile* is a set of label definitions that can be selected when invoking
  `apply`.  Each profile is defined by a mapping with the following fields:

    - `defaults` — A mapping of default label settings to apply to all labels
      in this profile.  Any field that can be set on a label can be set here,
      other than `name` and `rename-from`.  Settings set here override settings
      set in the top-level `defaults` mapping for the labels in this profile.

    - `labels` — A list of label specifications; each one is a mapping with the
      following fields:

        - `name` *(required)* — The name of the label.  Leading & trailing
          whitespace will be trimmed; if the resulting string is empty, is it
          an error.

            - Note that GitHub treats label names case-insensitively; thus, the
              names "foo" and "Foo" refer to the same label.  If you do or do
              not want your label names to enforce a specific casing, see the
              `enforce-case` option below.

            - It is an error if two or more labels in the same profile have the
              same name after case-folding.

        - `color` — The color to use for the label.  Colors can be specified as
          a hex RGB string `"#rrggbb"` (with or without leading `#`) or as [CSS
          color names][].  Alternatively, `color` may be set to a list of
          colors, in which case one of the colors will be picked at random when
          creating the label, and no change will be made to the label color
          when updating the label.  An empty list is equivalent to
          `["#000000"]`.

          The default `color` value is a list of the default colors displayed
          when creating a new label in the GitHub web UI as of 2023-09-24.

        - `description` — The description to apply to the label.  If this is
          not set, the description will be empty when creating the label, and
          no change will be made to the description when updating the label.

        - `create` (boolean; default: `true`) — If true, the label does not
          already exist, and no label specified in `rename-from` exists (See
          below), then the label will be created in GitHub.

        - `update` (boolean; default: `true`) — If true, the label exists (or a
          label specified in `rename-from` is being renamed to it), and the
          color and/or description of the pre-existing label differs from the
          value given in the configuration file, update the fields that differ.

        - `enforce-case` (boolean; default: `true`) — If true and the label
          exists but the name of the label in GitHub differs from the name in
          `name` when compared case-sensitively, update the label to use the
          casing specified in the configuration file.

        - `rename-from` — A list of label names; if the label given in the
          `name` field does not exist in the repository, but one of the labels
          in `rename-from` does, then the existing label will be renamed to the
          given `name`.  If multiple labels in `rename-from` exist, an error
          will occur.

            - If the label given in `name` does exist and so do one or more
              labels in `rename-from`, a warning is emitted by default; see the
              `on-rename-clash` option below.

            - It is an error if a label includes its own name in `rename-from`.

            - It is an error if a label specified by a profile is also in the
              `rename-from` list of another label in the same profile.

            - It is an error if a label is listed in the `rename-from` fields
              of two or more labels in the same profile.

        - `on-rename-clash` — Specify what to do if the label exists and one or
          more labels listed in `rename-from` also exist.  The possible values
          are:

            - `"ignore"` — Do nothing.
            - `"warn"` *(default)* — Emit a warning.
            - `"error"` — Fail with an error.

[CSS color names]: https://www.w3.org/TR/css-color-4/#named-colors

### Example Configuration File

The following TOML configuration shows the default GitHub labels as of
2023-10-18, along with the default value of the `color` setting:

```toml
[defaults]
color = [
    "0052cc",
    "006b75",
    "0e8a16",
    "1d76db",
    "5319e7",
    "b60205",
    "bfd4f2",
    "bfdadc",
    "c2e0c6",
    "c5def5",
    "d4c5f9",
    "d93f0b",
    "e99695",
    "f9d0c4",
    "fbca04",
    "fef2c0",
]

[[profiles.default.labels]]
name = "bug"
color = "d73a4a"
description = "Something isn't working"

[[profiles.default.labels]]
name = "documentation"
color = "0075ca"
description = "Improvements or additions to documentation"

[[profiles.default.labels]]
name = "duplicate"
color = "cfd3d7"
description = "This issue or pull request already exists"

[[profiles.default.labels]]
name = "enhancement"
color = "a2eeef"
description = "New feature or request"

[[profiles.default.labels]]
name = "good first issue"
color = "7057ff"
description = "Good for newcomers"

[[profiles.default.labels]]
name = "help wanted"
color = "008672"
description = "Extra attention is needed"

[[profiles.default.labels]]
name = "invalid"
color = "e4e669"
description = "This doesn't seem right"

[[profiles.default.labels]]
name = "question"
color = "d876e3"
description = "Further information is requested"

[[profiles.default.labels]]
name = "wontfix"
color = "ffffff"
description = "This will not be worked on"
```

That same configuration, in YAML:

```yaml
defaults:
  color:
    - "0052cc"
    - "006b75"
    - "0e8a16"
    - "1d76db"
    - "5319e7"
    - "b60205"
    - "bfd4f2"
    - "bfdadc"
    - "c2e0c6"
    - "c5def5"
    - "d4c5f9"
    - "d93f0b"
    - "e99695"
    - "f9d0c4"
    - "fbca04"
    - "fef2c0"

profiles:
  default:
    labels:
      - name: bug
        color: "d73a4a"
        description: Something isn't working

      - name: documentation
        color: "0075ca"
        description: Improvements or additions to documentation

      - name: duplicate
        color: "cfd3d7"
        description: This issue or pull request already exists

      - name: enhancement
        color: "a2eeef"
        description: New feature or request

      - name: good first issue
        color: "7057ff"
        description: Good for newcomers

      - name: help wanted
        color: "008672"
        description: Extra attention is needed

      - name: invalid
        color: "e4e669"
        description: This doesn't seem right

      - name: question
        color: "d876e3"
        description: Further information is requested

      - name: wontfix
        color: "ffffff"
        description: This will not be worked on
```
