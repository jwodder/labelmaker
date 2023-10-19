[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CI Status](https://github.com/jwodder/labelmaker/actions/workflows/test.yml/badge.svg)](https://github.com/jwodder/labelmaker/actions/workflows/test.yml)
[![codecov.io](https://codecov.io/gh/jwodder/labelmaker/branch/master/graph/badge.svg)](https://codecov.io/gh/jwodder/labelmaker)
[![Minimum Supported Rust Version](https://img.shields.io/badge/MSRV-1.70-orange)](https://www.rust-lang.org)
[![MIT License](https://img.shields.io/github/license/jwodder/labelmaker.svg)](https://opensource.org/licenses/MIT)

[GitHub](https://github.com/jwodder/labelmaker) | [Issues](https://github.com/jwodder/labelmaker/issues)

`labelmaker` is a Rust program for batch creation of [labels][] in GitHub
repositories, including updating and/or renaming existing labels to meet your
specifications.  Simply describe your desired labels in a configuration file
and point `labelmaker` in the direction of your repositories.

[labels]: https://docs.github.com/en/issues/using-labels-and-milestones-to-track-work/managing-labels

Installation
============

In order to install `labelmaker`, you first need to have [Rust and Cargo
installed](https://www.rust-lang.org/tools/install).  You can then build
`labelmaker` from source and install it in `~/.cargo/bin` by running:

    cargo install --git https://github.com/jwodder/labelmaker

<!--
You can then build the latest release of `labelmaker` and install it in
`~/.cargo/bin` by running:

    cargo install labelmaker
-->

Usage
=====


    labelmaker [<global options>] <subcommand> ...

The `labelmaker` command has one subcommand (for now): `apply` (detailed
below), which applies a set of label descriptions to one or more GitHub
repositories.

Global Options
--------------

- `-l <level>`, `--log-level <level>` — Set the log level to the given value.
  Possible values are "`OFF`", "`ERROR`", "`WARN`", "`INFO`", "`DEBUG`", and
  "`TRACE`" (all case-insensitive).  [default value: `INFO`]


`labelmaker apply`
------------------

    labelmaker [<global options>] apply [<options>] <config-file> [<repository> ...]

`labelmaker apply` takes a path to a configuration file (See "Configuration
File" below) and a list of GitHub repositories as arguments.  It then creates
and/or updates the labels of each repository based on the specification in the
configuration file.

Repositories can be specified in the form `OWNER/NAME` (or, when `OWNER` is the
authenticating user, just `NAME`) and/or as GitHub repository URLs.  If no
repositories are specified on the command line, then the current directory must
belong to a Git repository whose `origin` remote points to a GitHub repository;
`labelmaker` will operate on this remote repository.

For each repository, all changes are calculated before modifying anything, so
if an error occurs based on the state of the configuration file and/or current
repository labels, it will be caught before any changes to the repository are
made.

### Options

- `--dry-run` — Do not change anything in GitHub, but do emit log messages
  showing what would be changed.

- `-P NAME`/`--profile NAME` — Specify which profile in the configuration file
  to use.  Defaults to the value of `defaults.profile` in the configuration
  file, or to `default`.

Configuration File
------------------

The configuration file is a [TOML](https://toml.io) file with the following
fields:

- `[defaults]` — A table of default label settings to apply to all labels in
  this configuration file.  Any field that can be set on a label can be set
  here, other than `name` and `rename-from`.  File-wide defaults can be
  overridden for specific profiles via the `[profiles.*.defaults]` tables.

    - The `[defaults]` table may also contain a `profile` string field
      specifying the default profile for `apply` to use when no `--profile`
      option is given; the default profile is `default`.

- `[profiles.<name>]` — The configuration file may contain multiple *profiles*,
  different sets of label definitions that can be selected when invoking
  `apply`.  Each profile is defined by a table with the following fields:

    - `[profile.<name>.defaults]` — A table of default label settings to apply
      to all labels in this profile.  Any field that can be set on a label can
      be set here, other than `name` and `rename-from`.  Settings set here
      override settings set in the top-level `[defaults]` table for the labels
      in this profile.

    - `[[profiles.<profile>.labels]]` — A list of label specifications; each
      one is a table with the following fields:

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
          when creating a new label in the GitHub UI as of 2023-09-24.

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

            - It is an error if a label inclues its own name in `rename-from`.

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

### Example Configuration File

This shows the default GitHub labels as of 2023-10-18, along with the default
value of the `color` setting:

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

[[profile.default.label]]
name = "bug"
color = "d73a4a"
description = "Something isn't working"

[[profile.default.label]]
name = "documentation"
color = "0075ca"
description = "Improvements or additions to documentation"

[[profile.default.label]]
name = "duplicate"
color = "cfd3d7"
description = "This issue or pull request already exists"

[[profile.default.label]]
name = "enhancement"
color = "a2eeef"
description = "New feature or request"

[[profile.default.label]]
name = "good first issue"
color = "7057ff"
description = "Good for newcomers"

[[profile.default.label]]
name = "help wanted"
color = "008672"
description = "Extra attention is needed"

[[profile.default.label]]
name = "invalid"
color = "e4e669"
description = "This doesn't seem right"

[[profile.default.label]]
name = "question"
color = "d876e3"
description = "Further information is requested"

[[profile.default.label]]
name = "wontfix"
color = "ffffff"
description = "This will not be worked on"
```

[CSS color names][]: https://www.w3.org/TR/css-color-4/#named-colors