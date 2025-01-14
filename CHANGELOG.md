v0.6.1 (in development)
-----------------------
- **Bugfix:** Don't retry on 4xx statuses other than 403

v0.6.0 (2024-12-20)
-------------------
- `apply`: The argument to `--repo-file` can now be `-` to read from standard
  input
- Increased MSRV to 1.81
- Switched internal HTTP library from `reqwest` to `ureq`

v0.5.0 (2023-11-06)
-------------------
- The log message emitted before each API request is now emitted after the log
  message about sleeping before mutating requests
- The log message emitted before each API request is now repeated on each retry
  attempt
- `apply`: Added a `--repo-file` option for reading repositories to operate on
  from a file

v0.4.0 (2023-10-30)
-------------------
- Support JSON, JSON5, and YAML configuration files
- Fix example TOML configuration in README

v0.3.0 (2023-10-24)
-------------------
- Properly normalize whitespace in label names
- Properly normalize whitespace in descriptions
- Added a `make` subcommand for creating & updating individual labels

v0.2.0 (2023-10-22)
-------------------
- Sleep between mutating API requests in order to keep them at least one second
  apart, as recommended by GitHub
- Retry (with exponential backoff) requests that fail with 5xx errors or due to
  rate limiting
- Log a message if no changes are made to a given repository

v0.1.0 (2023-10-19)
-------------------
Initial release
