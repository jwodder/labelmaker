v0.2.0 (in development)
-----------------------
- Sleep between mutating API requests in order to keep them at least one second
  apart, as recommended by GitHub
- Retry (with exponential backoff) requests that fail with 5xx errors or due to
  rate limiting
- Log a message if no changes are made to a given repository

v0.1.0 (2023-10-19)
-------------------
Initial release
