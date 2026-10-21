# thipR: guidance for contributors and coding agents

This repository is public. Everything committed here, including history, is
visible to anyone. thipR provides read-only R access to THIP notebook databases.

## Never commit

- Credentials, tokens, `.Renviron` values or authentication headers.
- Platform hostnames, internal network addresses or personal account paths.
- Results from real THIP data: counts, tables, figures, run identifiers or logs,
  even when aggregate.
- Patient-level records, identifiers or extracts of any kind.
- Internal study notes, protocols or verification records. Keep them outside this
  repository.

## Development rules

- Examples, tests and vignettes use synthetic data only. Live snippets stay
  `eval=FALSE`; opt-in smoke scripts in `inst/examples/` use generated records.
- Database access is read-only: SELECT and lazy reads only, no table creation
  or mutation.
- The tested backend is ClickHouseHTTP 1.1.2: the minimum in `DESCRIPTION` and
  `.tested_clickhouse_version` in `R/connection.R` must match. If a new driver
  changes the connection slots thipR sets, update `.clickhouse_slots` and the
  connection factory together.
- The package supplies no clinical definitions; code lists are user-supplied.

## Before pushing

```sh
Rscript -e 'devtools::test()'
Rscript -e 'devtools::check()'
python3 tools/check-repo-files.py
```

For documentation changes, also build the site and run
`python3 tools/check-public-docs.py docs`. See `dev/CONTRIBUTING.md`.

## Releases

Bump `Version` in `DESCRIPTION`, add a `NEWS.md` section, and tag `vX.Y.Z`.
