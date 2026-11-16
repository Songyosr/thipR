# thipR: guidance for contributors and coding agents

This repository is public. Everything committed here, including history, is
visible to anyone. thipR provides read-only R access to THIP notebook databases.

## Never commit

- Credentials, tokens, `.Renviron` values or authentication headers.
- Platform hostnames, internal network addresses or personal account paths.
- Results from real THIP data: counts, tables, figures, run identifiers or logs,
  even when aggregate, except the narrowly authorized snapshot below.
- Patient-level records, identifiers or extracts of any kind.
- Internal study notes, protocols or verification records. Keep them outside this
  repository.

## Authorized public aggregate snapshot

The user authorized the 2026-10-27 yearly inpatient row-count snapshot for
"Meet the THIP data". `inst/extdata/thip_yearly_row_counts.csv` and its rendered
guide table may contain only `table_name` (nhsoYYYY), `fiscal_year_be`,
`stored_rows` (the complete table's count, without subgroups) and `checked_date`.
This exception does not cover unique-person counts, filtered subsets, record
previews, identifiers, credentials, hostnames, private notes or execution logs.
All illustrative records and executable teaching examples remain synthetic.
The public build reads the saved snapshot and never connects to THIP.

## Development rules

- Examples and tests use synthetic data only; the dated snapshot above is the
  sole real-result exception in the guides. Live snippets stay
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
