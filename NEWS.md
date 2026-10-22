# thipR (development version)

- New "Learn THIP data" course on the website: eight guides, from meeting the
  data to a worked appendicitis readmission example, data checks and responsible
  use. They run on the example database and replace the old guides; the old
  "Prepare and filter sources" and "Retrieve time windows" pages redirect to their
  replacements. `vignette("getting-started")` and `vignette("condition-flags")`
  are no longer installed; `vignette("start-here")` is.

- New dataset `thip_fields`: every documented column of THIP's yearly inpatient
  tables, with its group, description, format and code meanings.

- `icd_flag()` and `thip_filter_icd()` now also reject ranges written with an en
  dash or another Unicode dash (as copied from documents), and code lists with
  Unicode spaces, instead of silently matching nothing. The help pages now say
  plainly that `*`, `+` and `/` are kept as characters, so `"E11*"` matches no code.

- New `thip_connect_example()` opens a small synthetic THIP-like database
  (DuckDB, three invented yearly inpatient tables) so workflows can be practised,
  taught and tested without THIP access. thipR's functions and ordinary dplyr
  verbs work on it as on THIP. Needs the optional duckdb package (1.3.0 or later).

- `icd_flag()` on local data is about four times faster on large inputs: each
  distinct code is now cleaned once instead of once per cell. Results are
  unchanged.

# thipR 0.2.1

- Simplify THIP installation: use R's standard personal library
  (`R_LIBS_USER`), created once, instead of a custom `~/R/thipR-library` that
  needed `.libPaths()` in every session.

- Accept ClickHouseHTTP 1.1.2 or later instead of exactly 1.1.2, so a normal
  `pak::pak("Songyosr/thipR")` keeps working when CRAN updates the driver.
  Versions older than 1.1.2 are refused; a newer version that changes the
  connection structure thipR relies on stops with a clear message; other newer
  versions warn once per session as untested.

- Document what first-time `thip_setup()` looks like in a THIP notebook: the
  prompts in order, the masked password box, the verification step, the
  confirmation line and a troubleshooting table, in the help page and the
  start-here guide.

# thipR 0.2.0

First public release.

- Document Thai ICD-10-TM five-character codes in the condition guide: exact
  matching on a four-character code misses more detailed codes; use prefix
  matching and tabulate recorded codes before freezing a definition.


- Pin `ClickHouseHTTP (== 1.1.2)` in Imports so dependency resolution matches the
  runtime driver check; a newer driver now fails at install/load instead of at
  connection. The runtime error names the installed version and the pinned
  install command. Installation guides install the tested driver explicitly first.

- `icd_flag()` and `thip_filter_icd()` now error on range (`Q20-Q26`), SQL/regex
  wildcard and comma/space-list notation, which previously matched nothing and
  returned plausible all-FALSE results. Literal codes, prefixes, `*`, `+`, `/`,
  leading-zero procedure codes and empty vectors are unchanged.

- Add `thip_date_audit()` for explicit aggregate checks of raw/converted date
  pairs, separating missing text, blanks, parsed values and conversion failures.
  Document per-column formats and audit-before-filtering in the source guide.

- Split learning material into five focused guides with shared pkgdown navigation
  and reproducibly generated THIP notebooks plus a short index.

- Add offline synthetic and remote-source vignettes, with standard knitr build registration and explicit API execution contracts.

- Add opt-in date/datetime mappings and explicit formats to `thip_table()`.
  Preserve raw fields; conversions stay lazy and invalid values become missing.
  Datetimes require an explicit timezone and preserve millisecond precision.

# thipR 0.1.0

- Add `thip_window()` plans with explicit column roles and date boundaries;
  `dplyr::collect()` executes complete, read-only person batches. Local data
  frames use a keyed index and interval matching without altering source dates.
- Add structural validation, empty-window handling, completion reports and
  failure-without-partial-success behavior. Final collected data must fit R RAM.
- Extend `thip_table(con, tables = ...)` to compatible lazy UNION ALL sources,
  column alignment and optional provenance. Warn on the legacy singular argument.
- Add lazy `thip_filter_icd()` sharing ICD normalization and matching policy.
  Keep public `icd_flag()` local; preserve SQL flag experiments internally.
- Inherit platform row/time settings by default; retain configurable limits,
  two-thread default, read-only requests and errors on overflow.
- Deprecate `thip_collect()` and `thip_disconnect()` in favor of standard APIs.

# thipR 0.0.1.9003

- Add `thip_setup()` for one-time, user-level `.Renviron` configuration inside
  THIP, with masked password entry or existing process configuration.
- Preserve unrelated environment-file lines, require explicit replacement of
  existing THIP entries, and atomically write an owner-only plaintext file.
- Reuse connection validation and the SELECT-only verification step before
  saving; support explicit offline setup with `verify = FALSE`.
- Keep `thip_connect()` environment-based and retain explicit-argument overrides.
  Isolated THIP integration verifies masked input and a fresh-kernel connection.

# thipR 0.0.1.9002

- Dispatch `icd_flag()` automatically to read-only lazy SQL for ClickHouseHTTP
  dbplyr tables, retaining the sparse implementation for local data frames.
- Preserve exact/prefix, normalization, missing-code and repeated-ID semantics
  across backends. SQL row order is unspecified; collection remains explicit.
- Add offline SQL tests and a synthetic-only live equivalence check.
- Add opt-in `sql_strategy = "distinct"` to reuse matches across distinct codes
  using a single source scan and code-to-ID aggregation; retain `"row"` as the
  default. Both strategies remain lazy and read-only.
- Separate shared validation, local matching/aggregation and SQL construction
  into focused internal modules. Remove a redundant dense flag allocation and
  numeric-ID text conversion; reuse synthetic fixtures and an independent
  reference matcher across offline and smoke checks.

# thipR 0.0.1.9001

- Add local `icd_flag()` for custom condition lists, exact/prefix matching,
  dotted-code normalization, and sparse aggregation across repeated IDs.

# thipR 0.0.1.9000

- Initial internal DBI/dbplyr connection prototype.
- SELECT-only initialization with verified read-only request settings.
- Metadata discovery, lazy table access and bounded collection.
- Offline unit tests, opt-in read-only live checks and an R starter notebook.
