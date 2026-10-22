# thipR

**Prepare THIP sources, retrieve person-specific histories and flag custom ICD lists in R.**

thipR supports read-only analysis inside the THIP notebook platform using DBI
and dbplyr. Filter records in the database, collect the records you need, then
analyse them in R. Local condition flags and time-window examples also work
with synthetic data without a database connection.

**Status:** early public release. Interfaces may still change before
1.0. The live backend is tested with ClickHouseHTTP 1.1.2.

## Installation

Install from GitHub:

```r
install.packages("pak")  # Once, if needed
pak::pak("Songyosr/thipR")
```

In a THIP notebook, create R's personal library once first; see
[Start here](https://songyosr.github.io/thipR/articles/start-here.html) for the
THIP installation steps and one-time database setup.
Installing thipR does not grant THIP database access; credentials come from the
THIP platform. Never put credentials in notebook cells or source files.

## Quick start

Run this example inside an authorized THIP R session. Select tables and a date
range appropriate to your study, and verify their schema and date formats first.
These snippets are illustrative and are not executed when building this page.

```r
library(thipR)
library(dplyr)

# Once, if the platform has not supplied your database configuration:
# thip_setup()
con <- thip_connect()

ipd <- thip_table(
  con, tables = c("nhso2567", "nhso2568"), .id = "source_table",
  dates = c(adm = "dateadm", dsc = "datedsc", death = "death_date"),
  date_format = c(adm = "%Y%m%d", dsc = "%Y%m%d", death = "%Y-%m-%d")
)

# Still a lazy database query; adm is the converted Date column.
candidates <- ipd |>
  filter(adm >= as.Date("2023-10-01"), adm < as.Date("2023-10-08")) |>
  thip_filter_icd(codes = "K35", cols = "pdx", match = "prefix") |>
  select(pid, tran_id, adm, pdx)

entries <- collect(candidates)  # Records enter this R session here.
DBI::dbDisconnect(con)
```

This selects candidate admissions, not a finalized cohort or a validated
clinical definition. Date conversion retains original text and stays lazy.
Use [`thip_date_audit()`](https://songyosr.github.io/thipR/reference/thip_date_audit.html)
to distinguish missing or blank text from conversion failures **before** filtering
on converted dates. Audits execute aggregate queries over the selected scope.

For person-specific histories, `thip_window()` takes a source and an entry roster
with IDs and dates; use `date = "adm"` for the converted source date. Its
`collect()` method retrieves complete person batches. Use `icd_flag()` on local
records to flag multiple custom condition lists. See the runnable examples below.

## Learn THIP data

The guides form a short course. They run on an invented example database
(`thip_connect_example()`), so you can follow along without THIP access.

| Guide | What you will learn |
|---|---|
| [Start here](https://songyosr.github.io/thipR/articles/start-here.html) | Install thipR and connect to THIP |
| [Meet the THIP data](https://songyosr.github.io/thipR/articles/meet-the-data.html) | The yearly tables, what one row is, the main columns |
| [Working with a database from R](https://songyosr.github.io/thipR/articles/database-from-r.html) | Where your code runs, and when data come into R |
| [From question to cohort](https://songyosr.github.io/thipR/articles/question-to-cohort.html) | Turn a research question into one entry per person |
| [Look back and follow up](https://songyosr.github.io/thipR/articles/look-back-follow-up.html) | Fetch each person's history and outcomes around their entry |
| [Flag conditions](https://songyosr.github.io/thipR/articles/condition-flags.html) | Turn reviewed code lists into flags per person or entry |
| [Worked example](https://songyosr.github.io/thipR/articles/getting-started.html) | The whole course in one short analysis |
| [Check before you trust](https://songyosr.github.io/thipR/articles/data-checks.html) | Checks to run on the data before relying on it |
| [Using THIP data responsibly](https://songyosr.github.io/thipR/articles/responsible-use.html) | What THIP's conditions and good practice ask of you |

The [function reference](https://songyosr.github.io/thipR/reference/index.html)
describes arguments and return values. [News](https://songyosr.github.io/thipR/news/index.html)
records release and development changes. Installed builds containing vignettes
also support `vignette("start-here", package = "thipR")`.

## Scope

- Database connections use read-only request settings. Account permissions and
  server limits remain controlled by the platform.
- Table unions preserve duplicates. Source coverage, identity linkage, clinical
  definitions and cohort eligibility must be established for each study.
- Ordinary dplyr verbs, `collect()` and `DBI::dbDisconnect()` remain the standard
  interfaces. Collecting a result requires enough R memory for that result.
- No clinical definitions, automatic deduplication or survival-analysis decisions
  are supplied by the package.

Contributors can find build and test instructions in `dev/CONTRIBUTING.md`.
Please report problems and suggestions through GitHub issues.
