# thipR

**Work with THIP's yearly inpatient tables in R: stack them, look back and follow up from each entry, and flag conditions with your own code lists.**

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

Run this in a notebook inside your THIP account. It is the recipe from
[From question to cohort](https://songyosr.github.io/thipR/articles/question-to-cohort.html):
it selects admissions with a principal diagnosis of appendicitis (codes starting
K35) and keeps each person's first as their entry. Choose the tables, dates and
codes for your own study.

```r
library(thipR)
library(dplyr)

# Once, unless the platform supplies your database settings:
# thip_setup()
con <- thip_connect()

# The yearly tables as one query, with dates converted.
admissions <- thip_table(
  con,
  tables = c("nhso2565", "nhso2566", "nhso2567"),
  .id = "source_table",
  dates = c(adm = "dateadm", dsc = "datedsc"),
  date_format = c(adm = "%Y%m%d", dsc = "%Y%m%d")
)

# The candidate records come into R at collect().
candidates <- admissions |>
  filter(adm >= as.Date("2021-10-01"),
         adm <= as.Date("2024-08-31")) |>
  thip_filter_icd(codes = "K35", cols = "pdx",
                  match = "prefix") |>
  collect()

# One entry per person: the first with a discharge date.
entries <- candidates |>
  filter(!is.na(dsc)) |>
  arrange(pid, adm, tran_id) |>
  distinct(pid, .keep_all = TRUE) |>
  select(pid, entry_id = tran_id,
         adm_date = adm, dsc_date = dsc)

DBI::dbDisconnect(con)
```

Before you filter on dates, count the ones that could not be read with
[`thip_date_audit()`](https://songyosr.github.io/thipR/reference/thip_date_audit.html).
From `entries`, [`thip_window()`](https://songyosr.github.io/thipR/reference/thip_window.html)
fetches each person's records in a look-back or follow-up window, and
[`icd_flag()`](https://songyosr.github.io/thipR/reference/icd_flag.html) flags
conditions with your own code lists. The guides below run every step on an
example database.

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

- thipR only reads from the database; your THIP account and the platform decide
  what you can see and how much one query may return.
- Stacking tables keeps every row; thipR does not remove repeated records.
- For each study, check which years your tables cover and whether `pid`
  identifies the same person in every table you stack.
- thipR supplies no clinical definitions: the code lists, eligibility rules and
  analysis choices are yours.

Contributors can find build and test instructions in `dev/CONTRIBUTING.md`.
Please report problems and suggestions through GitHub issues.
