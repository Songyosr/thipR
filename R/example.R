#' Connect to a small synthetic THIP-like database
#'
#' Opens an in-memory DuckDB database with three invented yearly inpatient tables
#' (`nhso2565`, `nhso2566`, `nhso2567`) that use THIP's column names and text
#' formats. thipR functions and ordinary dplyr verbs work on it as they do on THIP,
#' so you can practise, teach or test a workflow without THIP access.
#'
#' @details Every person and record is invented by a seeded script
#'   (`data-raw/thip_example.R` in the package's GitHub repository); a few values
#'   are deliberately damaged so that checks have something to find. Rates in it
#'   are not THIP results. [dplyr::show_query()] shows DuckDB's SQL rather than
#'   ClickHouse's, and datetime conversion in [thip_table()] is not available here.
#'   Requires duckdb 1.3.0 or later.
#' @return A DBI connection. Close it with `DBI::dbDisconnect(con)`.
#' @md
#' @examplesIf requireNamespace("duckdb", quietly = TRUE) && utils::packageVersion("duckdb") >= "1.3.0"
#' con <- thip_connect_example()
#' thip_tables(con)
#' DBI::dbDisconnect(con)
#' @export
thip_connect_example <- function() {
  if (!requireNamespace("duckdb", quietly = TRUE) || utils::packageVersion("duckdb") < "1.3.0")
    stop("thip_connect_example() needs duckdb 1.3.0 or later: install.packages(\"duckdb\").", call. = FALSE)
  path <- system.file("extdata", "thip_example.rds", package = "thipR")
  if (!nzchar(path)) stop("Example data not found; reinstall thipR.", call. = FALSE)
  con <- suppressMessages(DBI::dbConnect(duckdb::duckdb()))
  tables <- readRDS(path)
  for (name in names(tables)) DBI::dbWriteTable(con, name, tables[[name]])
  for (sql in .example_macros) DBI::dbExecute(con, sql)
  attr(con, "thipr_database") <- "main"
  attr(con, "thipr_example") <- TRUE
  con
}

# SQL dialect of a connection: THIP's ClickHouse or the synthetic DuckDB example.
.thip_dialect <- function(con) {
  if (inherits(con, "ClickHouseHTTPConnection") || inherits(con, "ThipAdbcConnection")) return("clickhouse")
  if (isTRUE(attr(con, "thipr_example"))) return("example")
  NA_character_
}

# ClickHouse functions used by thipR's SQL, defined under the same names in DuckDB.
# Lambdas, CAST(x, 'type'), ALL joins, tuples and system tables are switched in
# the SQL builders instead, because DuckDB macros cannot express them.
.example_macros <- c(
  "CREATE MACRO trimBoth(x, chars := ' ') AS trim(x, chars)",
  "CREATE MACRO upperUTF8(x) AS upper(x)",
  "CREATE MACRO replaceAll(x, a, b) AS replace(x, a, b)",
  "CREATE MACRO startsWith(x, p) AS starts_with(x, p)",
  "CREATE MACRO match(x, p) AS regexp_matches(x, p)",
  "CREATE MACRO toString(x) AS CAST(x AS VARCHAR)",
  "CREATE MACRO toDate32OrNull(x) AS TRY_CAST(x AS DATE)",
  "CREATE MACRO toDate32(x) AS CAST(x AS DATE)",
  "CREATE MACRO countIf(x) AS coalesce(count_if(CAST(x AS BOOLEAN)), 0)",
  "CREATE MACRO throwIf(c, msg) AS CASE WHEN c THEN error(msg) ELSE 0 END",
  "CREATE MACRO toFloat64OrZero(x) AS coalesce(TRY_CAST(x AS DOUBLE), 0)",
  "CREATE MACRO empty(x) AS (length(x) = 0)",
  "CREATE MACRO has(a, x) AS list_contains(a, x)",
  "CREATE MACRO hasAny(a, b) AS list_has_any(a, b)",
  paste("CREATE MACRO toTypeName(x) AS CASE typeof(x) WHEN 'VARCHAR' THEN 'String'",
        "WHEN 'BIGINT' THEN 'Int64' WHEN 'INTEGER' THEN 'Int32' WHEN 'DOUBLE' THEN 'Float64'",
        "ELSE typeof(x) END"),
  "CREATE MACRO currentDatabase() AS 'main'")
