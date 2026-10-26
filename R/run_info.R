#' Record what the tables held when you ran
#'
#' Counts the rows of each table and notes when, and with which software, the
#' count was made. Keep the result with your study's outputs: THIP's tables can
#' change between runs, and a record of row counts and versions shows which
#' state of the data a result came from.
#' @param con A connection returned by [thip_connect()] or
#'   [thip_connect_example()].
#' @param tables One or more unique table names.
#' @return A data frame with one row per table: `table`, `rows` and
#'   `checked_at` (UTC). Its `"versions"` attribute is a named character vector:
#'   R, thipR, dplyr, dbplyr, DBI, the database driver and the database server.
#' @details Runs one `count()` query per table; no records are read. Row counts
#'   of real tables are results from THIP: follow THIP's terms and your protocol
#'   before the record leaves THIP.
#' @examples
#' \donttest{
#' if (requireNamespace("duckdb", quietly = TRUE) &&
#'     utils::packageVersion("duckdb") >= "1.3.0") {
#'   con <- thip_connect_example()
#'   info <- thip_run_info(con, c("nhso2566", "nhso2567"))
#'   info
#'   attr(info, "versions")
#'   DBI::dbDisconnect(con)
#' }
#' }
#' @export
thip_run_info <- function(con, tables) {
  if (!inherits(con, "DBIConnection")) stop("con must be a database connection.", call. = FALSE)
  if (!.icd_valid_names(tables)) stop("tables must be unique, nonempty table names.", call. = FALSE)
  .database(con)
  count_rows <- function(table) {
    query <- dplyr::count(dplyr::tbl(con, table), name = "rows")
    as.numeric(dplyr::pull(dplyr::collect(query), "rows"))
  }
  out <- data.frame(table = tables, rows = vapply(tables, count_rows, numeric(1)),
                    checked_at = rep(as.POSIXct(Sys.time(), tz = "UTC"), length(tables)),
                    row.names = NULL)
  attr(out, "versions") <- .run_versions(con)
  out
}

.run_versions <- function(con) {
  version_of <- function(pkg) if (nzchar(system.file(package = pkg)))
    as.character(utils::packageVersion(pkg)) else NA_character_
  driver <- if (identical(.thip_dialect(con), "example")) "duckdb" else if (inherits(con, "ThipAdbcConnection")) "adbi" else "ClickHouseHTTP"
  server <- tryCatch(as.character(DBI::dbGetQuery(con, "SELECT version() AS v")$v[1]),
                     error = function(e) NA_character_)
  stats::setNames(
    c(paste(R.version$major, R.version$minor, sep = "."), version_of("thipR"),
      version_of("dplyr"), version_of("dbplyr"), version_of("DBI"),
      version_of(driver), server),
    c("R", "thipR", "dplyr", "dbplyr", "DBI", driver, "server"))
}

