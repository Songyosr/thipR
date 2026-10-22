#' Summarise each source table before you rely on it
#'
#' One row per source table (or other group) with the checks from the guide
#' [Check before you
#' trust](https://songyosr.github.io/thipR/articles/data-checks.html): how many
#' records and people it holds, which dates it covers, months with no records,
#' missing dates and repeated record keys.
#' @param data A lazy table from [thip_table()] or a local data frame.
#' @param date The converted date column to summarise, such as `"adm"`.
#' @param by Grouping columns, usually `"source_table"`. `character()` summarises
#'   the whole stack, which also counts keys repeated across tables.
#' @param key The column that should identify each record once. Default
#'   `"tran_id"`.
#' @param person The person column. Default `"pid"`.
#' @return A local data frame with one row per group: the `by` columns,
#'   `records`, `people`, `first` and `last` (the earliest and latest `date`),
#'   `months` (months with at least one record), `months_spanned` (months from
#'   `first` to `last`), `missing_dates` (records whose `date` is missing, such
#'   as failed conversions) and `repeated_keys` (`key` values held by more than
#'   one record in the group).
#' @details Runs two aggregate queries on a lazy table; no records are read.
#'   `months_spanned` greater than `months` means a gap. A table whose `first`
#'   and `last` fall outside its fiscal year, or two tables whose dates overlap,
#'   deserve a look. The counts describe real tables: follow THIP's terms and
#'   your protocol before they leave THIP. Existing dplyr grouping is ignored;
#'   use `by`.
#' @examples
#' \donttest{
#' if (requireNamespace("duckdb", quietly = TRUE) &&
#'     utils::packageVersion("duckdb") >= "1.3.0") {
#'   con <- thip_connect_example()
#'   admissions <- thip_table(con, tables = c("nhso2566", "nhso2567"),
#'                            .id = "source_table",
#'                            dates = c(adm = "dateadm"),
#'                            date_format = "%Y%m%d")
#'   thip_source_summary(admissions, date = "adm", by = "source_table")
#'   DBI::dbDisconnect(con)
#' }
#' }
#' @export
thip_source_summary <- function(data, date, by = "source_table", key = "tran_id",
                                person = "pid") {
  if (!inherits(data, "tbl_sql") && !is.data.frame(data))
    stop("data must be a data frame or lazy SQL table.", call. = FALSE)
  fields <- colnames(data)
  .check_fields(fields, "data")
  one_column <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && x %in% fields
  if (!one_column(date) || !one_column(key) || !one_column(person))
    stop("date, key and person must each name one column of data.", call. = FALSE)
  outputs <- c("records", "people", "first", "last", "months", "months_spanned",
               "missing_dates", "repeated_keys")
  if (!is.character(by) || anyNA(by) || anyDuplicated(by) || any(!by %in% fields) ||
      any(by %in% c(date, key, person, outputs)))
    stop("by must name unique existing columns other than date, key and person.",
         call. = FALSE)
  if (is.data.frame(data) && !inherits(data[[date]], "Date"))
    stop("date must be a Date column; convert it with thip_table() first.", call. = FALSE)
  data <- dplyr::ungroup(data)
  groups <- dplyr::syms(by)
  d <- dplyr::sym(date); k <- dplyr::sym(key); p <- dplyr::sym(person)
  .data <- dplyr::.data
  out <- data |>
    dplyr::mutate(.month = substr(as.character(!!d), 1L, 7L)) |>
    dplyr::group_by(!!!groups) |>
    dplyr::summarise(records = dplyr::n(), people = dplyr::n_distinct(!!p),
                     first = min(!!d, na.rm = TRUE), last = max(!!d, na.rm = TRUE),
                     months = dplyr::n_distinct(.data$.month, na.rm = TRUE),
                     missing_dates = sum(as.integer(is.na(!!d)), na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::collect() |>
    as.data.frame()
  repeats <- data |>
    dplyr::count(!!!groups, !!k, name = ".n") |>
    dplyr::filter(.data$.n > 1L) |>
    dplyr::count(!!!groups, name = "repeated_keys") |>
    dplyr::collect() |>
    as.data.frame()
  out <- if (length(by)) merge(out, repeats, by = by, all.x = TRUE, sort = TRUE) else
    cbind(out, repeated_keys = if (nrow(repeats)) repeats$repeated_keys else 0)
  out$first <- as.Date(out$first); out$last <- as.Date(out$last)
  month_index <- function(x) as.numeric(format(x, "%Y")) * 12 + as.numeric(format(x, "%m"))
  out$months_spanned <- month_index(out$last) - month_index(out$first) + 1
  for (col in c("records", "people", "months", "missing_dates", "repeated_keys"))
    out[[col]] <- as.numeric(out[[col]])
  out$repeated_keys[is.na(out$repeated_keys)] <- 0
  out <- out[c(by, outputs)]
  rownames(out) <- NULL
  out
}
