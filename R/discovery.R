.database <- function(con) {
  db <- attr(con, "thipr_database")
  if (is.null(db)) stop("Use a connection created by thip_connect().", call. = FALSE)
  db
}

#' List available THIP tables
#'
#' Returns table names and engines from the configured database. Storage tables
#' ending in `_local` are hidden by default. This reads metadata, not table records.
#'
#' Use this function to discover available tables, then [thip_table()] to create
#' a lazy query over one or more selected tables. `thip_tables()` is a
#' compatibility alias with the same arguments and behavior.
#'
#' @md
#' @param con A connection returned by [thip_connect()].
#' @param include_local Include storage tables ending in _local? Default FALSE.
#' @return A data frame of table names and engines; no patient records.
#' @export
thip_list_tables <- function(con, include_local = FALSE) {
  if (!is.logical(include_local) || length(include_local) != 1L || is.na(include_local))
    stop("include_local must be TRUE or FALSE.", call. = FALSE)
  db <- DBI::dbQuoteString(con, .database(con))
  out <- DBI::dbGetQuery(con, if (identical(.thip_dialect(con), "example")) paste0(
    "SELECT table_name AS name, 'DuckDB' AS engine FROM information_schema.tables ",
    "WHERE table_schema = ", db, " ORDER BY name") else paste0(
    "SELECT name, engine FROM system.tables WHERE database = ", db, " ORDER BY name"))
  if (!include_local) out <- out[!endsWith(out$name, "_local"), , drop = FALSE]
  out
}

#' @rdname thip_list_tables
#' @export
thip_tables <- thip_list_tables

#' Inspect column metadata without reading table records
#' @param con A connection returned by [thip_connect()].
#' @param table One table name.
#' @return A data frame of column names, types, positions and comments.
#' @export
thip_columns <- function(con, table) {
  table <- .required_string(table, "table")
  db <- DBI::dbQuoteString(con, .database(con))
  if (identical(.thip_dialect(con), "example")) return(DBI::dbGetQuery(con, paste0(
    "SELECT column_name AS name, CASE data_type WHEN 'VARCHAR' THEN 'String' ",
    "ELSE data_type END AS type, ordinal_position AS position, '' AS comment ",
    "FROM information_schema.columns WHERE table_schema = ", db,
    " AND table_name = ", DBI::dbQuoteString(con, table), " ORDER BY position")))
  DBI::dbGetQuery(con, paste0(
    "SELECT name, type, position, comment FROM system.columns WHERE database = ",
    db, " AND table = ", DBI::dbQuoteString(con, table), " ORDER BY position"))
}

#' Create a lazy dbplyr table
#'
#' Database access occurs inside THIP. This may request column metadata;
#' records are fetched only when the lazy query is printed or collected.
#' Multiple compatible tables are stacked with UNION ALL, preserving duplicates.
#' Column order is aligned explicitly. Base types must match; Nullable and
#' LowCardinality wrappers may differ. No clinical harmonization is performed.
#' Use separate datasets for different kinds of records (e.g. vaccinations and
#' admissions). Metadata checks cannot establish semantic compatibility.
#' @param con A connection returned by [thip_connect()].
#' @param tables One or more unique distributed table names.
#' @param .id Optional new column containing the source table name.
#' @param table Deprecated singular argument, accepted for compatibility.
#' @param dates,datetimes Named character mappings `new_column = "source_column"`.
#'   Originals are retained. NULL (default) performs no conversion.
#' @param date_format,datetime_format Explicit source format: one string for each
#'   mapping group, or a named vector keyed by output column. Supported formats
#'   are `%Y%m%d`, `%Y-%m-%d`, `%Y-%m-%d %H:%M:%S`, and
#'   `%Y-%m-%d %H:%M:%OS` (optional 1-3 fractional digits). These are a bounded
#'   set of R-style formats, not arbitrary strptime syntax.
#' @param timezone Required IANA zone (e.g. "UTC") for datetimes. No zone is
#'   inferred from source text or the server. Ambiguous DST times should be
#'   resolved upstream; offset-bearing strings are not accepted.
#' @details Conversion stays lazy. Dates use Date32 and retain the source calendar
#'   date, explicitly discarding any clock time. Datetimes use DateTime64(3) and
#'   preserve milliseconds. Blank, NULL, malformed, impossible or out-of-range
#'   values become SQL NULL (R NA); original text remains available for auditing.
#'   Supported calendar years are 1900-2299. Timestamp DST gaps that fail a clock
#'   round-trip become NULL. No best-effort format guessing is performed.
#' @return A dbplyr lazy table supporting ordinary dplyr verbs.
#' @export
thip_table <- function(con, tables = NULL, .id = NULL, table = NULL,
                       dates = NULL, date_format = NULL,
                       datetimes = NULL, datetime_format = NULL, timezone = NULL) {
  if (!is.null(table)) {
    if (!is.null(tables)) stop("Supply tables, not both tables and table.", call. = FALSE)
    warning("table is deprecated; use tables = ...", call. = FALSE)
    tables <- table
  }
  if (!is.character(tables) || !length(tables) || anyNA(tables) ||
      any(!nzchar(trimws(tables))) || anyDuplicated(tables))
    stop("tables must contain unique nonempty table names.", call. = FALSE)
  if (any(endsWith(tables, "_local")))
    stop("Use the distributed table name, without _local.", call. = FALSE)
  meta <- lapply(tables, function(x) thip_columns(con, x))
  if (any(vapply(meta, nrow, integer(1)) == 0L)) stop("A requested table has no available column metadata.", call. = FALSE)
  fields <- meta[[1L]]$name
  .check_fields(fields, "table")
  base_type <- function(x) {
    repeat {
      y <- sub("^(Nullable|LowCardinality)\\((.*)\\)$", "\\2", x)
      if (identical(x, y)) return(x)
      x <- y
    }
  }
  types <- base_type(meta[[1L]]$type)
  for (m in meta) {
    .check_fields(m$name, "table")
    if (!setequal(fields, m$name) || !identical(types, base_type(m$type[match(fields, m$name)])))
      stop("Incompatible table schemas; explicitly select, rename or harmonize before stacking.", call. = FALSE)
  }
  if (!is.null(.id)) {
    .id <- .required_string(.id, ".id")
    if (.id %in% fields) stop(".id collides with a source column.", call. = FALSE)
  }
  ident <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  queries <- vapply(tables, function(name) {
    from <- ident(DBI::Id(schema = .database(con), table = name))
    provenance <- if (is.null(.id)) "" else paste0(", ", DBI::dbQuoteString(con, name), " AS ", ident(.id))
    paste0("SELECT ", paste(ident(fields), collapse = ", "), provenance, " FROM ", from)
  }, character(1))
  source <- dplyr::tbl(con, dbplyr::sql(paste(queries, collapse = " UNION ALL ")), vars = c(fields, .id))
  .table_temporal(source, dates, date_format, datetimes, datetime_format, timezone,
                  stats::setNames(c(types, if (!is.null(.id)) "String"), c(fields, .id)))
}

#' Collect a bounded lazy query without silently truncating it
#'
#' Requests at most max_rows + 1 rows and fails if the requested bound is
#' exceeded. Server-side limits still apply. Filtering and aggregation should
#' happen before collection. Do not use compute() or copy_to() in this workflow.
#' @param query A dbplyr tbl_sql query.
#' @param max_rows Maximum accepted rows, default 1000.
#' @return A local tibble, or an error when the bound is exceeded.
#' @export
thip_collect <- function(query, max_rows = 1000L) {
  .Deprecated("dplyr::collect", msg = "thip_collect() is deprecated; use dplyr::collect(). Its legacy explicit row check is retained for compatibility.")
  if (!inherits(query, "tbl_sql")) stop("query must be a dbplyr lazy query.", call. = FALSE)
  max_rows <- .positive_integer(max_rows, "max_rows", .Machine$integer.max - 1L)
  out <- dplyr::collect(utils::head(query, max_rows + 1L))
  if (nrow(out) > max_rows)
    stop("Result exceeds max_rows. Filter or aggregate further before collecting.", call. = FALSE)
  out
}
