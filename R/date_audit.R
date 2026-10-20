#' Audit raw and converted date columns
#'
#' Explicitly execute a date-quality audit on a scoped lazy ClickHouse query or
#' local data frame. No conversion or format guessing is performed: use the raw
#' and converted columns retained by [thip_table()]. Supports dates and datetimes.
#' @param data A lazy ClickHouse table or local data frame.
#' @param columns Named character vector `converted_column = "raw_column"`.
#' @param by Optional character vector of grouping columns, such as source table.
#' @return A local data frame with one row per group and field: `field`,
#'   `raw_column`, `n_total`, `n_missing` (raw NA/NULL), `n_blank` (empty or
#'   whitespace-only text), `n_parsed` (nonblank raw, nonmissing converted),
#'   `n_failed` (nonblank raw, missing converted), and `n_inconsistent`
#'   (missing/blank raw but nonmissing converted). The first four categories
#'   missing, blank, parsed and failed partition n_total. Inconsistency overlaps
#'   missing/blank and indicates the supplied pair needs checking.
#' @details This executes aggregate SELECT queries, one per field, on remote
#'   data; it does not retrieve individual records or write database objects.
#'   Scope data before calling, preferably without filtering on converted dates:
#'   that would remove the parsing failures you want to detect. Grouping by a
#'   person identifier would expose person-level summaries; use source metadata.
#'   No implicit sampling or row limit is applied. Auditing can scan the selected
#'   source once per field. Empty ungrouped input returns zero counts; empty
#'   grouped input has no groups. Existing dplyr grouping is ignored; use by.
#'   A nonmissing converted value does not prove correct calendar interpretation,
#'   clinical plausibility, or source completeness. Local converted columns must
#'   be Date or POSIXct; remote pairs should come from thip_table conversions.
#' @export
thip_date_audit <- function(data, columns, by = character()) {
  remote <- inherits(data, "tbl_sql")
  if (!remote && !is.data.frame(data)) stop("data must be a data frame or lazy SQL table.", call. = FALSE)
  fields <- colnames(data)
  .check_fields(fields, "data")
  if (!is.character(columns) || !length(columns) || anyNA(columns) ||
      is.null(names(columns)) || anyNA(names(columns)) ||
      any(!nzchar(names(columns))) || anyDuplicated(names(columns)) ||
      any(!c(names(columns), unname(columns)) %in% fields))
    stop("columns must map unique converted column names to existing raw columns.", call. = FALSE)
  reserved <- c("field", "raw_column", "n_total", "n_missing", "n_blank",
                "n_parsed", "n_failed", "n_inconsistent")
  if (!is.character(by) || anyNA(by) || anyDuplicated(by) ||
      any(!by %in% fields) || any(by %in% reserved))
    stop("by must name unique existing columns without audit-output name collisions.", call. = FALSE)
  count_names <- reserved[-c(1, 2)]
  one <- function(field, raw) {
    if (remote) {
      con <- dbplyr::remote_con(data)
      qi <- function(x) if (length(x)) as.character(DBI::dbQuoteIdentifier(con, x)) else character()
      qs <- function(x) as.character(DBI::dbQuoteString(con, x))
      r <- qi(raw); p <- qi(field)
      missing <- paste0("isNull(", r, ")")
      blank <- paste0("ifNull(match(", r, ",", qs("^[[:space:]]*$"), "),0)")
      present <- paste0("NOT (", missing, " OR ", blank, ")")
      expressions <- c("count()", paste0("countIf(", missing, ")"),
        paste0("countIf(", blank, ")"),
        paste0("countIf(", present, " AND isNotNull(", p, "))"),
        paste0("countIf(", present, " AND isNull(", p, "))"),
        paste0("countIf((", missing, " OR ", blank, ") AND isNotNull(", p, "))"))
      select <- c(qi(by), paste0(qs(field), " AS field"),
        paste0(qs(raw), " AS raw_column"), paste0(expressions, " AS ", count_names))
      sql <- paste0("SELECT ", paste(select, collapse = ", "), " FROM (",
        dbplyr::sql_render(dplyr::ungroup(data)), ") AS audit_source",
        if (length(by)) paste0(" GROUP BY ", paste(qi(by), collapse = ", ")) else "")
      return(tryCatch(DBI::dbGetQuery(con, sql), error = function(e)
        stop("Date audit query failed; backend details suppressed.", call. = FALSE)))
    }
    r <- data[[raw]]; p <- data[[field]]
    if (!is.character(r) || !(inherits(p, "Date") || inherits(p, "POSIXct")))
      stop("Raw columns must be character and converted columns Date or POSIXct.", call. = FALSE)
    missing <- is.na(r)
    blank <- !missing & grepl("^[[:space:]]*$", r)
    present <- !(missing | blank)
    flags <- data.frame(n_total = rep(1, nrow(data)), n_missing = as.numeric(missing),
      n_blank = as.numeric(blank), n_parsed = as.numeric(present & !is.na(p)),
      n_failed = as.numeric(present & is.na(p)),
      n_inconsistent = as.numeric((missing | blank) & !is.na(p)))
    if (length(by)) {
      grouped <- dplyr::group_by_at(as.data.frame(data)[by], by, .drop = TRUE)
      keys <- as.data.frame(dplyr::group_keys(grouped))
      groups <- dplyr::group_rows(grouped)
    } else { keys <- data.frame(row.names = 1L); groups <- list(seq_len(nrow(data))) }
    sums <- matrix(numeric(), nrow = 0L, ncol = length(count_names))
    if (length(groups)) sums <- do.call(rbind, lapply(groups, function(i) colSums(flags[i, , drop = FALSE])))
    colnames(sums) <- count_names
    cbind(keys, field = rep(field, length(groups)), raw_column = rep(raw, length(groups)), as.data.frame(sums))
  }
  out <- as.data.frame(dplyr::bind_rows(Map(one, names(columns), unname(columns))))
  rownames(out) <- NULL
  out
}
