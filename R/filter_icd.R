#' Filter a lazy THIP table by ICD codes
#'
#' Returns matching source rows without collection or aggregation. Uses the
#' same dot/case/whitespace normalization and literal exact/prefix policy as
#' [icd_flag()], including its error for unsupported range, wildcard, regex and
#' list notation. Missing/blank codes do not match. Select source fields before
#' this function if desired; filters remain part of the lazy query.
#' @param data A lazy ClickHouseHTTP table.
#' @param codes Character vector of codes; empty vectors match nothing.
#' @param cols Character vector of diagnosis columns.
#' @param match Either "exact" or "prefix".
#' @return A lazy table retaining source columns and matching row multiplicity.
#' @export
thip_filter_icd <- function(data, codes, cols, match = c("exact", "prefix")) {
  match <- match.arg(match)
  if (!inherits(data, "tbl_sql") || !inherits(dbplyr::remote_con(data), "ClickHouseHTTPConnection"))
    stop("data must be a lazy ClickHouseHTTP table; use icd_flag() for local data.", call. = FALSE)
  fields <- colnames(data)
  .check_fields(fields, "data")
  if (!.icd_valid_names(cols) || !all(cols %in% fields)) stop("cols must name diagnosis columns.", call. = FALSE)
  definitions <- .icd_prepare_conditions(list(condition = codes), "__unused_id")[[1L]]
  con <- dbplyr::remote_con(data)
  sql <- .icd_sql_context(con)
  array <- paste0("[", paste(vapply(cols, sql$normalize, character(1)), collapse = ", "), "]")
  predicate <- .icd_sql_predicate(definitions, match, array, sql, array = TRUE)
  invalid <- paste(vapply(cols, function(x) paste0(sql$base_type(x), " != 'String'"), character(1)), collapse = " OR ")
  guard <- paste0("throwIf(", invalid, ", 'ICD diagnosis columns must be String') = 0")
  query <- paste0("SELECT src.* FROM (", dbplyr::sql_render(data), ") AS src WHERE ", guard, " AND ", predicate)
  dplyr::tbl(con, dbplyr::sql(query), vars = fields)
}
