#' Flag custom ICD conditions in local data
#'
#' Uses normalized distinct codes and sparse aggregation. Dots are removed,
#' case is normalized, and surrounding whitespace is trimmed. Prefixes are
#' literal, not regular expressions. Range (`Q20-Q26`), wildcard (`%`, `_`, `?`),
#' regex and comma/space-separated list notation is rejected with an error rather
#' than silently matching nothing; supply explicit code vectors instead. `*`, `+`
#' and `/` remain literal characters. Codes are not clinically validated.
#' Repeated IDs are combined across rows and columns. Flags are TRUE for any
#' match, FALSE for recorded codes without a match, and NA when all selected
#' codes for an ID are missing/blank. FALSE does not establish disease absence.
#' Input is unchanged. Use thip_filter_icd() to restrict a lazy source, then
#' collect explicitly before local multi-condition flagging.
#'
#' @param data A local data frame. Lazy tables are rejected, never auto-collected.
#' @param conditions Nonempty named list of character code vectors. Empty vectors
#'   are allowed and never match recorded codes; condition names must be unique.
#' @param id Name of the ID column to aggregate by (e.g. encounter or entry).
#' @param cols Character vector naming diagnosis columns (character or factor).
#' @param match Either "exact" (default) or "prefix".
#' @return A data frame with one row per ID, in first-seen order, and logical
#'   condition columns. Empty input yields zero rows with the same schema.
#' @export
#' @examples
#' x <- data.frame(id = c("a", "b"), dx = c("E11.9", NA_character_))
#' icd_flag(x, list(diabetes = "E11"), "id", "dx", match = "prefix")
icd_flag <- function(data, conditions, id, cols, match = c("exact", "prefix")) {
  if (!is.data.frame(data) || inherits(data, "tbl_lazy"))
    stop("icd_flag() requires a local data frame. Use thip_filter_icd() or collect() explicitly.", call. = FALSE)
  match <- match.arg(match)
  .icd_validate_columns(names(data), id, cols)
  definitions <- .icd_prepare_conditions(conditions, id)
  .icd_flag_local(as.data.frame(data), definitions, id, cols, match)
}
