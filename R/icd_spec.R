# Shared metadata checks run once, before choosing an execution strategy.
.icd_is_remote <- function(data) {
  if (inherits(data, "DBIConnection"))
    stop("A connection does not identify a table. Use thip_table(con, tables = ...) first.", call. = FALSE)
  remote <- inherits(data, "tbl_sql")
  if (!remote && !is.data.frame(data))
    stop("data must be a local data frame or a lazy THIP table.", call. = FALSE)
  remote
}

.icd_valid_names <- function(x) {
  is.character(x) && length(x) > 0L && !anyNA(x) &&
    all(nzchar(trimws(x))) && !anyDuplicated(x)
}

.icd_validate_columns <- function(fields, id, cols) {
  if (anyNA(fields) || any(!nzchar(fields)) || anyDuplicated(fields))
    stop("data must have unique, nonempty column names.", call. = FALSE)
  if (!.icd_valid_names(id) || length(id) != 1L || !id %in% fields)
    stop("id must name one column in data.", call. = FALSE)
  if (!.icd_valid_names(cols) || !all(cols %in% fields) || id %in% cols)
    stop("cols must name distinct diagnosis columns in data, excluding id.", call. = FALSE)
  invisible(NULL)
}

.icd_prepare_conditions <- function(conditions, id) {
  if (!is.list(conditions) || is.data.frame(conditions) || !length(conditions) ||
      !.icd_valid_names(names(conditions)) || id %in% names(conditions))
    stop("conditions must be a nonempty named list with unique names distinct from id.",
         call. = FALSE)
  if (!all(vapply(conditions, function(x) is.character(x) && is.null(dim(x)), logical(1))))
    stop("Each condition must be a character vector of codes.", call. = FALSE)
  definitions <- lapply(conditions, .normalize_icd_codes)
  if (any(vapply(definitions, function(x) anyNA(x) || any(!nzchar(x)), logical(1))))
    stop("Condition definitions cannot contain missing or blank codes.", call. = FALSE)
  .icd_check_notation(definitions)
  lapply(definitions, unique)
}

# Codes are literal. Range, regex, SQL-wildcard and list notation would otherwise
# match nothing and return a plausible all-FALSE result. `*`, `+` and `/` stay
# literal (dagger/asterisk and similar source notations). Unicode dashes and
# spaces are rejected too: ranges copied from documents often use an en dash.
.icd_unsupported_notation <- paste0(
  "[\\-%_?^$|\\\\()\\[\\]{},;\\s",
  "\u00a0\u2010-\u2015\u2212\u202f\u3000\ufe58\ufe63\uff0d\uff0e]")  # PCRE

.icd_check_notation <- function(definitions) {
  bad <- lapply(definitions, function(x) x[grepl(.icd_unsupported_notation, x, perl = TRUE)])
  bad <- bad[lengths(bad) > 0L]
  if (!length(bad)) return(invisible(NULL))
  shown <- paste0(names(bad), ": ", vapply(bad, function(x)
    paste(utils::head(unique(x), 3L), collapse = ", "), character(1)), collapse = "; ")
  stop("Unsupported ICD notation (", shown, "). Codes are matched literally; ",
       "ranges (with a hyphen or a dash), wildcards, regular expressions and ",
       "comma/space-separated lists are not expanded. Supply an explicit reviewed ",
       "vector, e.g. paste0(\"Q\", 20:26) with match = \"prefix\" in place of ",
       "\"Q20-Q26\".", call. = FALSE)
}

# Clean each distinct value once: code columns repeat a few thousand codes many times.
.normalize_icd_codes <- function(x) {
  x <- as.character(x)
  u <- unique(x)
  toupper(gsub(".", "", trimws(u), fixed = TRUE))[match(x, u)]
}

# Row-value checks belong only to the local path. SQL defers them to execution.
.icd_validate_local <- function(data, id, cols) {
  ids <- data[[id]]
  if (!(is.character(ids) || is.factor(ids) || is.numeric(ids)) ||
      !is.null(dim(ids)) || anyNA(ids) ||
      (is.numeric(ids) && (is.object(ids) || any(!is.finite(ids)))) ||
      (!is.numeric(ids) && any(!nzchar(trimws(as.character(ids))))))
    stop("IDs must be character, factor, or plain numeric values without missing or blank IDs.",
         call. = FALSE)
  code_column <- function(x) (is.character(x) || is.factor(x)) && is.null(dim(x))
  if (!all(vapply(data[cols], code_column, logical(1))))
    stop("Diagnosis columns must be character or factor; numeric codes can lose leading zeros.",
         call. = FALSE)
  invisible(NULL)
}
