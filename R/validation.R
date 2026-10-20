# Shared schema checks; never retrieve source records.
.check_column <- function(fields, column, label) {
  if (!is.character(column) || length(column) != 1L || is.na(column) ||
      !nzchar(column) || !column %in% fields)
    stop(label, " must name one existing column.", call. = FALSE)
  column
}

.check_fields <- function(fields, label) {
  if (!length(fields) || anyNA(fields) || any(!nzchar(fields)) || anyDuplicated(fields))
    stop(label, " must have unique, nonempty column names.", call. = FALSE)
}

