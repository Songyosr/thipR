.window_id_kind <- function(x, label, missing_ok = FALSE) {
  kind <- if (is.character(x) && is.null(dim(x))) "character" else
    if (is.numeric(x) && !is.object(x) && is.null(dim(x))) "numeric" else "invalid"
  if (kind == "invalid")
    stop(label, " must be character or plain numeric IDs (no implicit coercion).", call. = FALSE)
  invalid <- if (kind == "character") !nzchar(trimws(x)) else
    !is.finite(x) | abs(x) > 2^53 | x != trunc(x)
  invalid[is.na(x)] <- !missing_ok
  if (any(invalid)) stop(label, " contains missing, blank, nonfinite or inexact IDs.", call. = FALSE)
  kind
}

.window_dates <- function(x, label, missing_ok = FALSE) {
  if (!inherits(x, "Date") || !is.null(dim(x)))
    stop(label, " must be Date, not text or a timestamp.", call. = FALSE)
  n <- as.numeric(x)
  bad <- !is.finite(n) | n != trunc(n)
  bad[is.na(n)] <- !missing_ok
  if (any(bad)) stop(label, " contains invalid or missing dates.", call. = FALSE)
  invisible(NULL)
}

.window_spec <- function(data, cohort, date, index, window, by, entry, start, end, bounds) {
  remote <- inherits(data, "tbl_sql")
  if (!remote && !is.data.frame(data)) stop("data must be a data frame or lazy table.", call. = FALSE)
  if (remote && !inherits(dbplyr::remote_con(data), "ClickHouseHTTPConnection"))
    stop("Remote windows currently support ClickHouseHTTP only.", call. = FALSE)
  if (!is.data.frame(cohort)) stop("cohort must be an explicitly collected local data frame.", call. = FALSE)
  fields <- if (remote) colnames(data) else names(data)
  .check_fields(fields, "data")
  .check_fields(names(cohort), "cohort")
  date <- .check_column(fields, date, "date in data")
  entry <- .check_column(names(cohort), entry, "entry in cohort")
  if (entry %in% fields || "__thipr_window_row" %in% fields)
    stop("Entry or internal column name collides with data; explicitly rename/select source columns.", call. = FALSE)
  if (!is.character(by) || length(by) != 1L || is.na(by))
    stop("by must be one shared name or a named source-to-cohort mapping.", call. = FALSE)
  source_id <- if (is.null(names(by))) unname(by) else names(by)
  cohort_id <- unname(by)
  .check_column(fields, source_id, "by source")
  .check_column(names(cohort), cohort_id, "by cohort")
  if (source_id == date || cohort_id == entry)
    stop("Person, date and entry roles must use distinct columns.", call. = FALSE)
  .window_id_kind(cohort[[cohort_id]], "cohort person IDs")
  .window_id_kind(cohort[[entry]], "cohort entry IDs")
  if (anyDuplicated(cohort[[entry]])) stop("cohort entry IDs must be unique.", call. = FALSE)
  if (!is.character(bounds) || length(bounds) != 1L || is.na(bounds) ||
      !bounds %in% c("[)", "[]", "(]", "()"))
    stop("bounds must be one of [), [], (], ().", call. = FALSE)
  explicit <- !is.null(start) || !is.null(end)
  if (explicit) {
    if (!is.null(index) || !is.null(window)) stop("Use start/end OR index/window, not both.", call. = FALSE)
    .check_column(names(cohort), start, "start in cohort")
    .check_column(names(cohort), end, "end in cohort")
    lo <- cohort[[start]]; hi <- cohort[[end]]
  } else {
    if (is.null(index)) index <- "index_date"
    .check_column(names(cohort), index, "index in cohort")
    .window_dates(cohort[[index]], "index")
    if (!is.numeric(window) || length(window) != 2L || anyNA(window) ||
        any(!is.finite(window)) || any(window != trunc(window)) || window[1] > window[2])
      stop("window must contain two ordered integer day offsets.", call. = FALSE)
    lo <- cohort[[index]] + window[1]; hi <- cohort[[index]] + window[2]
  }
  .window_dates(lo, "window start"); .window_dates(hi, "window end")
  if (any(lo > hi)) stop("Window start cannot exceed end.", call. = FALSE)
  if (remote && any(lo < as.Date("1900-01-01") | hi > as.Date("2299-12-31")))
    stop("Remote window dates must be within ClickHouse Date32 range 1900-2299.", call. = FALSE)
  # Normalize all endpoint policies to inclusive integer-day bounds once.
  lower <- as.numeric(lo) + as.integer(substr(bounds, 1L, 1L) == "(")
  upper <- as.numeric(hi) - as.integer(substr(bounds, 2L, 2L) == ")")
  windows <- data.frame(person = cohort[[cohort_id]], lower = lower,
                        upper = upper, ordinal = seq_len(nrow(cohort)))
  if (!remote) .window_check_source(data, source_id, date, cohort[[cohort_id]])
  list(remote = remote, windows = windows, entry_values = cohort[[entry]],
       source_id = source_id, cohort_id = cohort_id, date = date,
       entry = entry, bounds = bounds, fields = fields)
}

.window_check_source <- function(data, id, date, people) {
  source_kind <- .window_id_kind(data[[id]], "source person IDs", missing_ok = TRUE)
  cohort_kind <- .window_id_kind(people, "cohort person IDs")
  if (source_kind != cohort_kind) stop("Source and cohort person IDs have incompatible types.", call. = FALSE)
  .window_dates(data[[date]], "source record date", missing_ok = TRUE)
}
