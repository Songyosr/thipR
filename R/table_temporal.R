# Parsing policy, validation and SQL composition are separate from source union.
.temporal_mapping <- function(mapping, format, fields, types, label) {
  if (is.null(mapping)) {
    if (!is.null(format)) stop(label, " format supplied without columns.", call. = FALSE)
    return(list())
  }
  if (!is.character(mapping) || !length(mapping) || anyNA(mapping) ||
      is.null(names(mapping)) || anyNA(names(mapping)) ||
      any(!nzchar(trimws(names(mapping)))) || anyDuplicated(names(mapping)))
    stop(label, " must be a named character mapping with unique output names.", call. = FALSE)
  if (any(!mapping %in% fields)) stop(label, " source column is missing.", call. = FALSE)
  if (any(names(mapping) %in% fields)) stop(label, " output collides with a source column.", call. = FALSE)
  if (!is.null(types) && any(types[unname(mapping)] != "String"))
    stop(label, " source columns must be String.", call. = FALSE)
  allowed <- c("%Y%m%d", "%Y-%m-%d", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M:%OS")
  if (!is.character(format) || !length(format) || anyNA(format) || any(!format %in% allowed))
    stop(label, " requires an explicit supported format.", call. = FALSE)
  if (length(format) == 1L && is.null(names(format))) format <- rep(format, length(mapping)) else {
    if (is.null(names(format)) || anyNA(names(format)) || anyDuplicated(names(format)) ||
        !setequal(names(format), names(mapping)))
      stop(label, " formats must be named by every output column.", call. = FALSE)
    format <- format[names(mapping)]
  }
  Map(function(column, fmt) list(column = unname(column), format = unname(fmt)), mapping, format)
}

.temporal_sql <- function(con, column, format, datetime, timezone) {
  q <- function(x) as.character(DBI::dbQuoteString(con, x))
  x <- paste0("trimBoth(", DBI::dbQuoteIdentifier(con, column), ")")
  compact <- format == "%Y%m%d"
  clock <- format %in% c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M:%OS")
  pattern <- if (compact) "^[0-9]{8}$" else if (!clock) "^[0-9]{4}-[0-9]{2}-[0-9]{2}$" else
    paste0("^[0-9]{4}-[0-9]{2}-[0-9]{2} ([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]",
      if (format == "%Y-%m-%d %H:%M:%OS") "([.][0-9]{1,3})?" else "", "$")
  day <- if (compact) paste0("concat(substring(", x, ",1,4),'-',substring(", x,
    ",5,2),'-',substring(", x, ",7,2))") else paste0("substring(", x, ",1,10)")
  parsed_day <- paste0("toDate32OrNull(", day, ")")
  valid <- paste0("match(", x, ",", q(pattern), ") AND substring(", day,
    ",1,4) BETWEEN '1900' AND '2299' AND toString(", parsed_day, ") = ", day)
  parsed <- parsed_day
  if (datetime) {
    stamp <- if (clock) x else paste0("concat(", day, ", ' 00:00:00')")
    parsed <- paste0("toDateTime64OrNull(", stamp, ",3,", q(timezone), ")")
    expected <- if (clock) paste0("substring(", x, ",1,19)") else stamp
    valid <- paste0(valid, " AND formatDateTime(", parsed,
      ",'%Y-%m-%d %H:%i:%S',", q(timezone), ") = ", expected)
  }
  paste0("if(ifNull(", valid, ",0),", parsed, ",NULL)")
}

.table_temporal <- function(data, dates = NULL, date_format = NULL,
                            datetimes = NULL, datetime_format = NULL,
                            timezone = NULL, types = NULL) {
  fields <- colnames(data)
  days <- .temporal_mapping(dates, date_format, fields, types, "dates")
  times <- .temporal_mapping(datetimes, datetime_format, fields, types, "datetimes")
  if (anyDuplicated(c(names(days), names(times)))) stop("Temporal output names must be unique.", call. = FALSE)
  if (length(times)) {
    if (!is.character(timezone) || length(timezone) != 1L || is.na(timezone) ||
        !timezone %in% c("UTC", OlsonNames()))
      stop("datetimes require an explicit valid IANA timezone.", call. = FALSE)
  } else if (!is.null(timezone)) stop("timezone supplied without datetimes.", call. = FALSE)
  if (!length(days) && !length(times)) return(data)
  con <- dbplyr::remote_con(data)
  if (length(times) && identical(.thip_dialect(con), "example"))
    stop("Datetime conversion is not available in the example database.", call. = FALSE)
  expressions <- c(lapply(days, function(s) .temporal_sql(con, s$column, s$format, FALSE, NULL)),
                   lapply(times, function(s) .temporal_sql(con, s$column, s$format, TRUE, timezone)))
  additions <- paste0(unlist(expressions), " AS ", DBI::dbQuoteIdentifier(con, names(expressions)))
  query <- paste0("SELECT src.*, ", paste(additions, collapse = ", "),
                  " FROM (", dbplyr::sql_render(data), ") AS src")
  dplyr::tbl(con, dbplyr::sql(query), vars = c(fields, names(expressions)))
}
