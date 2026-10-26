#' @importFrom DBI dbGetInfo dbIsValid dbDisconnect dbQuoteIdentifier dbQuoteString
#'   dbSendQuery dbFetch dbColumnInfo dbClearResult dbHasCompleted dbGetRowCount
#'   dbGetStatement
#' @rawNamespace S3method(dbplyr::dbplyr_edition,ThipAdbcConnection)
#' @rawNamespace S3method(dbplyr::db_connection_describe,ThipAdbcConnection)
#' @rawNamespace S3method(dbplyr::sql_translation,ThipAdbcConnection)
NULL

# Experimental optional backend. No registrations on upstream connection classes.
# ClickHouse SQL translations follow the established ClickHouseHTTP 1.1.2
# mappings (GPL-3), with Date32/DateTime64 for Arrow semantic type preservation.
methods::setClass("ThipAdbcConnection", contains = "DBIConnection",
  slots = c(inner = "ANY", info = "list"))
methods::setClass("ThipAdbcResult", contains = "DBIResult",
  slots = c(inner = "ANY", columns = "data.frame"))

.new_adbc_connection <- function(config, driver = NULL) {
  if (!requireNamespace("adbi", quietly = TRUE) ||
      !requireNamespace("adbcdrivermanager", quietly = TRUE))
    stop("The ADBC backend needs adbi, adbcdrivermanager and a ClickHouse ADBC driver.", call. = FALSE)
  enc <- function(x) utils::URLencode(x, reserved = TRUE)
  settings <- config$settings
  query <- paste(paste0(vapply(names(settings), enc, character(1)), "=",
    vapply(settings, enc, character(1))), collapse = "&")
  uri <- paste0(if (config$https) "https" else "http", "://", config$host,
    ":", config$port, "/?", query)
  loaded_driver <- .load_adbc_driver(driver)
  inner <- tryCatch(DBI::dbConnect(
    adbi::adbi(loaded_driver),
    uri = uri, username = config$user, password = config$password,
    bigint = "character"), error = function(e)
      stop("ADBC connection failed; check the driver installation and connection settings. Details suppressed.", call. = FALSE))
  methods::new("ThipAdbcConnection", inner = inner,
    info = list(dbms.name = "ClickHouse", dbname = config$database))
}

methods::setMethod("show", "ThipAdbcConnection", function(object)
  cat("<thipR experimental ClickHouse ADBC connection>\n"))
methods::setMethod("dbGetInfo", "ThipAdbcConnection", function(dbObj, ...) dbObj@info)
methods::setMethod("dbIsValid", "ThipAdbcConnection", function(dbObj, ...)
  DBI::dbIsValid(dbObj@inner))
methods::setMethod("dbDisconnect", "ThipAdbcConnection", function(conn, ...)
  DBI::dbDisconnect(conn@inner))

.adbc_quote <- function(x, delimiter) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub(delimiter, paste0("\\", delimiter), x, fixed = TRUE)
  DBI::SQL(ifelse(is.na(x), "NULL", paste0(delimiter, x, delimiter)))
}
methods::setMethod("dbQuoteIdentifier", c("ThipAdbcConnection", "character"),
  function(conn, x, ...) {
    if (anyNA(x)) stop("Identifiers cannot be missing.", call. = FALSE)
    .adbc_quote(x, "`")
  })
methods::setMethod("dbQuoteString", c("ThipAdbcConnection", "character"),
  function(conn, x, ...) .adbc_quote(x, "'"))

methods::setMethod("dbSendQuery", c("ThipAdbcConnection", "character"),
  function(conn, statement, ..., params = NULL, immediate = TRUE) {
    if (!is.character(statement) || length(statement) != 1L || is.na(statement) ||
        !grepl("^\\s*(SELECT|WITH)\\b", statement, ignore.case = TRUE, perl = TRUE))
      stop("The experimental ADBC backend accepts SELECT/WITH queries only.", call. = FALSE)
    if (!is.null(params) || identical(immediate, FALSE))
      stop("Prepared statements are not supported by this experimental backend.", call. = FALSE)
    inner <- tryCatch(DBI::dbSendQuery(conn@inner, statement,
      immediate = TRUE, ...), error = function(e)
        stop("ADBC SELECT failed; backend details suppressed.", call. = FALSE))
    # adbi releases its stream after the final fetch. Retain schema beforehand
    # so validation and dbColumnInfo still work on empty/exhausted results.
    columns <- tryCatch(DBI::dbColumnInfo(inner), error = function(e) {
      DBI::dbClearResult(inner)
      stop("ADBC result schema unavailable; backend details suppressed.", call. = FALSE)
    })
    methods::new("ThipAdbcResult", inner = inner, columns = columns)
  })

.adbc_validate_result <- function(x, info) {
  # nanoarrow/adbi currently returns UInt64 as double even with bigint=character.
  # Fail closed before a rounded ID can escape. Safe-range counts stay numeric.
  for (name in info$name[info$type == "uint64"]) {
    v <- x[[name]]
    if (is.numeric(v) && any(!is.na(v) & (!is.finite(v) | v >= 2^53)))
      stop("UInt64 result reaches the double precision boundary; select toString(column) for exact values.", call. = FALSE)
  }
  x
}
methods::setMethod("dbFetch", "ThipAdbcResult", function(res, n = -1, ...) {
  x <- DBI::dbFetch(res@inner, n = n, ...)
  .adbc_validate_result(x, res@columns)
})
methods::setMethod("dbColumnInfo", "ThipAdbcResult", function(res, ...)
  res@columns)
methods::setMethod("dbClearResult", "ThipAdbcResult", function(res, ...)
  DBI::dbClearResult(res@inner, ...))
methods::setMethod("dbHasCompleted", "ThipAdbcResult", function(res, ...)
  DBI::dbHasCompleted(res@inner, ...))
methods::setMethod("dbIsValid", "ThipAdbcResult", function(dbObj, ...)
  DBI::dbIsValid(dbObj@inner, ...))
methods::setMethod("dbGetRowCount", "ThipAdbcResult", function(res, ...)
  DBI::dbGetRowCount(res@inner, ...))
methods::setMethod("dbGetStatement", "ThipAdbcResult", function(res, ...)
  DBI::dbGetStatement(res@inner, ...))

dbplyr_edition.ThipAdbcConnection <- function(con) 2L
db_connection_describe.ThipAdbcConnection <- function(con, ...) "ClickHouse [ADBC prototype]"
sql_translation.ThipAdbcConnection <- function(con) {
  dbplyr::sql_variant(
    scalar = dbplyr::sql_translator(.parent = dbplyr::base_scalar,
      `^` = dbplyr::sql_prefix("pow"),
      as.character = dbplyr::sql_prefix("toString"),
      as.integer = dbplyr::sql_prefix("toInt64"),
      as.double = dbplyr::sql_prefix("toFloat64"),
      as.numeric = dbplyr::sql_prefix("toFloat64"),
      as.logical = dbplyr::sql_prefix("toUInt8"),
      as.Date = dbplyr::sql_prefix("toDate32"),
      as_date = dbplyr::sql_prefix("toDate32"),
      as.POSIXct = function(x) dbplyr::build_sql("toDateTime64(", x, ", 3, 'UTC')"),
      as_datetime = function(x) dbplyr::build_sql("toDateTime64(", x, ", 3, 'UTC')"),
      Sys.Date = function() dbplyr::sql("toDate32(today())"),
      Sys.time = function() dbplyr::sql("now64(3, 'UTC')")),
    aggregate = dbplyr::sql_translator(.parent = dbplyr::base_agg,
      sd = dbplyr::sql_aggregate("stddevSamp", "sd"),
      var = dbplyr::sql_aggregate("varSamp", "var")),
    window = dbplyr::sql_translator(.parent = dbplyr::base_win,
      sd = dbplyr::win_aggregate("stddevSamp"),
      var = dbplyr::win_aggregate("varSamp")))
}
