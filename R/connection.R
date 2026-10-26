#' Connect to THIP using the standard ClickHouse DBI backend
#'
#' Configuration is read from the remote R process environment by default.
#' Use [thip_setup()] once inside THIP to save user-level configuration, unless
#' the platform already supplies it. Explicit arguments override the environment.
#' Run this inside the THIP network. The PC's Jupyter API token is not a
#' database credential and is not used by this package.
#'
#' Every request includes readonly=1 and overflow mode `throw`. Optional resource
#' limits complement, rather than replace,
#' operator-provisioned read-only database permissions. Do not alter the
#' returned object's slots or use SQL to override these settings.
#'
#' Both backends support `DBI::dbGetQuery()` and `dplyr::tbl()`.
#' The HTTP backend returns a `ClickHouseHTTPConnection`. Its setup uses the driver's
#' exported connection class because its `dbConnect()` method issues USE and
#' SET statements. This factory sends SELECT only; all query transport, result
#' decoding and dbplyr translation remain in the upstream driver. Tested with
#' ClickHouseHTTP 1.1.2: older versions are refused, a newer version that changes
#' the connection structure stops with a clear message, and other newer versions
#' warn once per session.
#'
#' The experimental ADBC backend wraps adbi and a prebuilt ClickHouse driver.
#' It accepts immediate SELECT/WITH queries, without prepared parameters. It
#' returns signed 64-bit integers as text and rejects unsigned 64-bit results
#' at or above 2^53; select `toString(column)` for exact values. Use
#' [thip_table()] for Date32/DateTime64 conversions; raw legacy Date/DateTime
#' fields may arrive as numbers. See [thip_install_driver()] for installation.
#'
#' @param host Database hostname; defaults to THIP_DB_HOST.
#' @param port HTTP port; defaults to THIP_DB_PORT or 8123.
#' @param database Database name; defaults to THIP_DB_NAME or thip.
#' @param user Database username; defaults to THIP_DB_USER.
#' @param password Database password; defaults to THIP_DB_PASSWORD.
#' @param scheme Either http or https; defaults to THIP_DB_SCHEME or http.
#' @param max_seconds Server query time limit, in seconds; NULL inherits the platform setting.
#' @param max_threads Maximum server threads per query, default 2; NULL inherits the platform setting.
#' @param max_rows Server result row limit; NULL inherits the platform setting.
#'   Explicit limits must be positive; overflow raises an error, never truncation.
#' @param backend Connection backend. The existing HTTP backend remains the
#'   default; use `"adbc"` for the experimental backend without the R arrow package.
#' @param driver ADBC driver name, manifest or shared-library path, or a directory
#'   containing `clickhouse.toml`. NULL finds the driver installed by
#'   [thip_install_driver()] or a platform-provided `clickhouse` driver.
#' @return A verified DBI connection.
#' @importClassesFrom DBI DBIConnection DBIResult
#' @export
#' @examples
#' \dontrun{
#' con <- thip_connect()
#' DBI::dbGetQuery(con, "SELECT 1 AS connection_ok")
#' DBI::dbDisconnect(con)
#' }
thip_connect <- function(host = Sys.getenv("THIP_DB_HOST"),
                         port = Sys.getenv("THIP_DB_PORT", "8123"),
                         database = Sys.getenv("THIP_DB_NAME", "thip"),
                         user = Sys.getenv("THIP_DB_USER"),
                         password = Sys.getenv("THIP_DB_PASSWORD"),
                         scheme = Sys.getenv("THIP_DB_SCHEME", "http"),
                         max_seconds = NULL, max_threads = 2L,
                         max_rows = NULL, backend = c("http", "adbc"),
                         driver = NULL) {
  backend <- match.arg(backend)
  config <- .connection_config(host, port, database, user, password, scheme,
                               max_seconds, max_threads, max_rows)
  if (backend == "http") .connect_checked(config) else .connect_checked(config, backend, driver)
}

.connect_checked <- function(config, backend = "http", driver = NULL) {
  con <- if (backend == "adbc") .new_adbc_connection(config, driver) else .new_connection(config)
  tryCatch(.verify_connection(con, config), error = function(e) {
    try(DBI::dbDisconnect(con), silent = TRUE)
    stop("THIP connection verification failed. Check the platform configuration, ",
         "network, read-only permissions and driver compatibility. ",
         "Server error details are omitted to avoid exposing credentials or SQL.",
         call. = FALSE)
  })
  attr(con, "thipr_database") <- config$database
  attr(con, "thipr_max_rows") <- if ("max_result_rows" %in% names(config$settings))
    as.integer(config$settings[["max_result_rows"]]) else NULL
  con
}

.required_string <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
    stop(label, " must be a nonempty string. Configure THIP_DB_* in R.", call. = FALSE)
  x
}

.positive_integer <- function(x, label, maximum = .Machine$integer.max) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x) || x > maximum)
    stop(label, " must be a positive integer <= ", maximum, ".", call. = FALSE)
  as.integer(x)
}

.connection_config <- function(host, port, database, user, password, scheme,
                               max_seconds, max_threads, max_rows) {
  host <- .required_string(host, "host")
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9.-]*$", host))
    stop("host must be a hostname without a URL, path or credentials.", call. = FALSE)
  database <- .required_string(database, "database")
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", database))
    stop("database must be a simple database identifier.", call. = FALSE)
  user <- .required_string(user, "user")
  password <- .required_string(password, "password")
  if (!is.character(scheme) || length(scheme) != 1L || is.na(scheme) ||
      !scheme %in% c("http", "https")) stop("scheme must be http or https.", call. = FALSE)
  if (is.character(port) && length(port) == 1L && !is.na(port) &&
      grepl("^[0-9]+$", port)) port <- as.numeric(port)
  port <- .positive_integer(port, "port", 65535)
  optional_limit <- function(x, name) if (is.null(x)) NULL else .positive_integer(x, name)
  limits <- list(max_execution_time = optional_limit(max_seconds, "max_seconds"),
                 max_threads = optional_limit(max_threads, "max_threads"),
                 max_result_rows = optional_limit(max_rows, "max_rows"))
  limits <- limits[!vapply(limits, is.null, logical(1))]
  settings <- c(list(database = database, readonly = "1"), limits,
                list(result_overflow_mode = "throw", timeout_overflow_mode = "throw",
                     wait_end_of_query = "1"))
  settings <- vapply(settings, function(x) if (is.numeric(x))
    format(x, scientific = FALSE, trim = TRUE) else x, character(1))
  list(host = host, port = port, database = database, user = user,
       password = password, https = scheme == "https", settings = settings)
}

# Minimum matches the Suggests entry in DESCRIPTION (tested). Releases in the
# tested minor series are accepted silently; newer ones warn once per session.
.tested_clickhouse_version <- "1.1.2"
# Slots thipR sets directly when building the connection object.
.clickhouse_slots <- c("host", "port", "user", "password", "https", "ssl_verifypeer",
                       "host_path", "session", "convert_uint", "extended_headers",
                       "reset_handle", "settings")
.driver_state <- new.env(parent = emptyenv())

.clickhouse_version <- function() as.character(utils::packageVersion("ClickHouseHTTP"))

.clickhouse_class_slots <- function()
  methods::slotNames(methods::getClass("ClickHouseHTTPConnection",
                                       where = asNamespace("ClickHouseHTTP")))

.check_clickhouse_driver <- function(installed = .clickhouse_version(),
                                     slots = .clickhouse_class_slots()) {
  tested <- package_version(.tested_clickhouse_version)
  found <- package_version(installed)
  if (found < tested)
    stop("thipR needs ClickHouseHTTP ", tested, " or later; found ", found,
         ". Update it with pak::pkg_install(\"ClickHouseHTTP\"), then restart R.",
         call. = FALSE)
  missing <- setdiff(.clickhouse_slots, slots)
  if (length(missing))
    stop("ClickHouseHTTP ", found, " changed the connection structure thipR ",
         "relies on (missing: ", paste(missing, collapse = ", "), "). Install the ",
         "tested version with pak::pkg_install(\"ClickHouseHTTP@", tested,
         "\"), restart R, and please report this issue.", call. = FALSE)
  same_minor <- identical(unlist(found)[1:2], unlist(tested)[1:2])
  if (!same_minor && !isTRUE(.driver_state$warned)) {
    .driver_state$warned <- TRUE
    warning("ClickHouseHTTP ", found, " has not been tested with thipR (tested: ",
            tested, "). Please report any connection problems.", call. = FALSE)
  }
  invisible(TRUE)
}

.new_connection <- function(config) {
  if (!requireNamespace("ClickHouseHTTP", quietly = TRUE))
    stop('The HTTP backend needs ClickHouseHTTP. Install it with ',
         'install.packages("ClickHouseHTTP"), or use backend = "adbc" with ',
         'thip_install_driver().', call. = FALSE)
  .check_clickhouse_driver()
  # Public exported S4 class; do not patch the driver namespace or shared dbplyr state.
  con <- methods::new("ClickHouseHTTPConnection",
    host = config$host, port = config$port, user = config$user,
    password = local({ secret <- config$password; function() invisible(secret) }),
    https = config$https, ssl_verifypeer = TRUE, host_path = NA_character_,
    session = NA_character_, convert_uint = FALSE, extended_headers = list(),
    reset_handle = FALSE,
    settings = paste(names(config$settings), config$settings, sep = "=", collapse = "&"))
  if (dbplyr::dbplyr_edition(con) != 2L)
    stop("Expected the driver dbplyr edition 2 backend.", call. = FALSE)
  con
}

.verify_connection <- function(con, config) {
  checked <- DBI::dbGetQuery(con, paste(
    "SELECT currentDatabase() AS database, toString(getSetting('readonly')) AS readonly,",
    "toString(getSetting('max_execution_time')) AS max_seconds,",
    "toString(getSetting('max_threads')) AS max_threads,",
    "toString(getSetting('max_result_rows')) AS max_rows"))
  expected <- c(database = config$database, readonly = "1")
  mapping <- c(max_seconds = "max_execution_time", max_threads = "max_threads", max_rows = "max_result_rows")
  for (label in names(mapping)) if (mapping[[label]] %in% names(config$settings))
    expected[label] <- config$settings[[mapping[[label]]]]
  actual <- vapply(checked, function(x) as.character(x[[1L]]), character(1))
  if (nrow(checked) != 1L || !all(names(expected) %in% names(actual)) ||
      !identical(unname(actual[names(expected)]), unname(expected)))
    stop("The server did not confirm the requested connection limits.", call. = FALSE)
  invisible(TRUE)
}

#' Release a THIP connection
#'
#' Uses the driver's DBI disconnect method. This does not stop Jupyter or
#' modify database tables. Treat the connection as unusable afterward.
#' @param con A DBI connection returned by [thip_connect()].
#' @return Invisibly TRUE.
#' @export
thip_disconnect <- function(con) {
  .Deprecated("DBI::dbDisconnect")
  DBI::dbDisconnect(con)
  invisible(TRUE)
}
