#' Install a ClickHouse ADBC driver in a user folder
#'
#' Run `thipR::thip_install_driver()` once after installing thipR. This function
#' downloads and installs the prebuilt ClickHouse ADBC driver for you, using
#' `dbc` with signature verification. No manual driver download or installation
#' is needed. It does not build Arrow C++ or install the R arrow package.
#'
#' This is an interim setup step for platforms that do not yet supply the driver.
#' The extra command may become unnecessary when the platform supplies it;
#' updated setup guidance will follow.
#'
#' @param path Destination folder. Defaults to the thipR user-data directory
#'   returned by `tools::R_user_dir("thipR", "data")`, under `adbc`. Choose a
#'   persistent home or project folder; avoid temporary or disposable storage.
#' @param dbc Optional path to an existing `dbc` executable. Otherwise uses one
#'   on PATH, or installs dbc 0.3.1 in a private Python virtual environment inside
#'   `path`. Bootstrapping requires Python 3 with venv and pip, and network access.
#' @return Invisibly, the absolute path to `clickhouse.toml`. Also selects this
#'   manifest for the current session via option `thipR.adbc_driver`.
#' @details
#' A working existing manifest is reused without downloading or upgrading.
#' Fresh installations use ClickHouse driver 0.1.1. An existing broken manifest
#' raises an error; use a new destination to repair without replacing a driver
#' that another session may be using.
#'
#' The helper is an interim bootstrap, not a temporary installation. Files survive
#' R restarts when the folder remains available. Persistence across notebook
#' server rebuilds depends on the platform's storage policy. The helper does not
#' change R library paths or startup files.
#'
#' The default location is found automatically in later R sessions. For a custom
#' location, pass that folder as `driver` to [thip_connect()] and [thip_setup()],
#' or set `options(thipR.adbc_driver = "path/to/clickhouse.toml")` in each session.
#' Use `backend = "adbc"`; the HTTP backend remains available separately through
#' the optional ClickHouseHTTP package. If the platform supplies the ADBC driver,
#' this installation helper can be skipped.
#'
#' @export
#' @examples
#' \dontrun{
#' thip_install_driver()  # Downloads and installs the driver; run once
#' con <- thip_connect(backend = "adbc")
#'
#' # Optional: choose a persistent folder yourself.
#' thip_install_driver(path = "~/my-drivers")
#' # In subsequent sessions:
#' con <- thip_connect(backend = "adbc", driver = "~/my-drivers")
#' }
thip_install_driver <- function(path = .adbc_driver_dir(), dbc = NULL) {
  .driver_string(path, "path")
  path <- path.expand(path)
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE))
    stop("Cannot create driver destination.", call. = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  # dbc interprets ADBC_DRIVER_PATH as a list of directories.
  if (grepl(.Platform$path.sep, path, fixed = TRUE))
    stop("Driver destination cannot contain the path-list separator.", call. = FALSE)
  manifest <- file.path(path, "clickhouse.toml")
  lock <- file.path(path, ".thipr-install-lock")
  if (!dir.create(lock, showWarnings = FALSE))
    stop("Driver installation is locked. Wait for the other installer; remove ",
         "the .thipr-install-lock folder only if no installer is running.", call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  if (!file.exists(manifest)) {
    executable <- .driver_installer(path, dbc)
    old <- Sys.getenv("ADBC_DRIVER_PATH", unset = NA_character_)
    on.exit(if (is.na(old)) Sys.unsetenv("ADBC_DRIVER_PATH") else
      Sys.setenv(ADBC_DRIVER_PATH = old), add = TRUE)
    Sys.setenv(ADBC_DRIVER_PATH = path)
    .driver_command(executable, c("install", "clickhouse=0.1.1"),
                    "ClickHouse driver download")
  }
  if (!file.exists(manifest))
    stop("Installer did not create clickhouse.toml in the destination.", call. = FALSE)
  .validate_adbc_manifest(manifest)
  options(thipR.adbc_driver = manifest)
  message("ClickHouse ADBC driver ready: ", manifest,
          "\nKeep this folder on persistent storage. Use backend = \"adbc\" to connect.")
  invisible(manifest)
}

.adbc_driver_dir <- function() file.path(tools::R_user_dir("thipR", "data"), "adbc")

.driver_string <- function(x, label) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x))
    stop(label, " must be a non-empty string.", call. = FALSE)
}

.resolve_adbc_driver <- function(driver = NULL) {
  if (is.null(driver)) driver <- getOption("thipR.adbc_driver")
  if (is.null(driver)) {
    local <- file.path(.adbc_driver_dir(), "clickhouse.toml")
    driver <- if (file.exists(local)) local else "clickhouse"
  }
  .driver_string(driver, "driver")
  driver <- path.expand(driver)
  if (dir.exists(driver)) driver <- file.path(driver, "clickhouse.toml")
  driver
}

.validate_adbc_manifest <- function(manifest) {
  tryCatch(adbcdrivermanager::adbc_driver(manifest), error = function(e)
    stop("The driver manifest exists but cannot be loaded. Check platform ",
         "compatibility, or install into a new folder. Existing files were kept.",
         call. = FALSE))
  invisible(TRUE)
}

.driver_command <- function(command, args, label) {
  log <- tempfile("thipr-driver-", fileext = ".log")
  on.exit(unlink(log), add = TRUE)
  status <- tryCatch(suppressWarnings(system2(command, shQuote(args),
    stdout = log, stderr = log)), error = function(e) 1L)
  if (!identical(as.integer(status), 0L))
    stop(label, " failed. Check network access, write permissions and platform ",
         "support. For Python bootstrap, Python 3 with venv and pip is required; ",
         "alternatively pass dbc = '/path/to/dbc'.", call. = FALSE)
  invisible(NULL)
}

.driver_installer <- function(path, dbc) {
  if (!is.null(dbc)) {
    .driver_string(dbc, "dbc")
    dbc <- path.expand(dbc)
    if (file.exists(dbc)) return(normalizePath(dbc, winslash = "/"))
    found <- Sys.which(dbc)
    if (nzchar(found)) return(unname(found))
    stop("Cannot find the supplied dbc executable.", call. = FALSE)
  }
  found <- Sys.which("dbc")
  if (nzchar(found)) return(unname(found))
  venv <- file.path(path, ".thipr-dbc-0.3.1")
  windows <- identical(.Platform$OS.type, "windows")
  bin <- file.path(venv, if (windows) "Scripts" else "bin")
  python <- file.path(bin, if (windows) "python.exe" else "python")
  executable <- file.path(bin, if (windows) "dbc.exe" else "dbc")
  if (!file.exists(python)) {
    candidates <- Sys.which(c("python3", "python"))
    candidates <- candidates[nzchar(candidates)]
    if (!length(candidates))
      stop("Python 3 is needed to bootstrap dbc; alternatively pass dbc = ",
           "'/path/to/dbc'.", call. = FALSE)
    .driver_command(unname(candidates[[1]]), c("-m", "venv", venv),
                    "Private Python environment creation")
  }
  if (!file.exists(executable))
    .driver_command(python, c("-m", "pip", "install", "--disable-pip-version-check",
      "--no-cache-dir", "dbc==0.3.1"), "dbc installation")
  executable
}

.load_adbc_driver <- function(driver = NULL) {
  selected <- .resolve_adbc_driver(driver)
  tryCatch(adbcdrivermanager::adbc_driver(selected), error = function(e)
    stop('Cannot load the ClickHouse ADBC driver. Run thip_install_driver() ',
         'once, or thip_install_driver(path = "~/my-drivers") and pass ',
         'driver = "~/my-drivers". If already installed, check the path and ',
         'platform compatibility. Setup does not download drivers automatically.',
         call. = FALSE))
}
