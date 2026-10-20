#' Configure database credentials once in your THIP account
#'
#' Save the six `THIP_DB_*` connection variables in a user-level `.Renviron`
#' file and set them in the current R process. Run this inside your remote THIP
#' account, not on the PC used to submit jobs. No Jupyter API token is involved.
#'
#' Existing THIP entries require `overwrite = TRUE`; unrelated lines are
#' preserved. The complete file is replaced atomically with permissions `0600`
#' on Unix (including Linux and macOS). Symbolic-link targets are refused.
#' These are plaintext credentials protected by file permissions, not encryption.
#' Processes running as this user and privileged administrators can read them.
#'
#' Missing host/user values are prompted for interactively. A missing password
#' uses IRkernel's masked `getPass` input, or the optional `askpass` package in
#' another interactive R frontend. Unsupported, cancelled or unattended input
#' fails without writing. Never type a real password as a notebook-cell literal.
#' Arguments also accept values already obtained from a trusted configuration.
#'
#' Normal R startup loads the saved file. A project `.Renviron`, an explicit
#' `R_ENVIRON_USER`, architecture-specific startup files, or `--no-environ` /
#' `--vanilla` can change that behavior. Configure the intended startup path
#' before launching R, or explicitly use `readRenviron(path)` in an existing
#' session. This function does not modify `.Rprofile` or install startup hooks.
#'
#' Verification uses the same SELECT-only connection check as [thip_connect()]
#' and disconnects afterward. Failed verification leaves the file and process
#' environment unchanged. `verify = FALSE` saves syntactically valid settings
#' without contacting the DB; it does not establish that they work.
#'
#' @inheritParams thip_connect
#' @param overwrite Replace existing THIP entries? Defaults to `FALSE`.
#' @param verify Verify the connection before saving? Defaults to `TRUE`.
#' @param path User environment file. Defaults to `R_ENVIRON_USER` when set,
#'   otherwise `~/.Renviron`. Its parent directory must exist. Keep it outside
#'   project repositories. A custom path needs corresponding R startup setup.
#' @return The saved file path, invisibly; never the credentials or a connection.
#' @export
#' @examples
#' \dontrun{
#' # Once, inside THIP; enter credentials through prompts, not cell literals.
#' thip_setup()
#' # In this session or subsequent sessions with normal R startup:
#' con <- thip_connect()
#' DBI::dbDisconnect(con)
#' }
thip_setup <- function(host = Sys.getenv("THIP_DB_HOST"),
                       port = Sys.getenv("THIP_DB_PORT", "8123"),
                       database = Sys.getenv("THIP_DB_NAME", "thip"),
                       user = Sys.getenv("THIP_DB_USER"),
                       password = Sys.getenv("THIP_DB_PASSWORD"),
                       scheme = Sys.getenv("THIP_DB_SCHEME", "http"),
                       overwrite = FALSE, verify = TRUE,
                       path = Sys.getenv("R_ENVIRON_USER", "~/.Renviron")) {
  for (name in c("overwrite", "verify")) {
    value <- get(name)
    if (!is.logical(value) || length(value) != 1L || is.na(value))
      stop(name, " must be TRUE or FALSE.", call. = FALSE)
  }
  target <- .environ_target(path)
  if (any(.thip_environ_lines(target$lines)) && !overwrite)
    stop("THIP configuration already exists. Use overwrite = TRUE to replace it.",
         call. = FALSE)
  host <- .setup_input(host, "Database host: ")
  user <- .setup_input(user, "Database username: ")
  password <- .setup_input(password, "Database password: ", secret = TRUE)
  config <- .connection_config(host, port, database, user, password, scheme,
                               NULL, 2L, NULL)
  values <- c(THIP_DB_HOST = config$host, THIP_DB_PORT = as.character(config$port),
              THIP_DB_NAME = config$database, THIP_DB_USER = config$user,
              THIP_DB_PASSWORD = config$password, THIP_DB_SCHEME = scheme)
  assignments <- paste0(names(values), "=", vapply(values, .environ_quote, character(1)))
  lines <- c(target$lines[!.thip_environ_lines(target$lines)], assignments)
  if (verify) {
    con <- .connect_checked(config)
    DBI::dbDisconnect(con)
  }
  .write_user_environ(target, lines)
  do.call(Sys.setenv, as.list(values))
  message("THIP configuration saved with owner-only permissions and loaded in this session.",
          if (!verify) " Database verification was skipped." else "")
  invisible(target$path)
}

.setup_input <- function(value, prompt, secret = FALSE) {
  if (!identical(value, "") && !is.null(value)) return(value)
  value <- tryCatch({
    if (secret) .setup_password(prompt)
    else if (interactive() || "jupyter:irkernel" %in% search()) readline(prompt)
    else NULL
  }, error = function(e) NULL, warning = function(w) NULL)
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value))
    stop("Input unavailable or cancelled. Run setup interactively inside THIP, ",
         "or supply configuration already loaded in the process environment. ",
         "No configuration was saved.", call. = FALSE)
  value
}

.setup_password <- function(prompt) {
  if ("jupyter:irkernel" %in% search()) {
    # IRkernel supplies a Jupyter password=True prompt on its search path,
    # or patches getPass's exported function when that package is installed.
    fun <- get0("getPass", as.environment("jupyter:irkernel"), inherits = FALSE)
    if (!is.function(fun) && "getPass" %in% loadedNamespaces())
      fun <- getExportedValue("getPass", "getPass")
    if (is.function(fun)) return(fun(prompt))
    return(NULL) # Never substitute an unmasked readline for passwords.
  }
  if (!interactive() || !requireNamespace("askpass", quietly = TRUE)) return(NULL)
  askpass::askpass(prompt)
}
