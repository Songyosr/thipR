setup_values <- function() {
  c(THIP_DB_HOST = "example.invalid", THIP_DB_PORT = "8123",
    THIP_DB_NAME = "example", THIP_DB_USER = "synthetic-user",
    THIP_DB_PASSWORD = "synthetic-secret", THIP_DB_SCHEME = "https")
}

setup_fixture <- function(path, ...) {
  args <- list(host = "example.invalid", port = "8123", database = "example",
               user = "synthetic-user", password = "synthetic-secret",
               scheme = "https", path = path, verify = FALSE)
  updates <- list(...)
  args[names(updates)] <- updates
  do.call(thip_setup, args)
}

local_setup_files <- function(env = parent.frame()) {
  skip_if(.Platform$OS.type != "unix", "Owner-only persistence is Unix-only")
  root <- withr::local_tempdir(.local_envir = env)
  withr::local_envvar(stats::setNames(rep(NA_character_, 6L), names(setup_values())),
                     .local_envir = env)
  file.path(root, ".Renviron")
}
