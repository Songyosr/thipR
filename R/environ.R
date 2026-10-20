# Persistence helpers use standard R .Renviron syntax, not an R-code file.
.thip_environ_lines <- function(lines) {
  grepl("^[[:space:]]*THIP_DB_(HOST|PORT|NAME|USER|PASSWORD|SCHEME)[[:space:]]*=", lines)
}

.environ_quote <- function(value) {
  # R expands ${...} before processing quotes, and a trailing backslash
  # escapes the closing quote. Refuse unsupported input rather than change it.
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      grepl("[\r\n]", value) || grepl("${", value, fixed = TRUE) ||
      endsWith(value, "\\") || nchar(value, type = "bytes") > 16000L)
    stop("A configuration value cannot be safely stored in .Renviron. ",
         "Multiline values, ${...}, trailing backslashes and values over ",
         "16000 bytes are unsupported; use externally supplied environment ",
         "variables instead. No configuration was saved.", call. = FALSE)
  paste0('"', gsub('"', '\\"', value, fixed = TRUE), '"')
}

.environ_target <- function(path) {
  if (.Platform$OS.type != "unix")
    stop("Persistent setup currently requires Unix file permissions. ",
         "Run thip_setup() inside your THIP account.", call. = FALSE)
  path <- path.expand(.required_string(path, "path"))
  if (!dir.exists(dirname(path)))
    stop("The configuration parent directory must already exist.", call. = FALSE)
  path <- file.path(normalizePath(dirname(path), mustWork = TRUE), basename(path))
  link <- Sys.readlink(path)
  if ((!is.na(link) && nzchar(link)) ||
      (file.exists(path) && !utils::file_test("-f", path)))
    stop("Configuration must be a regular file, not a directory or symbolic link.",
         call. = FALSE)
  exists <- file.exists(path)
  lines <- if (exists) readLines(path, warn = FALSE, encoding = "UTF-8") else character()
  list(path = path, exists = exists, lines = lines)
}

.write_user_environ <- function(target, lines) {
  # Do not make a plaintext backup. A private sibling file is renamed only
  # after writing succeeds; failures leave the original file in place.
  old_mask <- Sys.umask("0077")
  on.exit(Sys.umask(old_mask), add = TRUE)
  temp <- tempfile(".thipr-environ-", tmpdir = dirname(target$path))
  on.exit(unlink(temp), add = TRUE)
  tryCatch({
    if (!file.create(temp) || !Sys.chmod(temp, "0600")) stop("create")
    writeLines(lines, temp, useBytes = TRUE)
    if (as.integer(file.info(temp)$mode) != strtoi("600", base = 8L)) stop("mode")
    current <- .environ_target(target$path)
    if (!identical(current, target)) stop("changed")
    if (!file.rename(temp, target$path)) stop("replace")
  }, warning = function(w) {
    stop("Could not safely save configuration; check file permissions and ",
         "concurrent edits. No configuration was saved.", call. = FALSE)
  }, error = function(e) {
    stop("Could not safely save configuration; check file permissions and ",
         "concurrent edits. No configuration was saved.", call. = FALSE)
  })
  invisible(target$path)
}
