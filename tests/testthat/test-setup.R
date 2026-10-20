test_that("setup saves a private file and loads only its connection values", {
  path <- local_setup_files()
  existing <- c("# Existing user settings", "UNRELATED_SYNTHETIC=keep-me", "",
                "THIP_DB_EXTRA=keep-this-too")
  writeLines(existing, path)
  withr::local_envvar(UNRELATED_SYNTHETIC = "current-session-value")
  local_mocked_bindings(.connect_checked = function(...) stop("Unexpected DB access"))
  returned <- NULL
  expect_message(returned <- withVisible(setup_fixture(path)), "verification was skipped")
  expect_false(returned$visible)
  expect_identical(returned$value, normalizePath(path))
  expect_identical(Sys.getenv(names(setup_values())), setup_values())
  expect_identical(Sys.getenv("UNRELATED_SYNTHETIC"), "current-session-value")
  expect_identical(head(readLines(path), length(existing)), existing)
  expect_identical(as.integer(file.info(path)$mode), strtoi("600", 8L))
  expect_identical(list.files(dirname(path), all.files = TRUE, no.. = TRUE), ".Renviron")
})

test_that("default target honors R_ENVIRON_USER without touching real startup files", {
  path <- local_setup_files()
  withr::local_envvar(c(setup_values(), R_ENVIRON_USER = path))
  expect_message(thip_setup(verify = FALSE), "saved")
  expect_true(file.exists(path))
})

test_that("a fresh R process reads saved credentials literally at startup", {
  path <- local_setup_files()
  password <- " synthetic 'double\" \\slash $literal # = UTF8-\u03b1 "
  expect_message(setup_fixture(path, password = password), "saved")
  expected <- setup_values(); expected[["THIP_DB_PASSWORD"]] <- password
  expected_file <- file.path(dirname(path), "expected.rds")
  script <- file.path(dirname(path), "check-startup.R")
  saveRDS(expected, expected_file)
  writeLines(c("expected <- readRDS(commandArgs(TRUE)[1])",
               "stopifnot(identical(Sys.getenv(names(expected)), expected))",
               "cat('STARTUP_OK\\n')"), script)
  Sys.unsetenv(names(expected))
  withr::local_envvar(R_ENVIRON_USER = path)
  output <- system2(file.path(R.home("bin"), "Rscript"),
                    c("--no-init-file", "--no-site-file", shQuote(script),
                      shQuote(expected_file)), stdout = TRUE, stderr = TRUE)
  expect_null(attr(output, "status"))
  expect_identical(output, "STARTUP_OK")
})

test_that("replacement is explicit and removes duplicate THIP assignments only", {
  path <- local_setup_files()
  before <- c("# Keep", ' THIP_DB_USER = "old"', "OTHER=untouched",
              'THIP_DB_USER="duplicate"', 'THIP_DB_PASSWORD="old-secret"')
  writeLines(before, path)
  local_mocked_bindings(.connect_checked = function(...) stop("Unexpected DB access"))
  expect_error(setup_fixture(path, verify = TRUE), "overwrite = TRUE", fixed = TRUE)
  expect_identical(readLines(path), before)
  expect_identical(Sys.getenv("THIP_DB_USER"), "")
  expect_message(setup_fixture(path, overwrite = TRUE), "saved")
  after <- readLines(path)
  expect_identical(after[1:2], c("# Keep", "OTHER=untouched"))
  expect_equal(sum(grepl("^THIP_DB_USER=", after)), 1L)
  expect_equal(sum(grepl("^THIP_DB_PASSWORD=", after)), 1L)
  expect_false(any(grepl("old-secret|duplicate", after)))
})

test_that("verification uses the shared SELECT path and closes its connection", {
  path <- local_setup_files()
  captured <- character(); disconnected <- FALSE
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    captured <<- c(captured, statement)
    data.frame(database = "example", readonly = 1L, max_seconds = 60L,
               max_threads = 2L, max_rows = 10000L)
  }, .package = "DBI")
  local_mocked_bindings(dbDisconnect = function(con, ...) {
    expect_s4_class(con, "DBIConnection")
    disconnected <<- TRUE
  }, .package = "DBI")
  expect_message(setup_fixture(path, verify = TRUE), "saved")
  expect_length(captured, 1L)
  expect_true(startsWith(captured, "SELECT "))
  expect_true(disconnected)
})

test_that("failed verification preserves disk and environment without echoing secrets", {
  path <- local_setup_files()
  before <- c("OTHER=keep", "THIP_DB_USER=old-user")
  writeLines(before, path)
  withr::local_envvar(THIP_DB_USER = "current-user")
  local_mocked_bindings(.verify_connection = function(...) stop("synthetic-secret"))
  err <- tryCatch(setup_fixture(path, verify = TRUE, overwrite = TRUE), error = identity)
  expect_match(conditionMessage(err), "verification failed")
  expect_false(grepl("synthetic-secret", conditionMessage(err), fixed = TRUE))
  expect_null(conditionCall(err))
  expect_identical(readLines(path), before)
  expect_identical(Sys.getenv("THIP_DB_USER"), "current-user")
})

test_that("unsupported values and invalid config fail before writing or connecting", {
  path <- local_setup_files()
  local_mocked_bindings(.connect_checked = function(...) stop("Unexpected DB access"))
  for (password in c("synthetic\nsecret", "synthetic\rsecret", "${SECRET}", "ends\\")) {
    err <- tryCatch(setup_fixture(path, password = password, verify = TRUE), error = identity)
    expect_match(conditionMessage(err), "cannot be safely stored")
    expect_false(grepl(password, conditionMessage(err), fixed = TRUE))
    expect_false(file.exists(path))
  }
  expect_error(setup_fixture(path, host = "https://example.invalid"), "hostname")
  expect_error(setup_fixture(path, overwrite = NA), "overwrite")
  expect_error(setup_fixture(path, verify = 1), "verify")
  expect_false(file.exists(path))
  expect_identical(Sys.getenv("THIP_DB_PASSWORD"), "")
})

test_that("literal quoting round-trips representative credential punctuation", {
  path <- local_setup_files()
  withr::local_seed(817L)
  alphabet <- c(letters, LETTERS, 0:9, "'", '"', "\\", "$", "#", "=", " ")
  secrets <- c("single'quote", 'double"quote', "slashes\\\\mid", "$plain", " padded ",
               replicate(40, paste0(sample(alphabet, 24, TRUE), collapse = "")))
  secrets <- secrets[!endsWith(secrets, "\\")]
  for (secret in secrets) {
    writeLines(paste0("THIP_DB_PASSWORD=", thipR:::.environ_quote(secret)), path)
    readRenviron(path)
    expect_identical(Sys.getenv("THIP_DB_PASSWORD"), secret)
  }
})

test_that("symlinks and non-file targets are refused without changing their contents", {
  path <- local_setup_files()
  original <- file.path(dirname(path), "original")
  writeLines("OTHER=keep", original)
  expect_true(file.symlink(original, path))
  expect_error(setup_fixture(path), "symbolic link")
  expect_identical(readLines(original), "OTHER=keep")
  unlink(path)
  expect_true(file.symlink(file.path(dirname(path), "missing"), path))
  expect_error(setup_fixture(path), "symbolic link")
  unlink(path)
  dir.create(path)
  expect_error(setup_fixture(path), "regular file")
})

test_that("write failure and concurrent edits preserve the target and session", {
  path <- local_setup_files()
  writeLines("OTHER=original", path)
  target <- thipR:::.environ_target(path)
  writeLines("OTHER=edited", path)
  expect_error(thipR:::.write_user_environ(target, "OTHER=replace"), "Could not safely save")
  expect_identical(readLines(path), "OTHER=edited")
  local_mocked_bindings(file.rename = function(...) FALSE, .package = "base")
  expect_error(setup_fixture(path), "Could not safely save")
  expect_identical(readLines(path), "OTHER=edited")
  expect_identical(Sys.getenv("THIP_DB_PASSWORD"), "")
  expect_identical(list.files(dirname(path), all.files = TRUE, no.. = TRUE), ".Renviron")
})

test_that("IRkernel masked input is used and secrets are not printed or returned", {
  path <- local_setup_files()
  shadow <- new.env(parent = emptyenv())
  shadow$getPass <- function(prompt) "synthetic-masked-secret"
  attach(shadow, name = "jupyter:irkernel")
  withr::defer(detach("jupyter:irkernel"))
  result <- NULL
  messages <- capture.output(
    printed <- capture.output(result <- withVisible(setup_fixture(path, password = ""))),
    type = "message")
  expect_identical(Sys.getenv("THIP_DB_PASSWORD"), "synthetic-masked-secret")
  expect_false(result$visible)
  expect_false(any(grepl("synthetic-masked-secret", c(printed, messages, result$value), fixed = TRUE)))
})

test_that("cancelled or unsupported password prompts never write a file", {
  path <- local_setup_files()
  local_mocked_bindings(.setup_password = function(...) NULL)
  expect_error(setup_fixture(path, password = ""), "Input unavailable or cancelled")
  expect_false(file.exists(path))
  expect_identical(Sys.getenv("THIP_DB_PASSWORD"), "")
})

test_that("new saved values are consumed by the existing connection with argument overrides", {
  path <- local_setup_files()
  expect_message(setup_fixture(path), "saved")
  seen <- NULL
  local_mocked_bindings(.connect_checked = function(config) { seen <<- config; TRUE })
  expect_true(thip_connect())
  expect_identical(seen$host, "example.invalid")
  expect_identical(seen$password, "synthetic-secret")
  expect_true(thip_connect(user = "explicit-user"))
  expect_identical(seen$user, "explicit-user")
  expect_identical(Sys.getenv("THIP_DB_USER"), "synthetic-user")
})
