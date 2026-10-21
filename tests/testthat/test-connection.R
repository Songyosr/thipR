test_that("invalid configuration stops before network access", {
  expect_error(test_config(host = ""), "host")
  expect_error(test_config(host = "https://example.invalid/path"), "hostname")
  expect_error(test_config(database = "example&readonly=0"), "identifier")
  expect_error(test_config(password = ""), "password")
  expect_error(test_config(port = "8123junk"), "port")
  expect_error(test_config(port = 65536), "port")
  expect_error(test_config(max_rows = Inf), "max_rows")
  expect_error(test_config(max_rows = 2.5), "max_rows")
  expect_error(test_config(max_threads = 0), "max_threads")
  expect_error(test_config(max_seconds = NA_real_), "max_seconds")
  expect_error(test_config(scheme = "ftp"), "scheme")
})

test_that("driver settings are fixed, decimal, and stateless", {
  cfg <- test_config()
  con <- thipR:::.new_connection(cfg)
  expect_s4_class(con, "DBIConnection")
  expect_s4_class(con, "ClickHouseHTTPConnection")
  expect_identical(cfg$settings[["readonly"]], "1")
  expect_identical(cfg$settings[["max_result_rows"]], "100000")
  expect_match(con@settings, "readonly=1(&|$)")
  expect_match(con@settings, "result_overflow_mode=throw")
  expect_true(is.na(con@session))
  expect_true(con@ssl_verifypeer)
  expect_false(con@convert_uint)
  expect_false(grepl(cfg$password, con@settings, fixed = TRUE))
})

test_that("connection verification accepts matching settings and rejects drift", {
  cfg <- test_config()
  con <- thipR:::.new_connection(cfg)
  captured <- character()
  fixture <- data.frame(database = "example", readonly = 1L,
                        max_seconds = 60L, max_threads = 2L, max_rows = 100000L)
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    captured <<- c(captured, statement)
    fixture
  }, .package = "DBI")
  expect_true(thipR:::.verify_connection(con, cfg))
  expect_true(all(grepl("^SELECT ", captured)))
  fixture$readonly <- 0L
  expect_error(thipR:::.verify_connection(con, cfg), "did not confirm")
  fixture$readonly <- 1L
  fixture$max_rows <- 200000L
  expect_error(thipR:::.verify_connection(con, cfg), "did not confirm")
})

test_that("connection failures do not echo backend error contents", {
  local_mocked_bindings(.verify_connection = function(...) {
    stop("synthetic-secret SQL details")
  })
  err <- tryCatch(thip_connect(host = "example.invalid", user = "example",
                              password = "synthetic-secret"), error = identity)
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "verification failed")
  expect_false(grepl("synthetic-secret", conditionMessage(err), fixed = TRUE))
})

test_that("NULL limits inherit without sending an unlimited override", {
  cfg <- test_config(max_rows = NULL, max_seconds = NULL, max_threads = NULL)
  expect_false(any(c("max_result_rows", "max_execution_time", "max_threads") %in% names(cfg$settings)))
  con <- .new_connection(cfg)
  expect_false(grepl("max_result_rows=", con@settings, fixed = TRUE))
  local_mocked_bindings(dbGetQuery = function(...) data.frame(
    database = "example", readonly = 1, max_seconds = 17, max_threads = "auto(8)", max_rows = 123), .package = "DBI")
  expect_true(.verify_connection(con, cfg))
  expect_null(formals(thip_connect)$max_rows)
  expect_null(formals(thip_connect)$max_seconds)
})

test_that("the driver check enforces the minimum, structure and untested warning", {
  desc <- read.dcf(system.file("DESCRIPTION", package = "thipR"), fields = "Imports")[[1]]
  pin <- regmatches(desc, regexpr("ClickHouseHTTP \\([^)]*\\)", desc))
  expect_identical(pin, paste0("ClickHouseHTTP (>= ", thipR:::.tested_clickhouse_version, ")"))
  slots <- thipR:::.clickhouse_slots
  expect_true(all(slots %in% thipR:::.clickhouse_class_slots()))
  expect_true(thipR:::.check_clickhouse_driver("1.1.2", slots))
  expect_no_warning(thipR:::.check_clickhouse_driver("1.1.9", slots))
  expect_error(thipR:::.check_clickhouse_driver("1.0.0", slots), "needs ClickHouseHTTP 1.1.2 or later; found 1.0.0")
  expect_error(thipR:::.check_clickhouse_driver("1.3.0", setdiff(slots, "settings")),
               "changed the connection structure.*missing: settings.*ClickHouseHTTP@1.1.2")
  state <- thipR:::.driver_state
  rm(list = ls(state), envir = state)
  expect_warning(thipR:::.check_clickhouse_driver("1.2.0", slots), "1.2.0 has not been tested")
  expect_no_warning(thipR:::.check_clickhouse_driver("1.2.0", slots))  # once per session
  rm(list = ls(state), envir = state)
})
