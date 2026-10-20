source(system.file("testdata", "window-contract.R", package = "thipR"), local = TRUE)

test_that("date boundaries, names and overlap pairs match an independent reference", {
  f <- .window_test_fixture()
  original <- f
  for (bounds in c("[)", "[]", "(]", "()")) {
    expected <- .window_test_reference(f$data, f$cohort, bounds)
    plan <- .window_test_plan(f$data, f$cohort, bounds)
    for (size in c(1L, 2L, 100L)) {
      actual <- dplyr::collect(plan, batch_size = size)
      expect_identical(.window_test_order(actual), expected)
      expect_identical(actual$index_date, f$data$index_date[match(actual$record, f$data$record)])
      report <- attr(actual, "window_report")
      expect_true(report$complete)
      expect_equal(report$unmatched_entries, 1L)
      expect_equal(sum(report$batches$entries), nrow(f$cohort))
      expect_equal(sum(report$batches$matches), nrow(actual))
    }
  }
  expect_identical(f, original)
  expect_output(print(plan), "Execution pending")
})

test_that("empty and zero-width windows preserve types without invented records", {
  f <- .window_test_fixture()
  for (b in c("[)", "[]", "(]", "()")) {
    actual <- dplyr::collect(.window_test_plan(f$data, f$cohort, b, c(0, 0)))
    expect_identical(.window_test_order(actual), .window_test_reference(f$data, f$cohort, b, c(0, 0)))
  }
  empty <- dplyr::collect(.window_test_plan(f$data, f$cohort[FALSE, ]))
  expect_equal(nrow(empty), 0)
  expect_s3_class(empty[["observed date"]], "Date")
  expect_equal(nrow(attr(empty, "window_report")$batches), 0)
  empty_source <- dplyr::collect(.window_test_plan(f$data[FALSE, ], f$cohort))
  expect_equal(nrow(empty_source), 0)
  expect_equal(attr(empty_source, "window_report")$unmatched_entries, 5)
})

test_that("explicit windows and relative windows agree, independently of input order", {
  f <- .window_test_fixture()
  f$cohort$begin <- f$cohort$anchor - 10
  f$cohort$finish <- f$cohort$anchor + 10
  plan <- thip_window(f$data, f$cohort, date = "observed date",
    by = c(person = "subject"), entry = "entry key", start = "begin", end = "finish")
  expect_identical(.window_test_order(dplyr::collect(plan)), .window_test_reference(f$data, f$cohort))
  shuffled <- .window_test_plan(f$data[nrow(f$data):1, ], f$cohort[nrow(f$cohort):1, ])
  expect_identical(.window_test_order(dplyr::collect(shuffled, batch_size = 1)),
                   .window_test_reference(f$data, f$cohort))
})

test_that("ambiguous or invalid mappings are rejected before execution", {
  f <- .window_test_fixture()
  bad <- f$cohort; bad[["entry key"]][2] <- bad[["entry key"]][1]
  expect_error(.window_test_plan(f$data, bad), "unique")
  bad <- f$cohort; bad$subject[1] <- NA
  expect_error(.window_test_plan(f$data, bad), "missing")
  bad <- f$cohort; bad$anchor[1] <- NA
  expect_error(.window_test_plan(f$data, bad), "missing")
  bad <- f$cohort; bad$anchor <- as.character(bad$anchor)
  expect_error(.window_test_plan(f$data, bad), "Date")
  bad <- f$data; bad[["entry key"]] <- "collision"
  expect_error(.window_test_plan(bad, f$cohort), "collides")
  bad <- f$data; bad$person <- seq_len(nrow(bad))
  expect_error(.window_test_plan(bad, f$cohort), "incompatible")
  expect_error(.window_test_plan(f$data, f$cohort, window = c(1, -1)), "ordered")
  expect_error(.window_test_plan(f$data, f$cohort, window = c(0, 1.5)), "integer")
  expect_error(.window_test_plan(f$data, f$cohort, bounds = "bad"), "bounds")
  expect_error(thip_window(f$data, f$cohort, "missing", window = c(-1, 1)), "existing")
  expect_error(thip_window(f$data, f$cohort, "observed date", start = "anchor", end = "anchor",
    window = c(0, 1), by = c(person = "subject"), entry = "entry key"), "not both")
  plan <- .window_test_plan(f$data, f$cohort)
  expect_error(dplyr::collect(plan, batch_size = 0), "positive")
  expect_error(dplyr::collect(plan, n = 2), "Unused")
})

test_that("a failed middle batch never returns partial success or backend details", {
  f <- .window_test_fixture()
  original <- .window_local
  calls <- 0L
  local_mocked_bindings(.window_local = function(plan, windows) {
    calls <<- calls + 1L
    if (calls == 2L) stop("secret identifier in backend response")
    original(plan, windows)
  })
  error <- tryCatch(dplyr::collect(.window_test_plan(f$data, f$cohort), batch_size = 1), error = identity)
  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "batch 2/4 failed")
  expect_false(grepl("secret identifier", conditionMessage(error), fixed = TRUE))
  expect_equal(calls, 2L)
})

test_that("numeric IDs retain their types and source values", {
  d <- data.frame(pid = c(1L, 2L, 1L), date = as.Date("2020-01-01") + 0:2)
  c <- data.frame(pid = 1L, entry_id = 42L, index_date = as.Date("2020-01-02"))
  out <- dplyr::collect(thip_window(d, c, date = "date", window = c(-2, 2)))
  expect_identical(out$pid, c(1L, 1L))
  expect_identical(out$entry_id, c(42L, 42L))
  expect_identical(out$date, d$date[c(1, 3)])
})

test_that("remote construction stays lazy and generated SQL uses explicit safe projections", {
  f <- .window_test_fixture()
  con <- .new_connection(test_config())
  local_mocked_bindings(dbGetQuery = function(...) stop("Unexpected query"), .package = "DBI")
  remote <- dplyr::tbl(con, dbplyr::sql("SELECT * FROM synthetic WHERE selected = 1"), vars = names(f$data))
  plan <- .window_test_plan(remote, f$cohort)
  sql <- .window_sql(plan, plan$windows)
  expect_match(sql, "ALL INNER JOIN", fixed = TRUE)
  expect_match(sql, "WHERE selected = 1", fixed = TRUE)
  expect_match(sql, "s.`observed date` AS `observed date`", fixed = TRUE)
  expect_match(sql, as.character(DBI::dbQuoteString(con, "quote'person")), fixed = TRUE)
  expect_false(grepl("CREATE|INSERT|DROP|SET ", sql))
})
