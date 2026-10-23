test_that("source summary counts records, people, months, gaps and repeats locally", {
  x <- data.frame(
    source_table = c("a", "a", "a", "a", "b", "b"),
    tran_id = c("t1", "t2", "t2", "t3", "t4", "t1"),
    pid = c("p1", "p1", "p2", "p3", "p4", "p4"),
    adm = as.Date(c("2022-01-05", "2022-01-20", "2022-03-01", NA,
                    "2022-02-10", "2022-02-11")))
  out <- thip_source_summary(x, date = "adm")
  expect_identical(out$source_table, c("a", "b"))
  expect_identical(out$records, c(4, 2))
  expect_identical(out$people, c(3, 1))
  expect_identical(out$first, as.Date(c("2022-01-05", "2022-02-10")))
  expect_identical(out$last, as.Date(c("2022-03-01", "2022-02-11")))
  expect_identical(out$months, c(2, 1))
  expect_identical(out$months_spanned, c(3, 1))
  expect_identical(out$missing_dates, c(1, 0))
  expect_identical(out$repeated_keys, c(1, 0))
  whole <- thip_source_summary(x, date = "adm", by = character())
  expect_identical(whole$records, 6)
  expect_identical(whole$repeated_keys, 2)
  expect_named(whole, c("records", "people", "first", "last", "months",
                        "months_spanned", "missing_dates", "repeated_keys"))
})

test_that("source summary rejects unusable arguments", {
  x <- data.frame(source_table = "a", tran_id = "t", pid = "p",
                  adm = as.Date("2022-01-01"), raw = "20220101")
  expect_error(thip_source_summary(list(), "adm"), "data frame")
  expect_error(thip_source_summary(x, "missing"), "date, key and person")
  expect_error(thip_source_summary(x, "raw"), "Date column")
  expect_error(thip_source_summary(x, "adm", by = "adm"), "by must")
  expect_error(thip_source_summary(x, "adm", by = "nope"), "by must")
  expect_error(thip_source_summary(x, "adm", key = c("tran_id", "pid")),
               "date, key and person")
})
