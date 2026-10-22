skip_if_not_installed("duckdb", "1.3.0")

example_source <- function(con) {
  thip_table(con, tables = c("nhso2565", "nhso2566", "nhso2567"), .id = "source_table",
             dates = c(adm = "dateadm", dsc = "datedsc", death = "death_date"),
             date_format = c(adm = "%Y%m%d", dsc = "%Y%m%d", death = "%Y-%m-%d"))
}

test_that("the example database looks like THIP's inpatient tables", {
  con <- thip_connect_example()
  on.exit(DBI::dbDisconnect(con))
  expect_identical(thip_tables(con)$name, c("nhso2565", "nhso2566", "nhso2567"))
  cols <- thip_columns(con, "nhso2567")
  expect_identical(cols$name[1:3], c("tran_id", "pid", "sex"))
  expect_true(all(cols$type == "String"))
  expect_true(all(c(paste0("sdx", 1:20), paste0("proc", 1:21), "sdx_index") %in% cols$name))
})

test_that("date audit finds the deliberately damaged discharge dates", {
  con <- thip_connect_example()
  on.exit(DBI::dbDisconnect(con))
  audit <- thip_date_audit(example_source(con), c(adm = "dateadm", dsc = "datedsc"))
  expect_identical(audit$n_failed[audit$field == "adm"], 0)
  expect_identical(audit$n_failed[audit$field == "dsc"], 2)
  expect_identical(audit$n_blank[audit$field == "dsc"], 1)
  empty <- thip_date_audit(dplyr::filter(example_source(con), pid == "none"), c(dsc = "datedsc"))
  expect_true(all(unlist(empty[c("n_total", "n_missing", "n_blank", "n_parsed",
                                 "n_failed", "n_inconsistent")]) == 0))
})

test_that("database filters and windows agree with the same steps on local data", {
  con <- thip_connect_example()
  on.exit(DBI::dbDisconnect(con))
  ipd <- example_source(con)
  local <- dplyr::collect(ipd)
  dx <- c("pdx", paste0("sdx", 1:20))

  remote_hits <- dplyr::collect(thip_filter_icd(ipd, "K35", cols = dx, match = "prefix"))
  local_flags <- icd_flag(local, list(appendicitis = "K35"), id = "tran_id", cols = dx, match = "prefix")
  expect_setequal(remote_hits$tran_id, local_flags$tran_id[local_flags$appendicitis %in% TRUE])
  expect_gt(nrow(remote_hits), 100)

  entries <- remote_hits |>
    dplyr::filter(!is.na(dsc)) |>
    dplyr::arrange(pid, adm) |>
    dplyr::distinct(pid, .keep_all = TRUE) |>
    dplyr::transmute(pid, entry_id = tran_id, index_date = dsc)
  remote_window <- dplyr::collect(thip_window(ipd, entries, by = "pid", entry = "entry_id",
    date = "adm", index = "index_date", window = c(1L, 31L)))
  local_window <- dplyr::collect(thip_window(local, entries, by = "pid", entry = "entry_id",
    date = "adm", index = "index_date", window = c(1L, 31L)))
  key <- function(x) sort(paste(x$entry_id, x$tran_id))
  expect_identical(key(remote_window), key(local_window))
  expect_gt(nrow(remote_window), 0)
})

test_that("source summary agrees on the database and in R, and run info counts rows", {
  con <- thip_connect_example()
  on.exit(DBI::dbDisconnect(con))
  ipd <- example_source(con)
  remote <- thip_source_summary(ipd, date = "dsc")
  expect_identical(remote, thip_source_summary(dplyr::collect(ipd), date = "dsc"))
  expect_identical(remote$records, c(3273, 3211, 3343))
  expect_identical(remote$missing_dates, c(1, 2, 0))
  expect_identical(remote$repeated_keys, c(0, 0, 0))
  info <- thip_run_info(con, c("nhso2566", "nhso2567"))
  expect_identical(info$table, c("nhso2566", "nhso2567"))
  expect_identical(info$rows, c(3211, 3343))
  expect_s3_class(info$checked_at, "POSIXct")
  expect_true(all(c("R", "thipR", "dplyr", "dbplyr", "server") %in% names(attr(info, "versions"))))
  expect_error(thip_run_info(con, c("nhso2567", "nhso2567")), "unique")
  expect_error(thip_run_info("con", "nhso2567"), "connection")
})
