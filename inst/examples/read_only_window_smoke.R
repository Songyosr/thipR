# Supply a read-only con. All data below are synthetic SQL literals.
# No patient tables, temporary tables, writes or settings mutations are used.
source(system.file("testdata", "window-contract.R", package = "thipR"), local = TRUE)
fixture <- .window_test_fixture()
quote_value <- function(x) {
  if (inherits(x, "Date")) {
    if (is.na(x)) "CAST(NULL AS Nullable(Date32))" else
      paste0("toDate32(", DBI::dbQuoteString(con, format(x)), ")")
  } else if (is.character(x)) {
    if (is.na(x)) "CAST(NULL AS Nullable(String))" else as.character(DBI::dbQuoteString(con, x))
  } else as.character(DBI::dbQuoteLiteral(con, x))
}
literal_source <- function(df) {
  sql <- paste(vapply(seq_len(nrow(df)), function(i) {
    paste0("SELECT ", paste(vapply(names(df), function(n) {
      paste0(quote_value(df[[n]][i]), " AS ", DBI::dbQuoteIdentifier(con, n))
    }, character(1)), collapse = ", "))
  }, character(1)), collapse = " UNION ALL ")
  dplyr::tbl(con, dbplyr::sql(sql), vars = names(df))
}
remote <- literal_source(fixture$data)
checks <- 0L
for (bounds in c("[)", "[]", "(]", "()")) {
  expected <- .window_test_reference(fixture$data, fixture$cohort, bounds)
  plan <- .window_test_plan(remote, fixture$cohort, bounds)
  for (size in c(1L, 2L, 100L)) {
    actual <- dplyr::collect(plan, batch_size = size)
    # The driver may decode integer width differently; compare numeric values.
    actual$record <- as.integer(actual$record)
    stopifnot(identical(.window_test_order(actual), expected),
              isTRUE(attr(actual, "window_report")$complete),
              attr(actual, "window_report")$unmatched_entries == 1)
    checks <- checks + 1L
  }
}
for (bounds in c("[)", "[]", "(]", "()")) {
  plan <- .window_test_plan(remote, fixture$cohort, bounds, c(0, 0))
  actual <- dplyr::collect(plan, batch_size = 2)
  actual$record <- as.integer(actual$record)
  stopifnot(identical(.window_test_order(actual),
    .window_test_reference(fixture$data, fixture$cohort, bounds, c(0, 0))))
  checks <- checks + 1L
}
empty <- dplyr::collect(.window_test_plan(remote, fixture$cohort[FALSE, ]))
stopifnot(nrow(empty) == 0, inherits(empty[["observed date"]], "Date"))
empty_source <- dplyr::filter(remote, FALSE)
stopifnot(nrow(dplyr::collect(.window_test_plan(empty_source, fixture$cohort))) == 0)
checks <- checks + 2L
for (mode in c("exact", "prefix")) {
  definitions <- list(hit = c("F12", "F20"))
  local <- thipR::icd_flag(fixture$data, definitions, "record", "dx", mode)
  expected <- fixture$data$record[fixture$data$record %in% local$record[which(local$hit)]]
  actual <- dplyr::collect(thipR::thip_filter_icd(remote, definitions$hit, "dx", mode))
  stopifnot(identical(sort(as.integer(actual$record)), sort(expected)))
  checks <- checks + 1L
}
# Numeric person IDs and named source fields exercise a separate SQL literal path.
nd <- data.frame(pid = c(1L, 2L, 1L), date = as.Date("2020-01-01") + 0:2)
nc <- data.frame(pid = 1L, entry_id = 42L, index_date = as.Date("2020-01-02"))
actual <- dplyr::collect(thipR::thip_window(literal_source(nd), nc, date = "date", window = c(-2, 2)))
stopifnot(identical(sort(actual$date), sort(nd$date[c(1, 3)])),
          all(actual$entry_id == 42L), all(actual$pid == 1))
checks <- checks + 1L
cat("WINDOW_SMOKE_PASS:", checks, "\n")
