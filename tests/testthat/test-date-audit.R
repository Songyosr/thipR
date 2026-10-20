test_that("audit distinguishes missing, blank, invalid and valid values", {
  x <- data.frame(source = c(rep("a", 5), rep("b", 3)),
    raw = c(NA, "", " \t\r\n", "20240229", "20230229", "bad", "2024-01-01", ""),
    day = as.Date(c(NA, NA, NA, "2024-02-29", NA, NA, "2024-01-01", "2024-01-02")))
  a <- thip_date_audit(x, c(day="raw"), by="source")
  expect_equal(a$n_total, c(5,3))
  expect_equal(a$n_missing, c(1,0))
  expect_equal(a$n_blank, c(2,1))
  expect_equal(a$n_parsed, c(1,1))
  expect_equal(a$n_failed, c(1,1))
  expect_equal(a$n_inconsistent, c(0,1))
  expect_equal(a$n_missing+a$n_blank+a$n_parsed+a$n_failed, a$n_total)
  expect_equal(thip_date_audit(x[FALSE,],c(day="raw"))$n_total,0)
  expect_equal(nrow(thip_date_audit(x[FALSE,],c(day="raw"),by="source")),0)
  x$clock <- as.POSIXct(x$day)
  expect_equal(nrow(thip_date_audit(x,c(day="raw",clock="raw"))),2)
  x$source[1] <- NA
  expect_equal(sum(thip_date_audit(x,c(day="raw"),by="source")$n_total),8)
  expect_error(thip_date_audit(x,c(day="absent")),"existing")
  expect_error(thip_date_audit(x,"raw"),"map")
  expect_error(thip_date_audit(x,c(raw="raw")),"Date")
  expect_error(thip_date_audit(x,c(day="raw"),by="absent"),"by")
})

test_that("remote audit executes aggregate SELECT only and quotes names", {
  con <- .new_connection(test_config())
  q <- dplyr::tbl(con,dbplyr::sql("SELECT 1"),vars=c("raw weird", "day", "source"))
  captured <- NULL
  local_mocked_bindings(dbGetQuery=function(conn,statement,...) {
    captured <<- statement
    data.frame(field="day", raw_column="raw weird", n_total=0)
  },.package="DBI")
  thip_date_audit(q,c(day="raw weird"),by="source")
  expect_match(captured,"^SELECT")
  expect_match(captured,"countIf",fixed=TRUE)
  expect_match(captured,"GROUP BY",fixed=TRUE)
  expect_match(captured,"`raw weird`",fixed=TRUE)
  expect_false(grepl("CREATE|INSERT|UPDATE|DELETE|LIMIT",captured))
  thip_date_audit(q,c(day="raw weird"))
  expect_false(grepl("``",captured,fixed=TRUE))
  expect_false(grepl("GROUP BY",captured,fixed=TRUE))
  local_mocked_bindings(dbGetQuery=function(...) stop("sensitive backend text"),.package="DBI")
  expect_error(thip_date_audit(q,c(day="raw weird")),"backend details suppressed")
})
