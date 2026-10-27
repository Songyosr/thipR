adbc_stub <- function() methods::new("ThipAdbcConnection", inner = NULL, info = list())

test_that("ADBC is recognized and is the default backend", {
  expect_identical(.thip_dialect(adbc_stub()), "clickhouse")
  expect_identical(eval(formals(thip_connect)$backend), c("adbc", "http"))
  expect_identical(eval(formals(thip_setup)$backend), c("adbc", "http"))
  expect_identical(dbplyr::dbplyr_edition(adbc_stub()), 2L)
})

test_that("ADBC quotes strings and identifiers consistently with the current backend", {
  skip_if_not_installed("ClickHouseHTTP")
  old <- .new_connection(test_config())
  new <- adbc_stub()
  strings <- c("plain", "O'Brien", "back\\slash", "ภาษาไทย", NA_character_)
  expect_identical(as.character(DBI::dbQuoteString(new, strings)),
                   as.character(DBI::dbQuoteString(old, strings)))
  ids <- c("ordinary", "a`b", "with space", "back\\slash")
  expect_identical(as.character(DBI::dbQuoteIdentifier(new, ids)),
                   as.character(DBI::dbQuoteIdentifier(old, ids)))
  expect_error(DBI::dbQuoteIdentifier(new, NA_character_), "cannot be missing")
  expect_identical(as.character(DBI::dbQuoteIdentifier(new, DBI::Id(schema="s", table="t"))), "`s`.`t`")
})

test_that("ADBC uses semantic temporal types without guessing unsigned meanings", {
  con <- adbc_stub()
  expect_match(as.character(dbplyr::translate_sql(as.Date(raw), con=con)), "toDate32", fixed=TRUE)
  expect_match(as.character(dbplyr::translate_sql(as.POSIXct(raw), con=con)), "toDateTime64", fixed=TRUE)
  expect_match(as.character(dbplyr::translate_sql(as.POSIXct(raw), con=con)), "'UTC'", fixed=TRUE)
  x <- data.frame(u16=12L, u32=34, count=42)
  info <- data.frame(name=names(x),type=c("uint16","uint32","uint64"))
  expect_identical(.adbc_validate_result(x,info),x)
})

test_that("unsafe UInt64 output fails before rounded values escape", {
  info <- data.frame(name="id",type="uint64")
  expect_error(.adbc_validate_result(data.frame(id=2^64),info),"toString",fixed=TRUE)
  expect_error(.adbc_validate_result(data.frame(id=2^53),info),"precision boundary")
  expect_equal(.adbc_validate_result(data.frame(id=c(NA,2^53-1)),info)$id,c(NA,2^53-1))
  exact <- data.frame(id="18446744073709551615")
  expect_identical(.adbc_validate_result(exact,data.frame(name="id",type="string")),exact)
})

test_that("unsupported writes and preparation fail without opening a result", {
  con <- adbc_stub()
  expect_error(DBI::dbSendQuery(con,"CREATE TABLE anything (x Int32)"),"SELECT/WITH")
  expect_error(DBI::dbSendQuery(con,"SELECT 1",immediate=FALSE),"Prepared")
  expect_error(DBI::dbSendQuery(con,"SELECT ?",params=list(1)),"Prepared")
})


test_that("connections default to ADBC and retain explicit HTTP selection", {
  local_mocked_bindings(
    .new_adbc_connection = function(config, driver) adbc_stub(),
    .new_connection = function(config) structure(list(), class = "http_stub"),
    .verify_connection = function(...) TRUE)
  connect <- function(...) thip_connect(host="example.invalid",user="test",
    password="synthetic", ...)
  expect_s4_class(connect(), "ThipAdbcConnection")
  expect_s3_class(connect(backend="http"), "http_stub")
})
