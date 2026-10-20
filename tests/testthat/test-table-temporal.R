test_that("table temporal mappings stay lazy and preserve originals", {
  con <- .new_connection(test_config())
  attr(con, "thipr_database") <- "example"
  local_mocked_bindings(thip_columns = function(...) data.frame(
    name=c("id","raw","stamp"),type=rep("String",3)))
  local_mocked_bindings(dbGetQuery = function(...) stop("Unexpected record query"), .package="DBI")
  original <- thip_table(con,"one")
  expect_identical(as.character(dbplyr::sql_render(original)),
    as.character(dbplyr::sql_render(thip_table(con,"one",dates=NULL))))
  q <- thip_table(con,c("one","two"),.id="source",
    dates=c(adm="raw",day="stamp"),
    date_format=c(adm="%Y%m%d",day="%Y-%m-%d %H:%M:%OS"),
    datetimes=c(at="stamp"),datetime_format="%Y-%m-%d %H:%M:%OS",timezone="Asia/Bangkok")
  sql <- as.character(dbplyr::sql_render(q))
  expect_identical(colnames(q),c("id","raw","stamp","source","adm","day","at"))
  expect_match(sql,"UNION ALL",fixed=TRUE)
  expect_match(sql,"toDate32OrNull",fixed=TRUE)
  expect_match(sql,"toDateTime64OrNull",fixed=TRUE)
  expect_match(sql,"Asia/Bangkok",fixed=TRUE)
  expect_match(sql,"ifNull",fixed=TRUE)
  expect_false(grepl("LIMIT|CREATE|INSERT|UPDATE",sql))
  expect_identical(dbplyr::remote_con(q),con)
  expect_match(as.character(dbplyr::sql_render(dplyr::filter(q,!is.na(adm)))),"WHERE")
  weird <- thip_table(con,"one", dates=c("date ` alias"="raw"), date_format="%Y%m%d")
  expect_true("date ` alias" %in% colnames(weird))
})

test_that("temporal inputs fail explicitly instead of guessing", {
  con <- .new_connection(test_config())
  attr(con,"thipr_database") <- "example"
  local_mocked_bindings(thip_columns=function(...) data.frame(
    name=c("raw","num"),type=c("String","Int32")))
  expect_error(thip_table(con,"one",dates=c(day="raw")),"explicit supported")
  expect_error(thip_table(con,"one",dates="raw",date_format="%Y%m%d"),"named character")
  expect_error(thip_table(con,"one",dates=c(raw="raw"),date_format="%Y%m%d"),"collides")
  expect_error(thip_table(con,"one",dates=c(day="absent"),date_format="%Y%m%d"),"missing")
  expect_error(thip_table(con,"one",dates=c(day="num"),date_format="%Y%m%d"),"String")
  expect_error(thip_table(con,"one",dates=c(day="raw"),date_format="%d/%m/%Y"),"supported")
  expect_error(thip_table(con,"one",date_format="%Y%m%d"),"without columns")
  expect_error(thip_table(con,"one",dates=c(day="raw",day="raw"),date_format="%Y%m%d"),"unique")
  expect_error(thip_table(con,"one",dates=c(a="raw",b="raw"),
    date_format=c("%Y%m%d","%Y-%m-%d")),"named by every")
  expect_error(thip_table(con,"one",dates=c(a="raw"),
    date_format=c(b="%Y%m%d")),"named by every")
  expect_error(thip_table(con,"one",dates=c(a="raw"),date_format="%Y%m%d",
    datetimes=c(a="raw"),datetime_format="%Y%m%d",timezone="UTC"),"unique")
  expect_error(thip_table(con,"one",datetimes=c(at="raw"),datetime_format="%Y%m%d"),"timezone")
  expect_error(thip_table(con,"one",datetimes=c(at="raw"),datetime_format="%Y%m%d",timezone="guess"),"timezone")
  expect_error(thip_table(con,"one",timezone="UTC"),"without datetimes")
})
