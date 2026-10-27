test_that("discovery uses escaped metadata predicates", {
  con <- thipR:::.new_connection(test_config())
  attr(con, "thipr_database") <- "example"
  statements <- character()
  local_mocked_bindings(dbGetQuery = function(conn, statement, ...) {
    statements <<- c(statements, statement)
    data.frame(name = c("example2026", "example2026_local"),
               engine = c("Distributed", "ReplicatedMergeTree"))
  }, .package = "DBI")
  expect_equal(thip_list_tables(con)$name, "example2026")
  expect_equal(nrow(thip_list_tables(con, include_local = TRUE)), 2L)
  expect_identical(thip_tables(con), thip_list_tables(con))
  expect_identical(thip_tables(con, include_local = TRUE),
                   thip_list_tables(con, include_local = TRUE))
  expect_error(thip_list_tables(con, include_local = NA), "TRUE or FALSE")
  expect_error(thip_tables(con, include_local = NA), "TRUE or FALSE")
  thip_columns(con, "quote'test")
  expect_true(all(grepl("^SELECT .* FROM system[.]", statements)))
  expect_match(tail(statements, 1L), "quote\\'test", fixed = TRUE)
  expect_error(thip_table(con, "example2026_local"), "distributed")
})

test_that("upstream dbplyr translation is retained without network access", {
  con <- thipR:::.new_connection(test_config())
  sql <- dbplyr::translate_sql(as.integer(age), as.character(age), con = con)
  expect_match(as.character(sql[[1L]]), "toInt64")
  expect_match(as.character(sql[[2L]]), "toString")
})

test_that("bounded collection rejects overflow instead of silently truncating", {
  query <- dbplyr::lazy_frame(x = 1:5)
  class(query) <- c("tbl_sql", class(query))
  requested <- NULL
  local_mocked_bindings(collect = function(x, ...) {
    requested <<- dbplyr::sql_render(x)
    data.frame(x = 1:3)
  }, .package = "dplyr")
  expect_error(suppressWarnings(thip_collect(query, max_rows = 2L)), "exceeds")
  expect_match(as.character(requested), "LIMIT 3")
  expect_warning(out <- thip_collect(query, max_rows = 3L), "deprecated")
  expect_equal(nrow(out), 3L)
  expect_error(suppressWarnings(thip_collect(data.frame(x = 1))), "lazy")
})

test_that("table unions align fields, preserve provenance and reject incompatible inputs", {
  con <- .new_connection(test_config())
  attr(con, "thipr_database") <- "example"
  local_mocked_bindings(thip_columns = function(con, table) {
    if (table == "absent") return(data.frame(name = character(), type = character()))
    if (table == "bad") return(data.frame(name = c("id", "date"), type = c("String", "String")))
    if (table == "year2") return(data.frame(name = c("date", "id"), type = c("Date", "Nullable(String)")))
    data.frame(name = c("id", "date"), type = c("String", "Date"))
  })
  x <- thip_table(con, tables = c("year1", "year2"), .id = "source_table")
  sql <- as.character(dbplyr::sql_render(x))
  expect_match(sql, "UNION ALL", fixed = TRUE)
  expect_match(sql, "'year1' AS `source_table`", fixed = TRUE)
  expect_identical(colnames(x), c("id", "date", "source_table"))
  expect_identical(dbplyr::remote_con(x), con)
  expect_error(thip_table(con, c("year1", "bad")), "Incompatible")
  expect_error(thip_table(con, c("year1", "year1")), "unique")
  expect_error(thip_table(con, "absent"), "metadata")
  expect_error(thip_table(con, "year1", .id = "id"), "collides")
  expect_warning(thip_table(con, table = "year1"), "deprecated")
})
