test_that("SQL dispatch stays lazy and retains upstream filtering", {
  con <- thipR:::.new_connection(test_config())
  local_mocked_bindings(dbGetQuery = function(...) stop("Unexpected query"), .package = "DBI")
  q <- dplyr::tbl(con, dbplyr::sql("SELECT id, dx FROM example WHERE year = 2026"),
                  vars = c("id", "dx"))
  x <- .icd_flag_experimental(q, list(flag = c(" f12.1 ", "T407"), empty = character()), "id", "dx")
  expect_s3_class(x, "tbl_sql")
  expect_identical(colnames(x), c("id", "flag", "empty"))
  expect_identical(dbplyr::remote_con(x), con)
  sql <- as.character(dbplyr::sql_render(x))
  expect_match(sql, "WHERE year = 2026", fixed = TRUE)
  expect_match(sql, "'F121'", fixed = TRUE)
  expect_match(sql, "hasAny", fixed = TRUE)
  expect_match(sql, "Nullable(Bool)", fixed = TRUE)
  expect_match(sql, "GROUP BY s.__thipr_id", fixed = TRUE)
  after <- dplyr::filter(x, flag == TRUE)
  expect_match(as.character(dbplyr::sql_render(after)), "WHERE")
})

test_that("SQL literals and identifiers use DBI escaping", {
  con <- thipR:::.new_connection(test_config())
  q <- dplyr::tbl(con, dbplyr::sql("SELECT 1"), vars = c("id weird", "dx weird"))
  definition <- "F12'OR'1'='1"
  x <- .icd_flag_experimental(q, setNames(list(definition), "custom flag"),
                 "id weird", "dx weird", "prefix")
  sql <- as.character(dbplyr::sql_render(x))
  expect_match(sql, as.character(DBI::dbQuoteString(con, definition)), fixed = TRUE)
  expect_match(sql, "src.`dx weird`", fixed = TRUE)
  expect_match(sql, "AS `custom flag`", fixed = TRUE)
  expect_match(sql, "startsWith(code,", fixed = TRUE)
  expect_identical(colnames(x), c("id weird", "custom flag"))
})

test_that("remote inputs validate definitions and reject ambiguous dispatch", {
  con <- thipR:::.new_connection(test_config())
  q <- dplyr::tbl(con, dbplyr::sql("SELECT 1"), vars = c("id", "dx"))
  expect_error(.icd_flag_experimental(con, list(flag = "A"), "id", "dx"), "does not identify a table")
  expect_error(.icd_flag_experimental(q, list(flag = " "), "id", "dx"), "missing or blank")
  expect_error(.icd_flag_experimental(q, list(flag = "A"), "id", "unknown"), "cols must")
  expect_error(.icd_flag_experimental(q, list(id = "A"), "id", "dx"), "distinct from id")
  other <- dbplyr::tbl_sql("DBI", dbplyr::src_dbi(dbplyr::simulate_dbi()), "example",
                          vars = c("id", "dx"))
  expect_error(.icd_flag_experimental(other, list(flag = "A"), "id", "dx"), "ClickHouseHTTP tables only")
})

test_that("distinct-code SQL scans its source once and preserves lazy composition", {
  con <- thipR:::.new_connection(test_config())
  local_mocked_bindings(dbGetQuery = function(...) stop("Unexpected query"), .package = "DBI")
  q <- dplyr::tbl(con, dbplyr::sql("SELECT id, dx FROM synthetic_source WHERE year = 2026"),
                  vars = c("id", "dx"))
  for (mode in c("exact", "prefix")) {
    x <- .icd_flag_experimental(q, list(`custom flag` = " f12.1 ", empty = character()),
                  "id", "dx", mode, sql_strategy = "distinct")
    sql <- as.character(dbplyr::sql_render(x))
    expect_s3_class(x, "tbl_sql")
    expect_identical(colnames(x), c("id", "custom flag", "empty"))
    expect_identical(dbplyr::remote_con(x), con)
    expect_equal(sum(gregexpr("synthetic_source", sql, fixed = TRUE)[[1]] > 0L), 1L)
    expect_match(sql, "WHERE year = 2026", fixed = TRUE)
    expect_match(sql, "groupUniqArray(__thipr_id)", fixed = TRUE)
    expect_match(sql, "emptyArrayToSingle", fixed = TRUE)
    expect_match(sql, "'F121'", fixed = TRUE)
    expect_match(sql, "AS `custom flag`", fixed = TRUE)
    expect_match(as.character(dbplyr::sql_render(dplyr::filter(x, empty == FALSE))), "WHERE")
  }
  expect_error(.icd_flag_experimental(q, list(flag = "A"), "id", "dx", sql_strategy = "invalid"), "arg")
})

test_that("SQL strategy does not change local sparse flags", {
  x <- data.frame(id = c("a", "b"), dx = c("F12.1", NA_character_))
  expect_identical(.icd_flag_experimental(x, list(flag = "F12"), "id", "dx", "prefix"),
    .icd_flag_experimental(x, list(flag = "F12"), "id", "dx", "prefix", sql_strategy = "distinct"))
})

test_that("SQL-shaped definitions with spaces or comment markers stop before SQL", {
  con <- thipR:::.new_connection(test_config())
  q <- dplyr::tbl(con, dbplyr::sql("SELECT 1"), vars = c("id", "dx"))
  expect_error(.icd_flag_experimental(q, list(flag = "F12') OR 1 = 1 --"), "id", "dx"),
               "Unsupported ICD notation")
})
