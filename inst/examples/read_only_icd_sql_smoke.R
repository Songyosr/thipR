# Supply con <- thip_connect(...) before sourcing. Every query below is SELECT
# over literal synthetic rows. No real tables or database writes are used.
stopifnot(inherits(con, "ClickHouseHTTPConnection"))
source(system.file("testdata", "icd-contract.R", package = "thipR", mustWork = TRUE), local = TRUE)
ordered <- .icd_test_order
contract <- .icd_test_fixture()
literal <- function(z) {
  if (is.na(z)) "CAST(NULL AS Nullable(String))" else as.character(DBI::dbQuoteString(con, z))
}
fixture <- contract$data
query <- paste(vapply(seq_len(nrow(fixture)), function(i) {
  paste0("SELECT ", paste(vapply(names(fixture), function(name) {
    paste0(literal(fixture[[name]][i]), " AS ", DBI::dbQuoteIdentifier(con, name))
  }, character(1)), collapse = ", "))
}, character(1)), collapse = " UNION ALL ")
remote <- dplyr::tbl(con, dbplyr::sql(query), vars = names(fixture))
definitions <- contract$conditions
for (strategy in c("row", "distinct")) {
  for (mode in c("exact", "prefix")) {
    local <- ordered(.icd_test_reference(fixture, definitions, contract$id, contract$cols, mode))
    stopifnot(identical(local, ordered(thipR::icd_flag(fixture, definitions, contract$id, contract$cols, mode))))
    lazy <- thipR:::.icd_flag_experimental(remote, definitions, "id", c("dx1", "dx2"), mode, sql_strategy = strategy)
    actual <- ordered(dplyr::collect(lazy))
    if (!identical(local, actual)) {
      print(all.equal(local, actual))
      stop("Local and SQL flags differ")
    }
    stopifnot(is.logical(actual$cannabis))
    subset <- dplyr::filter(lazy, cannabis == TRUE)
    stopifnot(identical(sort(dplyr::collect(subset)$id), sort(local$id[which(local$cannabis)])))
    cat("PASS:", strategy, mode, "flags, normalization, NA, repeats, overlaps, quoting and downstream filtering\n")
  }
  empty <- dplyr::filter(remote, FALSE)
  stopifnot(nrow(dplyr::collect(thipR:::.icd_flag_experimental(empty, definitions, "id", "dx1", sql_strategy = strategy))) == 0L)
  for (bad in c("SELECT 'a' AS id, 123 AS dx", "SELECT '' AS id, 'F12' AS dx",
                "SELECT CAST(NULL AS Nullable(String)) AS id, 'F12' AS dx",
                "SELECT toFloat64('inf') AS id, 'F12' AS dx")) {
    x <- dplyr::tbl(con, dbplyr::sql(bad), vars = c("id", "dx"))
    failed <- inherits(try(dplyr::collect(thipR:::.icd_flag_experimental(x, list(flag = "F12"), "id", "dx", sql_strategy = strategy)), silent = TRUE), "try-error")
    stopifnot(failed)
  }
  cat("PASS:", strategy, "empty input and rejection of numeric diagnosis, blank/null/nonfinite IDs\n")
}
