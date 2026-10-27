# Explicit opt-in only. All remote SQL here is SELECT or metadata introspection.
# No patient tables are read, and no database objects are created or modified.
stopifnot(identical(Sys.getenv("THIP_RUN_READONLY_TESTS"), "true"))
library(thipR)
library(dplyr)
con <- thip_connect()
tryCatch({
  probe <- DBI::dbGetQuery(con, paste(
    "SELECT '00123' AS code, CAST(NULL AS Nullable(String)) AS missing,",
    "toString(toUInt64('18446744073709551615')) AS large_id,",
    "getSetting('readonly') AS readonly"))
  stopifnot(probe$code == "00123", is.na(probe$missing),
            probe$large_id == "18446744073709551615", probe$readonly == 1)

  # Synthetic rows generated in the query, not a database table.
  query <- tbl(con, dbplyr::sql("SELECT number AS n FROM numbers(5)")) |>
    filter(n < 3) |>
    summarise(total = sum(n, na.rm = TRUE), rows = n())
  result <- thip_collect(query, max_rows = 1L)
  stopifnot(result$total == 3, result$rows == 3)
  bounded <- tbl(con, dbplyr::sql("SELECT number AS n FROM numbers(5)"))
  overflow <- tryCatch({ thip_collect(bounded, max_rows = 2L); FALSE },
                       error = function(e) grepl("exceeds max_rows", conditionMessage(e)))
  stopifnot(overflow)

  tables <- thip_list_tables(con)
  stopifnot(all(c("name", "engine") %in% names(tables)))
  stopifnot(!any(endsWith(tables$name, "_local")))
  if (nrow(tables)) {
    fields <- thip_columns(con, tables$name[[1L]])
    stopifnot(nrow(fields) > 0L, all(c("name", "type") %in% names(fields)))
  }
  # Exercise thip_table against system metadata, never a patient table.
  metadata_con <- thip_connect(database = "system")
  tryCatch({
    metadata_query <- thip_table(metadata_con, "columns") |>
      filter(database == "thip") |>
      summarise(fields = n())
    metadata_result <- thip_collect(metadata_query, max_rows = 1L)
    stopifnot(metadata_result$fields > 0)
  }, finally = thip_disconnect(metadata_con))
  cat("PASS: read-only settings, DBI decoding, dbplyr filter/summarise, bounded collection, table metadata\n")
  cat("Visible distributed/non-local tables:", nrow(tables), "\n")
  cat("R:", as.character(getRversion()), "\n")
  for (p in c("thipR", "DBI", "dbplyr", "ClickHouseHTTP", "arrow"))
    cat(p, as.character(packageVersion(p)), "\n")
}, finally = DBI::dbDisconnect(con))
