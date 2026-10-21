.window_sql <- function(plan, windows) {
  con <- dbplyr::remote_con(plan$data)
  ident <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  lit <- function(x) as.character(DBI::dbQuoteLiteral(con, x))
  # Dates are transported as integer days, preserving endpoints without casts.
  values <- paste0(lit(windows$person), ", ",
    format(windows$lower, scientific = FALSE, trim = TRUE), ", ",
    format(windows$upper, scientific = FALSE, trim = TRUE), ", ", windows$ordinal)
  example <- identical(.thip_dialect(con), "example")
  roster <- if (example) {
    paste0("SELECT * FROM (VALUES ", paste0("(", values, ")", collapse = ", "),
           ") AS r(person, lo, hi, ordinal)")
  } else {
    paste0("SELECT t.1 AS person, t.2 AS lo, t.3 AS hi, t.4 AS ordinal FROM ",
           "(SELECT arrayJoin([", paste0("tuple(", values, ")", collapse = ", "), "]) AS t)")
  }
  source <- as.character(dbplyr::sql_render(plan$data))
  day <- paste0("dateDiff('day', toDate32('1970-01-01'), s.", ident(plan$date), ")")
  # Equality join + WHERE supports older ClickHouse versions without inequality ON.
  # ALL is explicit so server join defaults cannot discard overlapping windows.
  paste0("SELECT ", paste0("s.", ident(plan$fields), " AS ", ident(plan$fields), collapse = ", "),
    ", w.ordinal AS __thipr_window_row FROM (", source, ") AS s ",
    if (example) "INNER JOIN" else "ALL INNER JOIN", " (", roster,
    ") AS w ON s.", ident(plan$source_id), " = w.person WHERE ",
    "s.", ident(plan$source_id), " IN (", paste(lit(unique(windows$person)), collapse = ", "), ")",
    " AND s.", ident(plan$date), " >= toDate32(", lit(as.Date(min(windows$lower), origin = "1970-01-01")), ")",
    " AND s.", ident(plan$date), " <= toDate32(", lit(as.Date(max(windows$upper), origin = "1970-01-01")), ")",
    " AND ", day, " >= w.lo AND ", day, " <= w.hi")
}

.window_query <- function(plan, sql = NULL, prototype = FALSE) {
  con <- dbplyr::remote_con(plan$data)
  if (prototype) sql <- paste0("SELECT * FROM (", as.character(dbplyr::sql_render(plan$data)), ") AS src LIMIT 0")
  out <- tryCatch(DBI::dbGetQuery(con, sql), error = function(e)
    stop("Read-only window query failed; backend details suppressed.", call. = FALSE))
  expected <- c(plan$fields, if (!prototype) "__thipr_window_row")
  if (!identical(names(out), expected)) stop("Window result schema did not match the plan.", call. = FALSE)
  .window_check_source(out, plan$source_id, plan$date, plan$windows$person)
  out
}
