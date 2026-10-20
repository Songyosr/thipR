.icd_flag_sql <- function(data, definitions, id, cols, match, strategy = "row") {
  con <- dbplyr::remote_con(data)
  if (!inherits(con, "ClickHouseHTTPConnection"))
    stop("Database icd_flag() currently supports ClickHouseHTTP tables only.", call. = FALSE)
  sql <- .icd_sql_context(con)
  normalized <- .icd_sql_source(data, id, cols, sql)
  query <- switch(strategy,
    row = .icd_row_query(normalized, definitions, id, match, sql),
    distinct = .icd_distinct_query(normalized, definitions, id, match, sql))
  # Explicit vars prevent schema discovery from executing the query here.
  dplyr::tbl(con, dbplyr::sql(query), vars = c(id, names(definitions)))
}

.icd_row_query <- function(normalized, definitions, id, match, sql) {
  codes <- "s.__thipr_codes"
  flags <- vapply(seq_along(definitions), function(j) {
    predicate <- .icd_sql_predicate(definitions[[j]], match, codes, sql, array = TRUE)
    .icd_sql_flag(paste0("length(", codes, ")"), predicate, names(definitions)[j], sql)
  }, character(1))
  paste0("SELECT s.__thipr_id AS ", sql$identifier(id), ",\n", paste(flags, collapse = ",\n"),
    "\nFROM (", normalized, ") AS s\nGROUP BY s.__thipr_id")
}

.icd_distinct_query <- function(normalized, definitions, id, match, sql) {
  # A blank sentinel retains IDs with no recorded codes. One source scan builds
  # an inverted code-to-ID index; condition predicates belong after this group.
  long <- paste0("SELECT n.__thipr_id,\n",
    "arrayJoin(arrayDistinct(emptyArrayToSingle(n.__thipr_codes))) AS __thipr_code\n",
    "FROM (", normalized, ") AS n")
  grouped <- paste0("SELECT __thipr_code, groupUniqArray(__thipr_id) AS __thipr_ids\n",
    "FROM (", long, ") AS entries\nGROUP BY __thipr_code")
  aliases <- paste0("__thipr_flag_", seq_along(definitions))
  predicates <- vapply(seq_along(definitions), function(j) {
    paste0(.icd_sql_predicate(definitions[[j]], match, "g.__thipr_code", sql),
           " AS ", aliases[j])
  }, character(1))
  mapped <- paste0("SELECT g.__thipr_ids, g.__thipr_code != '' AS __thipr_recorded,\n",
    paste(predicates, collapse = ",\n"), "\nFROM (", grouped, ") AS g")
  expanded <- paste0("SELECT arrayJoin(m.__thipr_ids) AS __thipr_id, m.__thipr_recorded,\n",
    paste0("m.", aliases, collapse = ",\n"), "\nFROM (", mapped, ") AS m")
  flags <- .icd_sql_flag("hits.__thipr_recorded", paste0("hits.", aliases), names(definitions), sql)
  paste0("SELECT hits.__thipr_id AS ", sql$identifier(id), ",\n", paste(flags, collapse = ",\n"),
    "\nFROM (", expanded, ") AS hits\nGROUP BY hits.__thipr_id")
}

# Retained benchmark implementation; not a public API.
.icd_flag_experimental <- function(data, conditions, id, cols, match = c("exact", "prefix"),
                     sql_strategy = c("row", "distinct")) {
  match <- match.arg(match)
  sql_strategy <- match.arg(sql_strategy)
  remote <- .icd_is_remote(data)
  fields <- if (remote) colnames(data) else names(data)
  .icd_validate_columns(fields, id, cols)
  definitions <- .icd_prepare_conditions(conditions, id)
  if (remote) return(.icd_flag_sql(data, definitions, id, cols, match, sql_strategy))
  .icd_flag_local(as.data.frame(data), definitions, id, cols, match)
}
