# Quoting and normalization are shared by SQL strategies and built once per call.
.icd_sql_context <- function(con) {
  identifier <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  literal <- function(x) as.character(DBI::dbQuoteString(con, x))
  field <- function(x) paste0("src.", identifier(x))
  text <- function(x) paste0("ifNull(toString(", field(x), "), '')")
  whitespace <- literal(" \t\r\n")
  trim <- function(x) paste0("trimBoth(", text(x), ", ", whitespace, ")")
  example <- identical(.thip_dialect(con), "example")
  list(
    identifier = identifier, literal = literal, field = field, text = text, trim = trim,
    # Array lambdas and typed casts differ between ClickHouse and DuckDB.
    keep = function(predicate, x) if (example)
      paste0("list_filter(", x, ", lambda code: ", predicate, ")") else
      paste0("arrayFilter(code -> ", predicate, ", ", x, ")"),
    any = function(predicate, x) if (example)
      paste0("(len(list_filter(", x, ", lambda code: ", predicate, ")) > 0)") else
      paste0("arrayExists(code -> ", predicate, ", ", x, ")"),
    as_bool = function(x) if (example) paste0("CAST(", x, " AS BOOLEAN)") else
      paste0("CAST(", x, ", 'Nullable(Bool)')"),
    strings = function(x) paste0("[", paste(literal(x), collapse = ", "), "]"),
    normalize = function(x) paste0("upperUTF8(replaceAll(", trim(x), ", '.', ''))"),
    base_type = function(x) paste0(
      "replaceAll(replaceAll(replaceAll(toTypeName(", field(x),
      "), 'LowCardinality(', ''), 'Nullable(', ''), ')', '')")
  )
}

.icd_sql_guard <- function(id, cols, sql) {
  id_types <- c("String", paste0("Int", c(8, 16, 32, 64)),
                paste0("UInt", c(8, 16, 32, 64)), "Float32", "Float64")
  invalid <- c(
    paste0("NOT has(", sql$strings(id_types), ", ", sql$base_type(id), ")"),
    vapply(cols, function(x) paste0(sql$base_type(x), " != 'String'"), character(1)),
    paste0("(", sql$field(id), " IS NULL)"), paste0("empty(", sql$trim(id), ")"),
    paste0("(startsWith(", sql$base_type(id), ", 'Float') AND NOT isFinite(toFloat64OrZero(",
           sql$text(id), ")))"))
  paste0("throwIf(", paste(invalid, collapse = " OR "), ", ",
    sql$literal("icd_flag: IDs must be nonmissing/nonblank and finite; diagnosis columns must be String; unsupported column type."), ") = 0")
}

.icd_sql_source <- function(data, id, cols, sql) {
  source <- as.character(dbplyr::sql_render(data))
  paste0("SELECT ", sql$field(id), " AS __thipr_id,\n",
    sql$keep("code != ''", paste0("[", paste(vapply(cols, sql$normalize, character(1)), collapse = ", "), "]")),
    " AS __thipr_codes\nFROM (", source, ") AS src\nWHERE ", .icd_sql_guard(id, cols, sql))
}

# Match policy is common; the row strategy applies it to an array, distinct to a code.
.icd_sql_predicate <- function(terms, match, x, sql, array = FALSE) {
  if (!length(terms)) return("0")
  if (match == "exact") {
    if (array) return(paste0("hasAny(", x, ", ", sql$strings(terms), ")"))
    return(paste0(x, " IN (", paste(sql$literal(terms), collapse = ", "), ")"))
  }
  code <- if (array) "code" else x
  predicate <- paste0("(", paste0("startsWith(", code, ", ", sql$literal(terms), ")", collapse = " OR "), ")")
  if (array) sql$any(predicate, x) else predicate
}

.icd_sql_flag <- function(recorded, hit, name, sql) {
  paste0(sql$as_bool(paste0("if(max(", recorded, ") = 0, NULL, max(", hit, "))")),
    " AS ", sql$identifier(name))
}
