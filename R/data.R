#' Fields of THIP's yearly inpatient tables
#'
#' A field reference for the `nhso` inpatient tables, written for this package
#' from THIP's published data dictionaries. The same fields appear in the example
#' database from [thip_connect_example()]. Use [thip_columns()] to list a table's
#' columns on your connection.
#'
#' @format A data frame with one row per field and five columns:
#' \describe{
#'   \item{field}{Column name in the table.}
#'   \item{group}{Topic: identifiers, person, admission and discharge, payment
#'     group, diagnoses, procedures, death or geography.}
#'   \item{description}{What the field records.}
#'   \item{format}{How values are written. Every column is stored as text.}
#'   \item{codes}{Meaning of the codes for coded fields; empty otherwise.}
#' }
#' @source THIP's data dictionaries (the 2024 "Data Structure Description" and
#'   the 2023 field list), rewritten.
#' @examples
#' thip_fields[, c("field", "group", "description")]
#'
#' # What each discharge status code means
#' strsplit(thip_fields$codes[thip_fields$field == "dischs"], "; ")[[1]]
#' @md
"thip_fields"
