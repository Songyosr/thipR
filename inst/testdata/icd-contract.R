# Synthetic contract fixtures and an intentionally simple independent reference.
# Shared by offline tests and opt-in smoke checks; never used by production code.
.icd_test_fixture <- function() {
  list(
    data = data.frame(id = c("b", "a", "b", "c", "d", "e", "f"),
      dx1 = c("\t f12.1 \r\n", "F12", "E11.9", NA, "Z99", "001.2", "F12*X"),
      dx2 = c("T40.7", "", "F121", ".", NA, "F12X", "F12'OR'1'='1")),
    conditions = list(cannabis = c("f12", "T40.7"), diabetes = "E11", zero = "0012",
      overlap = "T407", empty = character(), `literal punctuation` = "F12*",
      quoted = "F12'OR'1'='1"),
    id = "id", cols = c("dx1", "dx2"))
}

.icd_test_reference <- function(data, conditions, id, cols, mode) {
  clean <- function(z) chartr("abcdefghijklmnopqrstuvwxyz", "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                              gsub("[.]", "", trimws(as.character(z))))
  out <- data[!duplicated(data[[id]]), id, drop = FALSE]
  rownames(out) <- NULL
  for (condition in names(conditions)) {
    out[[condition]] <- vapply(out[[id]], function(key) {
      selected <- data[data[[id]] == key, cols, drop = FALSE]
      codes <- clean(unlist(lapply(selected, as.character), use.names = FALSE))
      codes <- codes[!is.na(codes) & nzchar(codes)]
      if (!length(codes)) return(NA)
      any(vapply(codes, function(code) {
        any(vapply(clean(conditions[[condition]]), function(term) {
          if (mode == "exact") code == term else substr(code, 1L, nchar(term)) == term
        }, logical(1)))
      }, logical(1)))
    }, logical(1), USE.NAMES = FALSE)
  }
  out
}

.icd_test_order <- function(x, id = "id") {
  x <- as.data.frame(x)
  x <- x[order(x[[id]]), , drop = FALSE]
  rownames(x) <- NULL
  x
}
