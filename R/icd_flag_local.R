.icd_flag_local <- function(data, definitions, id, cols, match) {
  .icd_validate_local(data, id, cols)
  ids <- data[[id]]
  first <- !duplicated(ids)
  out <- data[first, id, drop = FALSE]
  rownames(out) <- NULL
  n_ids <- nrow(out)
  if (!n_ids) {
    for (name in names(definitions)) out[[name]] <- logical()
    return(out)
  }
  # Keep column-major ordering shared by codes and row-to-ID indices.
  codes <- unlist(lapply(data[cols], .normalize_icd_codes), use.names = FALSE)
  id_index <- rep.int(base::match(ids, ids[first]), length(cols))
  recorded <- !is.na(codes) & nzchar(codes)
  observed_ids <- unique(id_index[recorded])
  observed <- unique(codes[recorded])
  hits <- .icd_match_codes(observed, definitions, match)
  flags <- .icd_sparse_flags(codes, id_index, observed, hits, n_ids)
  flags[!seq_len(n_ids) %in% observed_ids, ] <- NA
  for (j in seq_along(definitions)) out[[names(definitions)[j]]] <- flags[, j]
  out
}

# Only distinct observed codes participate in condition matching.
.icd_match_codes <- function(observed, definitions, match) {
  lapply(definitions, function(definition) {
    if (match == "exact") {
      pos <- base::match(definition, observed, nomatch = 0L)
      pos[pos > 0L]
    } else {
      found <- rep(FALSE, length(observed))
      for (prefix in definition) found <- found | startsWith(observed, prefix)
      which(found)
    }
  })
}

.icd_sparse_flags <- function(codes, id_index, observed, hits, n_ids) {
  all_hits <- as.integer(unlist(hits, use.names = FALSE))
  relevant <- sort(unique(all_hits))
  # Allocate a dense all-FALSE matrix only when no sparse product is needed.
  if (!length(relevant)) return(matrix(FALSE, nrow = n_ids, ncol = length(hits)))
  code_index <- base::match(codes, observed[relevant])
  keep <- which(!is.na(code_index))
  visits <- Matrix::sparseMatrix(
    i = id_index[keep], j = code_index[keep], x = rep(TRUE, length(keep)),
    dims = c(n_ids, length(relevant)))
  mapping <- Matrix::sparseMatrix(
    i = base::match(all_hits, relevant),
    j = rep.int(seq_along(hits), lengths(hits)),
    x = rep(TRUE, length(all_hits)),
    dims = c(length(relevant), length(hits)))
  as.matrix((visits %*% mapping) > 0)
}
