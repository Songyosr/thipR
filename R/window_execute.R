.datatable.aware <- TRUE

.window_collect <- function(plan, batch_size, max_query_bytes) {
  if (!plan$remote) {
    plan$data <- as.data.frame(plan$data)
    keep <- which(!is.na(plan$data[[plan$source_id]]) & !is.na(plan$data[[plan$date]]))
    plan$local_index <- data.table::data.table(person = plan$data[[plan$source_id]][keep], row = keep)
    data.table::setkeyv(plan$local_index, "person")
  }
  prototype <- if (plan$remote) .window_query(plan, prototype = TRUE) else
    as.data.frame(plan$data)[FALSE, , drop = FALSE]
  .window_check_source(prototype, plan$source_id, plan$date, plan$windows$person)
  people <- unique(plan$windows$person)
  group <- ceiling(match(plan$windows$person, people) / batch_size)
  batches <- unname(split(seq_len(nrow(plan$windows)), group))
  chunks <- vector("list", length(batches))
  reports <- vector("list", length(batches))
  seen <- logical(nrow(plan$windows))
  for (i in seq_along(batches)) {
    rows <- batches[[i]]
    w <- plan$windows[rows, , drop = FALSE]
    active <- w[w$lower <= w$upper, , drop = FALSE]
    chunk <- tryCatch({
      if (!nrow(active)) {
        out <- prototype
        out[["__thipr_window_row"]] <- integer()
        out
      } else if (plan$remote) {
        query <- .window_sql(plan, active)
        if (nchar(query, type = "bytes") > max_query_bytes)
          stop("SQL payload exceeds max_query_bytes; reduce batch_size or subdivide this person's windows explicitly.", call. = FALSE)
        .window_query(plan, sql = query)
      } else .window_local(plan, active)
    }, error = function(e) {
      # Backend errors may contain identifiers or credential-bearing requests.
      msg <- if (grepl("^SQL payload exceeds", conditionMessage(e))) conditionMessage(e) else
        "Query or result validation failed; backend details suppressed."
      stop("Window batch ", i, "/", length(batches), " failed. No partial result returned. ", msg, call. = FALSE)
    })
    ord <- chunk[["__thipr_window_row"]]
    if (length(ord) != nrow(chunk) || anyNA(ord) || any(!ord %in% rows))
      stop("Invalid batch entry mapping. No partial result returned.", call. = FALSE)
    seen[ord] <- TRUE
    chunk[["__thipr_window_row"]] <- NULL
    chunk[[plan$entry]] <- plan$entry_values[ord]
    chunks[[i]] <- chunk
    reports[[i]] <- data.frame(batch = i, people = length(unique(w$person)),
                               entries = nrow(w), matches = nrow(chunk))
  }
  out <- prototype
  out[[plan$entry]] <- plan$entry_values[FALSE]
  if (length(chunks)) out <- as.data.frame(dplyr::bind_rows(chunks))
  attr(out, "window_report") <- list(complete = TRUE, entries = nrow(plan$windows),
    matched_entries = sum(seen), unmatched_entries = sum(!seen),
    batches = if (length(reports)) do.call(rbind, reports) else
      data.frame(batch = integer(), people = integer(), entries = integer(), matches = integer()))
  out
}

.window_local <- function(plan, windows) {
  # Only return match indices from the engine; reconstruct columns from originals.
  # This avoids non-equi join output-name/value ambiguity and never mutates input.
  src <- plan$data
  keep <- plan$local_index[list(unique(windows$person)), nomatch = NULL]$row
  visits <- data.table::data.table(person = src[[plan$source_id]][keep],
    lo = as.numeric(src[[plan$date]][keep]), hi = as.numeric(src[[plan$date]][keep]))
  ranges <- data.table::data.table(person = windows$person, lo = windows$lower,
                                  hi = windows$upper, ordinal = windows$ordinal)
  data.table::setkeyv(ranges, c("person", "lo", "hi"))
  hits <- data.table::foverlaps(visits, ranges,
    by.x = c("person", "lo", "hi"), by.y = c("person", "lo", "hi"),
    type = "within", nomatch = NULL, which = TRUE)
  out <- src[keep[hits$xid], , drop = FALSE]
  out[["__thipr_window_row"]] <- ranges$ordinal[hits$yid]
  rownames(out) <- NULL
  out
}
