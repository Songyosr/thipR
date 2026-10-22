#' Plan retrieval of records in person-specific date windows
#'
#' Creates a retrieval plan without reading source records. Use
#' [dplyr::collect()] to execute complete batches. Filter and select a lazy source
#' before planning; this plan is not a general dbplyr table. A local data frame
#' is also supported, using the same interval rules with a data.table matcher.
#'
#' Relative windows use integer days, not calendar months or years. Dates must
#' be Date values; strings and timestamps are not silently converted. Missing
#' source dates/IDs do not match. Missing cohort keys/dates are rejected.
#' Output contains source columns unchanged plus the cohort entry key. A source
#' row can match multiple entries, and source duplicates are preserved. Entries
#' without matches remain in the input roster; see the attached `window_report`
#' for completion and zero-match counts. An empty clinical row is never invented.
#'
#' @param data A lazy table from \code{\link{thip_connect}()} or \code{\link{thip_connect_example}()}, or a local data frame.
#' @param cohort Local data frame with one row per unique entry.
#' @param date Name of the record-date column in data.
#' @param index Reference-date column in cohort, default "index_date" when window is supplied.
#' @param window Two integer day offsets from index, in increasing order.
#' @param by One shared person column, or a named mapping c(source = "cohort").
#' @param entry Unique entry-key column in cohort; must not collide with source columns.
#' @param start,end Explicit cohort date columns, instead of index/window.
#' @param bounds Endpoint inclusion: "[)" (default), "[]", "(]", or "()".
#' @return A thip_window_plan, executed with collect().
#' @export
#' @examples
#' visits <- data.frame(pid = c("a", "a"),
#'                      visit_date = as.Date(c("2023-01-01", "2023-01-10")))
#' entries <- data.frame(pid = "a", entry_id = "e1", index_date = as.Date("2023-01-10"))
#' plan <- thip_window(visits, entries, date = "visit_date", window = c(-9, 0))
#' dplyr::collect(plan)
thip_window <- function(data, cohort, date, index = NULL, window = NULL,
                        by = "pid", entry = "entry_id", start = NULL,
                        end = NULL, bounds = "[)") {
  spec <- .window_spec(data, cohort, date, index, window, by, entry, start, end, bounds)
  structure(c(list(data = data), spec), class = "thip_window_plan")
}

#' @export
print.thip_window_plan <- function(x, ...) {
  cat("THIP window plan: ", nrow(x$windows), " entries; ",
      length(unique(x$windows$person)), " people\n", sep = "")
  cat("Match source ", x$source_id, " to cohort ", x$cohort_id, "\n",
      "Record date ", x$date, "; bounds ", x$bounds, "\n", sep = "")
  cat("Execution pending: source records have not been read.\n",
      "Use dplyr::collect() to run the plan.\n", sep = "")
  invisible(x)
}

#' Execute a THIP window plan
#'
#' Batches partition people, keeping all their windows together. Batch size is
#' not a result-row limit. No writes or temporary DB tables are used. A failed
#' batch raises an error, never a partial successful result. All returned records
#' are ultimately assembled in R memory; batching does not make collect out of core.
#'
#' @param x A plan from thip_window().
#' @param ... Reserved; unused arguments are rejected.
#' @param batch_size Maximum distinct people per batch, default 1000.
#' @param max_query_bytes Maximum generated SQL bytes per batch. Oversized batches
#'   fail with guidance to reduce batch_size, rather than silently truncating.
#' @return A data frame with a window_report attribute containing batch counts,
#'   completion and unmatched-entry counts. Output row order is unspecified.
#' @importFrom dplyr collect
#' @export
collect.thip_window_plan <- function(x, ..., batch_size = 1000L,
                                     max_query_bytes = 1048576L) {
  if (length(list(...))) stop("Unused collect arguments for a THIP window plan.", call. = FALSE)
  batch_size <- .positive_integer(batch_size, "batch_size")
  max_query_bytes <- .positive_integer(max_query_bytes, "max_query_bytes")
  .window_collect(x, batch_size, max_query_bytes)
}
