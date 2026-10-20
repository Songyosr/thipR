# Tiny synthetic contract; intentionally independent of production helpers.
.window_test_fixture <- function() {
  day <- as.Date("2023-01-15")
  data <- data.frame(person = c(rep("a", 10), "b", "b", "other", NA, "a", "quote'person"),
    observed = day + c(-11, -10, -1, 0, 1, 9, 10, 11, NA, -1, -2, 2, 0, 0, 3, -1),
    record = seq_len(16), index_date = day + 100 + seq_len(16),
    dx = c("F12.1", "E11.9", "F20", "F23", "K35", "F29", "E11", "X", NA,
           "F20", "F12", "F29", "K35", "K35", "E11", "F20"), check.names = FALSE)
  names(data)[2] <- "observed date"
  # Preserve a truly duplicated source record, not just duplicate keys.
  data <- rbind(data, data[3, , drop = FALSE])
  cohort <- data.frame(subject = c("a", "a", "b", "no_records", "quote'person"),
    anchor = day + c(0, 2, 0, 0, 0), entry = c("e1", "e2", "e3", "e4", "e5"),
    stringsAsFactors = FALSE)
  names(cohort)[3] <- "entry key"
  list(data = data, cohort = cohort)
}

.window_test_reference <- function(data, cohort, bounds = "[)", offsets = c(-10, 10)) {
  result <- data[FALSE, , drop = FALSE]
  result[["entry key"]] <- character()
  for (i in seq_len(nrow(cohort))) {
    lower <- cohort$anchor[i] + offsets[1]
    upper <- cohort$anchor[i] + offsets[2]
    dates <- data[["observed date"]]
    a <- if (substr(bounds, 1, 1) == "[") dates >= lower else dates > lower
    b <- if (substr(bounds, 2, 2) == "]") dates <= upper else dates < upper
    hit <- which(!is.na(data$person) & data$person == cohort$subject[i] & a & b)
    part <- data[hit, , drop = FALSE]
    part[["entry key"]] <- rep(cohort[["entry key"]][i], length(hit))
    result <- rbind(result, part)
  }
  .window_test_order(result)
}

.window_test_order <- function(x) {
  attr(x, "window_report") <- NULL
  x <- as.data.frame(x)
  x <- x[order(x[["entry key"]], x$record), , drop = FALSE]
  rownames(x) <- NULL
  x
}

.window_test_plan <- function(data, cohort, bounds = "[)", window = c(-10, 10)) {
  thipR::thip_window(data, cohort, date = "observed date", index = "anchor",
    window = window, by = c(person = "subject"), entry = "entry key", bounds = bounds)
}
