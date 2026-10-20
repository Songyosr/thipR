# Synthetic checks only. No connection or database access is required.
library(thipR)
source(system.file("testdata", "icd-contract.R", package = "thipR", mustWork = TRUE), local = TRUE)
fixture <- .icd_test_fixture()
for (mode in c("exact", "prefix")) {
  actual <- icd_flag(fixture$data, fixture$conditions, fixture$id, fixture$cols, mode)
  expected <- .icd_test_reference(fixture$data, fixture$conditions, fixture$id, fixture$cols, mode)
  stopifnot(identical(actual, expected))
}

# A larger multi-condition check against independent vectorized regex scans.
set.seed(731)
parents <- sprintf("F%02d", 0:49)
codes <- as.vector(outer(parents, 0:9, paste0))
large <- data.frame(admission_id = sprintf("synthetic_%05d", 1:10000))
for (j in 1:8) large[[paste0("dx", j)]] <- sample(c(codes, NA), 10000, TRUE)
for (mode in c("exact", "prefix")) {
  vocabulary <- if (mode == "exact") codes else parents
  maps <- setNames(lapply(1:10, function(i) sample(vocabulary, 5)), paste0("group_", 1:10))
  flags <- icd_flag(large, maps, "admission_id", paste0("dx", 1:8), mode)
  for (name in names(maps)) {
    pattern <- paste0("^(", paste(maps[[name]], collapse = "|"), ")",
                      if (mode == "exact") "$" else "")
    reference <- Reduce(`|`, lapply(large[-1], function(z) grepl(pattern, z)))
    all_missing <- Reduce(`&`, lapply(large[-1], is.na))
    reference[all_missing] <- NA
    stopifnot(identical(flags[[name]], reference))
  }
}
print(actual)
cat("PASS: normalization, exact/prefix, repeated IDs, missing codes, leading zeros,\n",
    "and 10,000 synthetic admissions x 8 diagnosis columns x 10 conditions.\n", sep = "")
