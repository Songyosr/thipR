test_config <- function(...) {
  args <- list(host = "example.invalid", port = "8123", database = "example",
               user = "example", password = "synthetic-secret", scheme = "https",
               max_seconds = 60L, max_threads = 2L, max_rows = 100000L)
  overrides <- list(...)
  args[names(overrides)] <- overrides
  do.call(thipR:::.connection_config, args)
}
