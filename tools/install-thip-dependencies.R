# Run manually inside THIP, not during package load or tests.
# Installs into a separate user library; never changes the shared R library.
lib <- path.expand("~/R/thipR-library")
dir.create(lib, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(lib, .libPaths()))
options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 300)

# The observed Ubuntu 22.04 THIP R lacked a CXX20 configuration, although
# /usr/bin/g++ supported it. Enable this only on that confirmed platform.
if (identical(Sys.getenv("THIP_USE_EXISTING_GXX20"), "true")) {
  stopifnot(file.exists("/usr/bin/g++"))
  makevars <- tempfile("thipr-Makevars-")
  writeLines(c("CXX20 = /usr/bin/g++", "CXX20STD = -std=gnu++20",
               "CXX20FLAGS = -O2 -fPIC", "CXX20PICFLAGS = -fPIC"), makevars)
  Sys.setenv(R_MAKEVARS_USER = makevars)
}
Sys.setenv(NOT_CRAN = "true", LIBARROW_BINARY = "true", MAKEFLAGS = "-j2")
driver <- "1.1.2"  # Must match the exact Imports pin in DESCRIPTION
install.packages(c("arrow", "DBI", "httr2", "jsonlite", "data.table"), lib = lib, Ncpus = 1)
# Install exactly the tested driver, from the CRAN archive once CRAN moves on.
current <- tryCatch(unname(available.packages()["ClickHouseHTTP", "Version"]),
                    error = function(e) NA_character_)
if (identical(current, driver)) {
  install.packages("ClickHouseHTTP", lib = lib, Ncpus = 1)
} else {
  install.packages(sprintf("%s/src/contrib/Archive/ClickHouseHTTP/ClickHouseHTTP_%s.tar.gz",
                           getOption("repos")[["CRAN"]], driver),
                   repos = NULL, type = "source", lib = lib)
}
stopifnot(requireNamespace("arrow", quietly = TRUE),
          requireNamespace("ClickHouseHTTP", quietly = TRUE),
          packageVersion("ClickHouseHTTP") == driver)
cat("Installed in:", lib, "\n")
