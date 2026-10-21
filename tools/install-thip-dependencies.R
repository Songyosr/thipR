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
install.packages(c("arrow", "ClickHouseHTTP"), lib = lib, Ncpus = 1)
stopifnot(requireNamespace("arrow", quietly = TRUE),
          requireNamespace("ClickHouseHTTP", quietly = TRUE),
          packageVersion("ClickHouseHTTP") >= "1.1.2")  # Minimum in DESCRIPTION
cat("Installed in:", lib, "\n")
