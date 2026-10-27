test_that("driver discovery supports default, custom and platform locations", {
  root <- withr::local_tempdir()
  withr::local_options(thipR.adbc_driver = NULL)
  local_mocked_bindings(.adbc_driver_dir = function() root)
  expect_identical(.resolve_adbc_driver(), "clickhouse")
  file.create(file.path(root, "clickhouse.toml"))
  expect_identical(.resolve_adbc_driver(), file.path(root, "clickhouse.toml"))
  expect_identical(.resolve_adbc_driver(root), file.path(root, "clickhouse.toml"))
  options(thipR.adbc_driver = "chosen.toml")
  expect_identical(.resolve_adbc_driver(), "chosen.toml")
  expect_identical(.resolve_adbc_driver("explicit.toml"), "explicit.toml")
  expect_error(.resolve_adbc_driver(NA_character_), "non-empty string")
})

test_that("working installations are reused without processes or environment edits", {
  root <- withr::local_tempdir(pattern = "driver with spaces ")
  manifest <- file.path(normalizePath(root, winslash = "/"), "clickhouse.toml")
  file.create(manifest)
  before <- Sys.getenv(c("ADBC_DRIVER_PATH", "R_LIBS_USER", "R_ENVIRON_USER"), unset = NA)
  libs <- .libPaths()
  withr::local_options(thipR.adbc_driver = NULL)
  local_mocked_bindings(
    .validate_adbc_manifest = function(x) expect_identical(x, manifest),
    .driver_installer = function(...) stop("Must not bootstrap"))
  expect_message(out <- thip_install_driver(root), "ready")
  expect_identical(out, manifest)
  expect_identical(.resolve_adbc_driver(), manifest)
  expect_identical(Sys.getenv(names(before), unset = NA), before)
  expect_identical(.libPaths(), libs)
  expect_false(dir.exists(file.path(root, ".thipr-install-lock")))
})

test_that("new installations select the destination and restore the environment", {
  root <- withr::local_tempdir()
  withr::local_options(thipR.adbc_driver = NULL)
  withr::local_envvar(ADBC_DRIVER_PATH = "existing-location")
  local_mocked_bindings(
    .driver_installer = function(...) "dbc",
    .driver_command = function(command, args, label) {
      expect_identical(Sys.getenv("ADBC_DRIVER_PATH"), normalizePath(root, winslash = "/"))
      expect_identical(args, c("install", "clickhouse=0.1.1"))
      file.create(file.path(root, "clickhouse.toml"))
    },
    .validate_adbc_manifest = function(...) TRUE)
  expect_message(thip_install_driver(root), "ready")
  expect_identical(Sys.getenv("ADBC_DRIVER_PATH"), "existing-location")
})

test_that("failures release the lock and do not select an incomplete driver", {
  root <- withr::local_tempdir()
  withr::local_options(thipR.adbc_driver = "previous.toml")
  withr::local_envvar(ADBC_DRIVER_PATH = NA)
  local_mocked_bindings(
    .driver_installer = function(...) "dbc",
    .driver_command = function(...) stop("Download failed"))
  expect_error(thip_install_driver(root), "Download failed")
  expect_identical(getOption("thipR.adbc_driver"), "previous.toml")
  expect_true(is.na(Sys.getenv("ADBC_DRIVER_PATH", unset = NA)))
  expect_false(dir.exists(file.path(root, ".thipr-install-lock")))
})

test_that("broken drivers and concurrent installers are never replaced", {
  root <- withr::local_tempdir()
  manifest <- file.path(root, "clickhouse.toml")
  writeLines("broken", manifest)
  withr::local_options(thipR.adbc_driver = "previous.toml")
  local_mocked_bindings(
    .validate_adbc_manifest = function(...) stop("Cannot load existing"),
    .driver_installer = function(...) stop("Must not replace"))
  expect_error(thip_install_driver(root), "Cannot load existing")
  expect_identical(readLines(manifest), "broken")
  expect_identical(getOption("thipR.adbc_driver"), "previous.toml")
  lock <- file.path(root, ".thipr-install-lock")
  dir.create(lock)
  expect_error(thip_install_driver(root), "locked")
  expect_true(dir.exists(lock))
  expect_error(thip_install_driver(character()), "non-empty string")
})

test_that("setup defaults to ADBC and forwards a custom driver before saving", {
  root <- withr::local_tempdir()
  withr::local_envvar(c(THIP_DB_HOST=NA, THIP_DB_PORT=NA, THIP_DB_NAME=NA,
    THIP_DB_USER=NA, THIP_DB_PASSWORD=NA, THIP_DB_SCHEME=NA))
  local_mocked_bindings(.load_adbc_driver = function(driver) invisible(NULL),
    .connect_checked = function(config, backend, driver) {
    expect_identical(backend, "adbc")
    expect_identical(driver, "chosen.toml")
    stop("Stop before saving")
  })
  file <- file.path(root, "Renviron")
  expect_error(thip_setup(host="example.invalid",user="test",password="synthetic",
    driver="chosen.toml",path=file), "Stop before saving")
  expect_false(file.exists(file))
})


test_that("setup reports missing drivers before prompting and never installs them", {
  root <- withr::local_tempdir()
  local_mocked_bindings(.setup_input = function(...) stop("Must not prompt"),
    thip_install_driver = function(...) stop("Must not install"))
  expect_error(thip_setup(host="",user="",password="",
    driver=file.path(root,"missing-driver.toml"),path=file.path(root,"Renviron")),
    "Run thip_install_driver()", fixed=TRUE)
  expect_false(file.exists(file.path(root,"Renviron")))
})
