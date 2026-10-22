test_that("custom flags normalize codes and distinguish exact from prefix", {
  x <- data.frame(id = c("b", "a", "b", "c", "d", "e"),
                  pdx = c(" f12.1 ", "F12", "E11.9", NA, "Z99", "001.2"),
                  sdx = factor(c("T40.7", "", "F121", " . ", NA, "F12X")))
  original <- x
  defs <- list(cannabis = c("f12", " T40.7 "), diabetes = "e11",
               zero = "0012", overlap = "T407", empty = character())
  exact <- icd_flag(x, defs, "id", c("pdx", "sdx"))
  expect_identical(exact$id, c("b", "a", "c", "d", "e"))
  expect_identical(exact$cannabis, c(TRUE, TRUE, NA, FALSE, FALSE))
  expect_identical(exact$diabetes, c(FALSE, FALSE, NA, FALSE, FALSE))
  expect_identical(exact$zero, c(FALSE, FALSE, NA, FALSE, TRUE))
  expect_identical(exact$overlap, c(TRUE, FALSE, NA, FALSE, FALSE))
  expect_identical(exact$empty, c(FALSE, FALSE, NA, FALSE, FALSE))
  prefix <- icd_flag(x, defs, "id", c("pdx", "sdx"), "prefix")
  expect_identical(prefix$cannabis, c(TRUE, TRUE, NA, FALSE, TRUE))
  expect_identical(prefix$diabetes, c(TRUE, FALSE, NA, FALSE, FALSE))
  expect_identical(x, original)
})

test_that("prefixes are literal and matching is limited to selected columns", {
  x <- data.frame(id = 1:4, dx = c("F12", "F12X", "F12*Z", "AF12"),
                  ignored = "F12")
  expect_identical(icd_flag(x, list(flag = "F12"), "id", "dx")$flag,
                   c(TRUE, FALSE, FALSE, FALSE))
  expect_identical(icd_flag(x, list(flag = "F12*"), "id", "dx", "prefix")$flag,
                   c(FALSE, FALSE, TRUE, FALSE))
  expect_identical(icd_flag(x, list(flag = "F12"), "id", "dx", "prefix")$flag,
                   c(TRUE, TRUE, TRUE, FALSE))
})

test_that("empty inputs, no matches, duplicate codes and IDs preserve the contract", {
  x <- data.frame(id = factor(c("z", "a", "z"), levels = c("a", "z", "unused")),
                  dx = c("A01", "", "A01"))
  out <- icd_flag(x, list(`custom name` = c("A01", "A.01")), "id", "dx")
  expect_identical(out$id, x$id[1:2])
  expect_identical(out[["custom name"]], c(TRUE, NA))
  expect_identical(names(out), c("id", "custom name"))
  expect_identical(icd_flag(x, list(none = "Z99"), "id", "dx")$none, c(FALSE, NA))
  empty <- icd_flag(x[FALSE, ], list(flag = "A01"), "id", "dx")
  expect_identical(empty$id, x$id[FALSE])
  expect_identical(empty$flag, logical())
  expect_identical(icd_flag(data.frame(id = 1L, dx = NA_character_),
                           list(flag = "A"), "id", "dx")$flag, NA)
  expect_identical(icd_flag(data.frame(id = c(2L, 1L), dx = c("A", "B")),
                           list(flag = "A"), "id", "dx")$id, c(2L, 1L))
})

test_that("code cleaning gives repeated and missing values the same result", {
  x <- factor(c(" e11.9 ", NA, " e11.9 ", "J06", "", " j06 ", NA))
  expect_identical(thipR:::.normalize_icd_codes(x),
                   c("E119", NA, "E119", "J06", "", "J06", NA))
  expect_identical(thipR:::.normalize_icd_codes(character()), character())
})

test_that("malformed inputs are rejected before classification", {
  x <- data.frame(id = "a", dx = "F12")
  run <- function(data = x, conditions = list(flag = "F12"), id = "id", cols = "dx")
    icd_flag(data, conditions, id, cols)
  expect_error(run(data = list()), "local data frame")
  expect_error(run(data = setNames(x, c("id", "id"))), "column names")
  expect_error(run(id = "missing"), "id must")
  expect_error(run(cols = c("dx", "dx")), "cols must")
  expect_error(run(cols = "id"), "cols must")
  expect_error(run(cols = "missing"), "cols must")
  expect_error(run(conditions = list("F12")), "named list")
  expect_error(run(conditions = list(a = "A", a = "B")), "named list")
  expect_error(run(conditions = list(id = "A")), "named list")
  expect_error(run(conditions = list(a = 123)), "character vector")
  expect_error(run(conditions = list(a = NA_character_)), "missing or blank")
  expect_error(run(conditions = list(a = ".")), "missing or blank")
  expect_error(run(data = data.frame(id = NA_character_, dx = "A")), "IDs must")
  expect_error(run(data = data.frame(id = " ", dx = "A")), "IDs must")
  expect_error(run(data = data.frame(id = Inf, dx = "A")), "IDs must")
  expect_error(run(data = data.frame(id = "a", dx = 123)), "numeric codes")
  expect_error(icd_flag(x, list(a = "A"), "id", "dx", "regex"), "arg")
})

test_that("sparse aggregation agrees with an independent row-wise reference", {
  set.seed(491)
  pool <- c("F12", "f12.1", "T40.7", "E11.9", "A01", "001.2", "", NA)
  x <- data.frame(id = sample(letters[1:20], 40, replace = TRUE),
                  dx1 = sample(pool, 40, replace = TRUE),
                  dx2 = sample(pool, 40, replace = TRUE))
  defs <- list(a = c("F12", "T407"), b = c("E11", "0012"), c = "Z99")
  for (mode in c("exact", "prefix")) {
    out <- icd_flag(x, defs, "id", c("dx1", "dx2"), mode)
    reference <- .icd_test_reference(x, defs, "id", c("dx1", "dx2"), mode)
    for (group in names(defs)) {
      expect_identical(out[[group]], reference[[group]])
    }
  }
})

test_that("the common ICD contract covers both modes and supported local ID types", {
  fixture <- .icd_test_fixture()
  original <- fixture$data
  for (kind in c("character", "factor", "integer", "numeric", "empty")) {
    x <- original
    if (kind == "factor") {
      x$id <- factor(x$id, levels = c(unique(x$id), "unused"))
      x$dx2 <- factor(x$dx2)
    }
    if (kind %in% c("integer", "numeric")) {
      x$id <- match(x$id, unique(x$id))
      if (kind == "numeric") x$id <- as.numeric(x$id) / 2
    }
    if (kind == "empty") x <- x[FALSE, ]
    before <- x
    for (mode in c("exact", "prefix")) {
      expect_identical(icd_flag(x, fixture$conditions, fixture$id, fixture$cols, mode),
                       .icd_test_reference(x, fixture$conditions, fixture$id, fixture$cols, mode))
    }
    expect_identical(x, before)
  }
  for (invalid in list(NaN, -Inf, structure(1, class = "custom"), matrix(1))) {
    x <- data.frame(dx = "F12")
    x$id <- invalid
    expect_error(icd_flag(x, list(flag = "F12"), "id", "dx"), "IDs must")
  }
})

test_that("unsupported range, wildcard and list notation errors before matching", {
  x <- data.frame(id = c("A", "B"), dx = c("Q20.0", "Q26.9"))
  expect_error(icd_flag(x, list(cardiac = "Q20-Q26"), "id", "dx", "prefix"),
               "Unsupported ICD notation \\(cardiac: Q20-Q26\\)")
  for (code in c("E11%", "E1_", "E11?", "^E11", "E11$", "E1[01]", "E10|E11",
                 "E10,E11", "E10 E11", "E1(0)", "E10;E11", "E10\u2013E14",
                 "E10\u2014E14", "Q20\u2212Q26", "E11\u00a0", "E11\uff0e9"))
    expect_error(icd_flag(x, list(flag = code), "id", "dx", "prefix"),
                 "Unsupported ICD notation", info = code)
  expect_error(icd_flag(x, list(ok = "Q20", bad = c("Q27", "Q20-Q26")), "id", "dx"),
               "bad: Q20-Q26")
  expect_identical(icd_flag(x, list(cardiac = paste0("Q", 20:26)), "id", "dx", "prefix")$cardiac,
                   c(TRUE, TRUE))
})

test_that("the notation guard keeps supported code formats unchanged", {
  x <- data.frame(id = 1:5, dx = c("E11.9", "F12*Z", "A17.0+", "0010", "U07.1"))
  out <- icd_flag(x, list(dm = " e11.9 ", lit = "F12*", dag = "A170+", proc = "0010",
                          covid = "U07.1", empty = character()), "id", "dx")
  expect_identical(out$dm, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(out$dag, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_identical(out$proc, c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(out$covid, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_false(any(out$empty))
  # `*` is literal: "E11*" is a code, not "E11 and everything beneath it".
  expect_identical(icd_flag(x, list(star = "E11*"), "id", "dx", "prefix")$star,
                   rep(FALSE, 5))
})
