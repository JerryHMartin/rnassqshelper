

# tests/testthat/test_api_nass.R

test_that("get_nass_key returns key when set in env", {
  skip_on_cran()
  skip_on_ci()
  
  withr::local_envvar(c(NASS_API_KEY = "abc12345"))
  # No output expected (quietly get); suppress to be safe
  suppressMessages(suppressWarnings({
    k <- get_nass_key(sources = c("env"))
  }))
  expect_equal(k, "abc12345")
})

test_that("get_nass_key errors when key is missing", {
  skip_on_cran()
  skip_on_ci()
  
  withr::local_envvar(c(NASS_API_KEY = ""))
  # Expect an error; suppress normal output so pass is quiet
  expect_error(
    suppressMessages(suppressWarnings(
      get_nass_key(sources = c("env"), error_if_missing = TRUE)
    )),
    "not found",
    fixed = FALSE
  )
})

test_that("set_nass_key(session) sets env (auth skipped if rnassqs missing)", {
  skip_on_cran()
  skip_on_ci()
  
  withr::local_envvar(c(NASS_API_KEY = ""))
  # No need to authenticate if rnassqs not installed; suppress messages
  suppressMessages(suppressWarnings({
    invisible(set_nass_key("xyz987", persist = "session", auth = requireNamespace("rnassqs", quietly = TRUE)))
  }))
  expect_equal(Sys.getenv("NASS_API_KEY", unset = ""), "xyz987")
})

test_that("set_nass_key(env) sets process environment only", {
  skip_on_cran()
  skip_on_ci()
  
  withr::local_envvar(c(NASS_API_KEY = ""))
  suppressMessages(suppressWarnings({
    invisible(set_nass_key("envkey", persist = "env", auth = FALSE))
  }))
  expect_equal(Sys.getenv("NASS_API_KEY", unset = ""), "envkey")
})

test_that("set_nass_key(renviron) writes to specified file without loading", {
  skip_on_cran()
  skip_on_ci()
  
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  
  # Write key to renviron file; do NOT call readRenviron() here to avoid global side effects
  suppressMessages(suppressWarnings({
    invisible(set_nass_key("firstkey", persist = "renviron", renviron_path = tmp, overwrite = TRUE, auth = FALSE))
  }))
  lines <- readLines(tmp, warn = FALSE)
  expect_true(any(grepl("^\\s*NASS_API_KEY\\s*=\\s*firstkey\\s*$", lines)))
  
  # Overwrite with a new key
  suppressMessages(suppressWarnings({
    invisible(set_nass_key("secondkey", persist = "renviron", renviron_path = tmp, overwrite = TRUE, auth = FALSE))
  }))
  lines2 <- readLines(tmp, warn = FALSE)
  expect_true(any(grepl("^\\s*NASS_API_KEY\\s*=\\s*secondkey\\s*$", lines2)))
})

test_that("set_nass_key(renviron) errors if key exists and overwrite = FALSE", {
  skip_on_cran()
  skip_on_ci()
  
  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  
  suppressMessages(suppressWarnings({
    invisible(set_nass_key("persistkey", persist = "renviron", renviron_path = tmp, overwrite = TRUE, auth = FALSE))
  }))
  
  expect_error(
    suppressMessages(suppressWarnings(
      set_nass_key("newkey", persist = "renviron", renviron_path = tmp, overwrite = FALSE, auth = FALSE)
    )),
    "already exists",
    fixed = FALSE
  )
})

test_that("ensure_nass_key retrieves and authenticates when rnassqs installed", {
  skip_if_not_installed("rnassqs")
  skip_on_cran()
  skip_on_ci()
  
  withr::local_envvar(c(NASS_API_KEY = "ensurekey"))
  # Keep output quiet; authentication may print—suppress it
  suppressMessages(suppressWarnings({
    k <- ensure_nass_key(persist = "session")
  }))
  expect_equal(k, "ensurekey")
  # Confirm env set
  expect_equal(Sys.getenv("NASS_API_KEY", unset = ""), "ensurekey")
})
