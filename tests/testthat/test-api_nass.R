# Tests for get_nass_key() helper
# These tests verify that the function correctly retrieves the API key
# and provides informative errors when the key is missing.

test_that("get_nass_key() returns the key when set", {
  # Temporarily set the environment variable for this test only
  withr::local_envvar(c(NASS_API_KEY = "TESTKEY123"))
  
  key <- get_nass_key()
  expect_equal(key, "TESTKEY123")
})

test_that("get_nass_key() errors when key is missing", {
  # Simulate a missing API key
  withr::local_envvar(c(NASS_API_KEY = ""))
  
  expect_error(
    get_nass_key(),
    "NASS_API_KEY is not set"
  )
})

test_that("get_nass_key() error message is helpful", {
  # Ensure the error message includes setup instructions
  withr::local_envvar(c(NASS_API_KEY = ""))
  
  expect_error(
    get_nass_key(),
    "Sys.setenv"
  )
})

