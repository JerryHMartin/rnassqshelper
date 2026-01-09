
test_that("get_nass_yield returns a data.frame when as_tibble = FALSE", {
  result <- get_nass_yield(state = "TEXAS", year = 2020, as_tibble = FALSE)
  expect_s3_class(result, "data.frame")
})

test_that("get_nass_yield returns a tibble by default", {
  result <- get_nass_yield(state = "TEXAS", year = 2020)
  expect_s3_class(result, "tbl_df")
})
