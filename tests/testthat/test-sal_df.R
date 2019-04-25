context("test-sal_df")
load("tests/testthat/sals_raw.rda")

test_that("sal_df function works", {
  library(jsonlite)
  library(dplyr)
  library(checkmate)
  expect_error(sal_df(limit = -50)) # negative number of entries
  expect_error(sal_df(fiscal_year = 2005)) # fiscal year outside of range
  expect_error(sal_df(offset = -50)) # offset is a negative number
  expect_tibble(sals_raw, min.rows = 1, ncols = 8)
  expect_named(sals_raw, c("base_salary_date", "fiscal_year", "gender", "name", "place_of_residence",
                      "position", "total_salary_paid", "travel_subsistence"),
               ignore.order = TRUE, ignore.case = TRUE)
})


