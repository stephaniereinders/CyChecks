context("test-sal_df")

test_that("sal_df function works", {
  exampDF1 <- sal_df(limit = 20, fiscal_year = 2018)
  expect_error(sal_df(limit = -50)) # negative number of entries
  expect_error(sal_df(fiscal_year = 2005)) # fiscal year outside of range
  expect_error(sal_df(offset = -50)) # offset is a negative number
  expect_tibble(exampDF1, min.rows = 1, ncols = 8)
  expect_named(exampDF1, c("base_salary_date", "fiscal_year", "gender", "name", "place_of_residence",
                      "position", "total_salary_paid", "travel_subsistence"),
               ignore.order = TRUE, ignore.case = TRUE)
})


