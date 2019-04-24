context("test-stats_mf")


test_that(
  "stats_mf is working", {
    library(checkmate)
    expect_error(stats_mf(data = select(sals_dept, -total_salary_paid))) # no total_salary_paid column
  expect_error(stats_mf(data = select(sals_dept, -department))) # no department column
  expect_error(stats_mf(data = select(sals_dept, -gender))) # no gender column
  expect_error(stats_mf(data = select(sals_dept, -position))) # no position column
  expect_error(stats_mf(data = (sals_dept %>%
                                  mutate(total_salary_paid = as.character(total_salary_paid))))) # salary isn't numeric

  }
  )
