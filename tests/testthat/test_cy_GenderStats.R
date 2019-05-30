context("test-cy_GenderStats")

test_that(
  "cy_GenderStats is working", {
  library(checkmate)
  system.file("extdata", "depts.csv", package = "Cychecks")
  expect_error(cy_GenderStats(data = select(sals_dept,
                                            -total_salary_paid))) # no total_salary_paid column
  expect_error(cy_GenderStats(data = select(sals_dept,
                                            -department))) # no department column
  expect_error(cy_GenderStats(data = select(sals_dept,
                                            -gender))) # no gender column
  expect_error(cy_GenderStats(data = select(sals_dept,
                                            -position))) # no position column
  expect_error(cy_GenderStats(data = (sals_dept %>%
                                  mutate(total_salary_paid = as.character(total_salary_paid)))))# salary isn't numeric
  sals_test <- sal_df(fiscal_year = 2018, limit = 10000, token = "$$app_token=GJISvIEj4Jg2KmwRkV3oCGJrj") %>%
    left_join(., depts, by = "name")
  expect_equal(cy_GenderStats(sals18) %>% select(p_val), cy_GenderStats(sals_test) %>% select(p_val))
  }
  )

