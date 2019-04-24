context("test-get_profs")

test_that("get_profs is working", {
  library(checkmate)
  expect_error(get_profs(data = select(sals_dept, -position))) # no position column
  expect_error(get_profs(data = 1:10)) # input is not a dataframe
})
