######### Testing anonymize function ############


context("test_anonymize_function")

library(testthat)

test_that("Anonymize function is working", {
  expect_true(is.character(cols_to_mask)) # 
  expect_true(is.data.frame(data)) #
  expect_true(is.function(anonymize)) #
  expect_error(is.string(algo)) # algorithm is a mix of numbers and letters
  show_failure(data$travel_subsistence) # The column has values. 
  expect_true(is.numeric(data$total_salary_paid)) # 
  expect_true(is.factor(data$gender)) # gender is factor
})