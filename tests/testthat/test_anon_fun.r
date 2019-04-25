######### Testing anonymize function ############

context("test_anonymize_function")


test_that("Anonymize function is working", {
  library(testthat)
  expect_true(is.character(cols_to_anon)) ##
  expect_true(is.data.frame(df)) #
  expect_true(is.function(anonymize)) #
  expect_error(is.string(algo)) # algorithm is a mix of numbers and letters
  show_failure(df$travel_subsistence) # The column has values.
  expect_true(is.numeric(df$total_salary_paid)) #
  expect_true(is.factor(df$gender)) # gender is factor
  expect_error(anonymize(data.frame(name = letters), cols_to_anon = "name", algo = "crc32")) #--test col name is appropriate

})

