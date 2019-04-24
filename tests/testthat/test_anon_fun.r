######### Testing anonymize function ############

context("test_anonymize_function")


test_that("Anonymize function is working", {
  library(testthat)
  expect_error(anonymize(data.frame(name = letters), cols_to_anon = "name", algo = "crc32")) #--test col name is appropriate
})

