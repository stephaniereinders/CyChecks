######### Testing anonymize function ############

context("test_anonymize_function")


test_that("Anonymize function is working", {
  expect_error(anonymize(sals18, "XX")) #--test col name is appropriate

})

