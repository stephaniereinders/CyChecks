######### Testing anonymize function ############

context("test_anonymize_function")


test_that("Anonymize function is working", {
  expect_error(anonymize(sals18, "XX")) #--test col name is appropriate
  expect_error(anonymize(filter(sals18, gender == "XX"), "place_of_residence")) #--feed it empty df
  expect_error(anonymize(sals18, "place_of_residence", 5)) #--feed it non-character algo

})

