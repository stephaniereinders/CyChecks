context("test_run_Shiny")
library(stringr)

test_that("run_shiny_all_works ",{
  appDir <- system.file("shiny-examples", "myapp", package = "CyChecks")
  appDir2 <- system.file("shiny-examples", "bipbop", package = "Cychecks")
  expect_true(str_detect(appDir, "shiny-examples"))
  expect_error(cy_RunShiny("app.R")) # don't need to put anything in
})
