context("test-sal_df")
library(jsonlite)
library(dplyr)
library(checkmate)
sals_raw <-structure(list(base_salary_date = structure(c(1183248000, 1183248000,
                                                1183248000, 1183248000, 1183248000, 1183248000, 1183248000, 1183248000,
                                                1183248000, 1183248000), class = c("POSIXct", "POSIXt"), tzone = "UTC"),
                 fiscal_year = c(2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007),
                 gender = c("M", "F", "M", "M", "M", "M", "F", "F", "M", "F"),
                 name = c("ABASHT BEHNAM", "ABATE SARAH ANN", "ABBADI IMAD",
                          "ABBAS THOMAS EDWARD", "ABBEY JAMES DUANE", "ABBEY JOSHUA MICHAEL",
                          "ABBOTT BARBARA ANN B", "ABBOTT CHRISTY J", "ABBOTT ERIC ALAN",
                          "ABBOTT JESSICA MARIE"),
                 place_of_residence = c("STORY", "STORY", "COOK", "OAKLAND", "STORY",
                                        "STORY", "STORY", "BOONE", "STORY", "STORY"),
                 position = c("POSTDOC RES ASSOC", "CASUAL HOURLY", "SYS SUP SPEC III",
                              "CASUAL HOURLY", "GRAD ASST-TA", "CASUAL HOURLY",
                              "COMM SPEC III", "ASST MGR FS I", "PROF", "CASUAL HOURLY"),
                 total_salary_paid = c(37000, 3986.88, 42410.07, 1028.71, 16200, 1134.9, 59609, 38302,82467, 3005.05),
                 travel_subsistence = c(1535.52, NA, NA, NA, NA, NA, 1310.59, NA, 4429.17, NA)),
                 class = c("tbl_df","tbl", "data.frame"), row.names = c(NA, -10L))

test_that("sal_df function works", {
  expect_error(sal_df(limit = -50)) # negative number of entries
  expect_error(sal_df(limit = "fifty")) # not numeric
  expect_error(sal_df(offset = "fifty")) # not numeric
  expect_error(sal_df(fiscal_year = 2005)) # fiscal year outside of range
  expect_error(sal_df(fiscal_year = "Two thousand and seven")) #not numeric
  expect_error(sal_df(offset = -50)) # offset is a negative number
  checkmate::expect_tibble(sals_raw, min.rows = 1, ncols = 8)
  expect_named(sals_raw, c("base_salary_date", "fiscal_year", "gender", "name", "place_of_residence",
                      "position", "total_salary_paid", "travel_subsistence"),
               ignore.order = TRUE, ignore.case = TRUE)
  expect_equal(sals_raw, sal_df(fiscal_year = 2007, limit = 10))
  expect_equal(sals_raw, sal_df(fiscal_year = 2007, limit = 10, token = "$$app_token=GJISvIEj4Jg2KmwRkV3oCGJrj"))
})


