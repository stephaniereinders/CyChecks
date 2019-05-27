context("test-get_profs")

test_that(
  "get_profs is working", {
    library(checkmate)
    library(dplyr)
    expect_error(get_profs(data = select(sals_dept, -position))) # no position column
    expect_error(get_profs(data = 1:10)) # input not a dataframe
    sals_test <- sal_df(fiscal_year = 2018, limit = 10000,
                        token = "$$app_token=GJISvIEj4Jg2KmwRkV3oCGJrj") %>% dplyr::distinct(name, .keep_all = TRUE) %>%
                            dplyr::select(-c(name, base_salary_date))
    sals18_test <- sals18 %>% dplyr::distinct(id, .keep_all = TRUE) %>%
      dplyr::select(-c(base_salary,department, organization, id, base_salary_date))
    expect_equal(get_profs(sals18_test),get_profs(sals_test))

  }
)
