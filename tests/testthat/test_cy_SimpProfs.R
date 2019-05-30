context("test_cy_SimpProfs")

test_that(
  "cy_SimpProfs is working", {
    library(checkmate)
    library(dplyr)
    expect_error(cy_SimpProfs(data = select(sals_dept, -position))) # no position column
    expect_error(cy_SimpProfs(data = 1:10)) # input not a dataframe
    sals_test <- sal_df(fiscal_year = 2018, limit = 10000,
                        token = "$$app_token=GJISvIEj4Jg2KmwRkV3oCGJrj") %>% dplyr::distinct(name, .keep_all = TRUE) %>%
                            dplyr::select(-c(name, base_salary_date))
    sals18_test <- sals18 %>% dplyr::distinct(id, .keep_all = TRUE) %>%
      dplyr::select(-c(base_salary,department, organization, id, base_salary_date))
    expect_equal(cy_SimpProfs(sals18_test),cy_SimpProfs(sals_test))

  }
)
