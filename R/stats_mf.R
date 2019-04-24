#' Function to compare M vs F salaries within a department
#'
#' @name stats_mf
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import broom
#' @param data A dataframe of ISU salary data with academic department info. Default is for year 2018.
#' @return A dataframe of department, nested data, p-value for gender pay gap, and a verdict
#' @examples
#' stats_mf(data = filter(sals18, department == "AGRONOMY", grepl("PROF", position)))
#' @export

#library(tidyverse)
#library(broom)


# Actual function
stats_mf <- function(data = sals18){

  # Make sure it has the columns I want, and that it's not empty
  assertthat::assert_that(is.data.frame(data))
  #assertthat::assert_colnames(data, department, total_salary_paid, gender, only_colnames = FALSE)
  assertthat::not_empty(data)


  # make a function to be used for mapping within my real function,
  # made to be done within a department

  mymapfun <- function(data){

    # Figure out which positions have both a M and F
    poslist <- data %>%
      dplyr::group_by(position, gender) %>%
      dplyr::summarise(n = n()) %>%
      tidyr::spread(gender, n) %>%
      dplyr::filter(!is.na(M), !is.na(`F`)) %>%
      dplyr::pull(position)

    # Filter to get only those positions
    mydsub <- data %>%
      dplyr::filter(position %in% poslist)

    # Now I'm having trouble.
    # If it's empty, it needs to make a fake tibble
    # If it's not empty, fit a simple model

    assertthat::not_empty(mydsub)

    # This is what I want it to do if mydsub isn't empty
    myres <- broom::tidy(stats::anova(stats::lm(total_salary_paid ~
                                                  position + gender, data = mydsub))) %>%
      dplyr::filter(term  == "gender") %>%
      dplyr::select(term, p.value) %>%
      dplyr::rename("p_val" = p.value) %>%
      dplyr::mutate(verdict = ifelse(p_val < 0.2, "boo", "ok"))

    # This is what I want if it IS empty
    #myres <- tibble(term = "gender",
    #                         p_val = NA,
    #                         verdict = NA)

    return(myres)
  }


  yourstats <- data %>%
    dplyr::group_by(department) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      mod = data %>%
             purrr::map(possibly(mymapfun,
               otherwise = NA_real_))) %>%
    tidyr::unnest(mod)

  return(yourstats)

}

stats_mf(data = filter(sals18, department == "AGRONOMY", grepl("PROF", position)))

sals18 %>% filter(grepl("ENV", department)) %>% select(department)
sals18 %>% filter(grepl("POST", position)) %>% select(position)
