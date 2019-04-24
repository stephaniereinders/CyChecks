# Function to be called inside real function
fun1 <- function(data) {
  # -------Start fun
  assertthat::assert_that(is.data.frame(data))
  assertable::assert_colnames(data, c("position",
                                      "gender",
                                      "total_salary_paid"), only_colnames = FALSE)
  assertthat::not_empty(data)
  assertthat::assert_that(is.factor(data$position),
                          is.factor(data$gender),
                          is.numeric(data$total_salary_paid))

  # Make sure there are two genders in that department (eye roll)
  twogenders <- data %>%
    dplyr::group_by(position, gender) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::filter(gender %in% c("F", "M")) %>%
    tidyr::spread(gender, n)

  if (ncol(twogenders) < 3) {
    myres <- tibble(term = "gender",
                    p_val = NA,
                    verdict = NA)

  } else {
    # Figure out which positions have both a M and F
    poslist <-  twogenders %>%
      dplyr::filter(!is.na(M),!is.na(`F`)) %>%
      dplyr::filter((M > 1 & F > 1)) %>% #--you need more than 1 of each to do a comparison
      dplyr::pull(position)

    # Filter to get only those positions
    mydsub <- data %>%
      dplyr::filter(position %in% poslist)

    # If it's empty, it needs to make a fake tibble
    # If it's not empty and has more than 1 position, fit a simple model

    #--If#1
    if (nrow(mydsub) > 0) {
      #--If#2
      if (length(poslist) > 1) {
        # This is what I want it to do if mydsub isn't empty
        myres <- broom::tidy(stats::anova(
          stats::lm(total_salary_paid ~
                      position + gender, data = mydsub)
        )) %>%
          dplyr::filter(term  == "gender") %>%
          dplyr::select(term, p.value) %>%
          dplyr::rename("p_val" = p.value) %>%
          dplyr::mutate(verdict = ifelse(p_val < 0.2, "boo", "ok"))
      } else {
        # This is what I want it to do if mydsub isn't empty but there is only 1 row
        myres <-
          broom::tidy(stats::anova(stats::lm(total_salary_paid ~
                                               gender, data = mydsub))) %>%
          dplyr::filter(term  == "gender") %>%
          dplyr::select(term, p.value) %>%
          dplyr::rename("p_val" = p.value) %>%
          dplyr::mutate(verdict = ifelse(p_val < 0.2, "boo", "ok"))
      }
    } else {
      # This is what I want if it IS empty
      myres <- tibble(term = "gender",
                      p_val = NA,
                      verdict = NA)
    }
  }
  return(myres)
}

#' Function to compare M vs F salaries within a department
#'
#' @name stats_mf
#' @importFrom tidyr spread nest unnest
#' @importFrom dplyr group_by summarise select rename mutate filter
#' @importFrom broom tidy
#' @importFrom purrr map possibly
#' @importFrom assertthat assert_that not_empty
#' @importFrom asserrtable assert_colnames
#' @param data A dataframe of ISU salary data with academic department info. Default is for year 2018.
#' @return A dataframe of department, nested data, p-value for gender pay gap, and a verdict
#' @examples
#' stats_mf(data = filter(sals18, department == "AGRONOMY", grepl("PROF", position)))
#' @export


# Actual function
stats_mf <- function(data = sals18){

  # Make sure it has the columns I want, and that it's not empty
  assertthat::assert_that(is.data.frame(data))
  assertable::assert_colnames(data, c("department","position", "gender", "total_salary_paid"), only_colnames = FALSE)
  assertthat::not_empty(data)


  yourstats <- data %>%
    dplyr::group_by(department) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      mod = data %>%
        purrr::map(purrr::possibly(fun1,
                                   otherwise = NA_real_))) %>%
    tidyr::unnest(mod) %>%
    arrange(p_val)

  return(yourstats)

}

# For trouble shooting
#library(tidyverse)
# hm <- sals18 %>% filter(department == "H SCI STUDNT SR")
# res <- stats_mf(data = hm)
#
# depts <- unique(sals18$department)
# depts[25]
# length(depts)
# for (i in 201:300) {
# hm <- sals18 %>% filter(department == depts[i])
# res1 <- stats_mf(data = hm)
# res <- rbind(res, res1)
# }
#
#res <- stats_mf(data = sals18)
