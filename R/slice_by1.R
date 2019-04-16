#' Function to slice dataframe of salaries by a single variable
#'
#' @name slice_by1
#' @aliases slice_by1
#' @title slice_by1
#' @usage slice_by1(all_sals,slice_variable)
#' @import tidyverse
#' @param all_sals A dataframe of ISU salary data. Default is for year 2007.
#' @param slice_var A variable to group_by
#' @return A dataframe of mean, median, min, 1st quartile, 3rd quartile, and max of total_salary_paid, grouped by slice_variable
#' @examples
#' slice_by1(all_sals,'gender')
#' slice_by(all_sals,'position')
#' @export


slice_by1 <- function(data = all_sals, slice_var='gender'){
  df <- all_sals %>%
    dplyr::filter(!is.na(total_salary_paid)) %>%
    dplyr::group_by_(slice_var) %>%
    dplyr::summarize(mean = mean(total_salary_paid),
            min = min(total_salary_paid),
            twentyfive = quantile(total_salary_paid,0.25),
            median = median(total_salary_paid),
            seventyfive = quantile(total_salary_paid,0.75),
            max = max(total_salary_paid),
            n=n())

return(df)
}
slice_by1()
