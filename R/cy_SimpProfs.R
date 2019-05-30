#' Extract professors' salary data and assign simplified categories of 'professor'
#' @description Create a dataframe that only contains professors' salary data
#'   from a larger dataframe of salary data. Reassign position as a simplified title in one of four #'   categories: assistant, associate, full professor, and other.
#' @title cy_SimpProfs
#' @usage cy_SimpProfs(data)
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @param data A dataframe of salary data with a variable 'position'
#'
#' @return profs A dataframe of professors' salary data with tidied position
#'   categories in a new column titled 'pos_simp'
#' @export
#'
#' @examples
#' cy_SimpProfs()
#'

cy_SimpProfs <- function(data=sals_dept){

assertthat::assert_that(is.data.frame(data))
assertthat::assert_that("position" %in% names(data))

# Define simple categories to keep
myprofs <- c("ASST PROF", "ASSOC PROF", "PROF")

# Filter only prof and simplify
profs <-
  data %>%
  dplyr::filter(grepl("PROF", position)) %>%
  dplyr::mutate(prof_simp = ifelse(position %in% myprofs, position, "OTHER"),
         pos_simp = factor(prof_simp, levels = c(myprofs, "OTHER")))
return(profs)
}


