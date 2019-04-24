#' Create a dataframe of professors' salary data
#'
#' @description Create a dataframe that only contains professors' salaray data
#'   from a larger dataframe of salary data. Create a new variable 'position_simplified'
#'   that groups professor titles into categories such as associate, visting,
#'   and emeritus.
#' @name get_profs
#' @title get_profs
#' @usage get_profs(data)
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @param data A dataframe of salary data with a variable 'position'
#'
#' @return profs A dataframe of professors' salary data with tidied position
#'   categories
#' @export
#'
#' @examples
#' get_profs(data)
#'

get_profs <- function(data=sals_dept){

# Make sure it has the columns I want, and that it's not empty
assertthat::assert_that(is.data.frame(data))
assertable::assert_colnames(data, c("position"), only_colnames = FALSE)
assertthat::not_empty(data)

# Filter dataframe for all positions that contain the string 'PROF'
data <- data %>%
  dplyr::mutate(position = as.character(position)) %>%
  dplyr::filter(grepl('PROF', position))

# Create a new variable 'position_simplified' that groups professor titles into groups
# such as associate, visting, and emeritus.
data <- data %>% dplyr::mutate(position_simplified = gsub(".*EMER.*",'emeritus', position),
                          position_simplified = gsub(".*DISTG.*",'distinguished', position_simplified),
                          position_simplified = gsub(".*UNIV.*",'university',position_simplified),
                          position_simplified = gsub(".*MORRILL.*",'morrill',position_simplified),
                          position_simplified = gsub(".*ADJ.*",'adjunct',position_simplified),
                          position_simplified = gsub(".*AFFIL.*",'affiliate',position_simplified),
                          position_simplified = gsub(".*VSTG.*",'visiting',position_simplified),
                          position_simplified = gsub(".*ASST.*", "assistant", position_simplified),
                          position_simplified = gsub(".*ASSOC.*", "associate", position_simplified),
                          position_simplified = gsub(".*COLLAB.*",'collab', position_simplified),
                          position_simplified = gsub(".*(CHAIR|CHR).*",'chair',position_simplified),
                          position_simplified = gsub(".*(RES PROF|CLIN PROF).*",'professor',position_simplified),
                          position_simplified = replace(position_simplified,position_simplified=="PROF",'professor'))

data <- data %>% dplyr::mutate(position = gsub(".*PROF.*",'professor', position))

return(data)
}




