#' Create a dataframe of professors' salary data
#'
#' @description Create a dataframe that only contains professors' salaray data
#'   from a larger dataframe of salary data. Create a new variable 'level'
#'   that groups professor titles into categories such as associate, visting,
#'   and emeritus.
#' @name get_profs
#' @title get_profs
#' @usage get_profs(dataframe)
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @param dataframe A dataframe of salary data with a variable 'position'
#'
#' @return profs A dataframe of professors' salary data with tidied position
#'   categories
#' @export
#'
#' @examples
#' get_profs(all_sals)
#'

get_profs <- function(dataframe=all_sals){

# Filter dataframe for all positions that contain the string 'PROF'
dataframe <- dataframe %>%
  dplyr::mutate(position = as.character(position)) %>%
  dplyr::filter(grepl('PROF', position))

# Create a new variable 'level' that groups professor titles into groups
# such as associate, visting, and emeritus.
dataframe <- dataframe %>% dplyr::mutate(level = gsub(".*EMER.*",'emeritus', position),
                          level = gsub(".*DISTG.*",'distinguished', level),
                          level = gsub(".*UNIV.*",'university',level),
                          level = gsub(".*MORRILL.*",'morrill',level),
                          level = gsub(".*ADJ.*",'adjunct',level),
                          level = gsub(".*AFFIL.*",'affiliate',level),
                          level = gsub(".*VSTG.*",'visiting',level),
                          level = gsub(".*ASST.*", "assistant", level),
                          level = gsub(".*ASSOC.*", "associate", level),
                          level = gsub(".*COLLAB.*",'collab', level),
                          level = gsub(".*(CHAIR|CHR).*",'chair',level),
                          level = gsub(".*(RES PROF|CLIN PROF).*",'professor',level),
                          level = replace(level,level=="PROF",'professor'))

dataframe <- dataframe %>% dplyr::mutate(position = gsub(".*PROF.*",'professor', position))

return(dataframe)
}




