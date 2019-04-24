#' Create a dataframe of salary data for positions in the academic pipeline
#'
#' @description Create a dataframe that contains salaray data
#'   for graduate assistants, lecturers, and professors
#' @name get_academic_pipeline
#' @title get_academic_pipeline
#' @usage get_academic_pipeline(dataframe)
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @param dataframe A dataframe of salary data with a variable 'position'
#'
#' @return academic_pipeline A dataframe of professors' salary data with tidied position
#'   categories
#' @export
#'
#' @examples
#' get_academic_pipeline(all_sals)
#'

get_academic_pipeline <- function(dataframe=all_sals){

# create levels
df <- all_sals %>%
  dplyr::mutate(position = as.character(position)) %>%
  dplyr::filter(grepl('GRAD ASST|POSTDOC|LECTURER', position)) %>%
  dplyr::mutate(level = gsub(".*GRAD ASST-TA.*",'TA', position),
                level = gsub(".*GRAD ASST-RA.*",'RA', level),
                level = gsub(".*GRAD ASST-TA/RA.*",'TA/RA', level),
                level = gsub(".*GRAD ASST-AA.*",'AA', level),
                level = gsub(".*GRAD ASST-OTHER.*",'other', level),
                level = replace(level,level=="GRAD ASST",'graduate assistant'),
                level = gsub(".*SENIOR LECTURER.*",'senior', level),
                level = replace(level,level=="LECTURER",'lecturer'),
                level = gsub(".*RES ASSOC.*",'research associate', level),
                level = gsub(".*FELLOW.*",'fellow', level),
                level = gsub(".*TRAINEE.*",'trainee', level))

# replace positions
df <- df %>%
  dplyr::mutate(position = gsub(".*GRAD ASST.*",'graduate assistant', position),
                position = gsub(".*LECTURER.*",'lecturer', position),
                position = gsub(".*POSTDOC.*",'postdoc', position))
profs_df <- get_profs(all_sals)

academic_pipeline <- rbind(df,profs_df)

return(academic_pipeline)
}
