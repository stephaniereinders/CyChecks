#' Create a dataframe of salary data for positions in the academic pipeline
#'
#' @description Create a dataframe that contains salaray data for graduate
#'   assistants, lecturers, and professors
#' @name get_academic_pipeline
#' @title get_academic_pipeline
#' @usage get_academic_pipeline(dataframe)
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @param dataframe A dataframe of salary data with a variable 'position'
#'
#' @return academic_pipeline A dataframe of graduate assistants', lecturers' and
#'   professors' salary data with tidied position and level variables
#' @export
#'
#' @examples
#' get_academic_pipeline(dataframe)
#'

get_academic_pipeline <- function(dataframe=all_sals){

# create levels
df <- all_sals %>%
  dplyr::mutate(position = as.character(position)) %>%
  dplyr::filter(grepl('GRAD ASST|POSTDOC|LECTURER', position)) %>%
  dplyr::mutate(position_simplified = gsub(".*GRAD ASST-TA.*",'TA', position),
                position_simplified = gsub(".*GRAD ASST-RA.*",'RA', position_simplified),
                position_simplified = gsub(".*GRAD ASST-TA/RA.*",'TA/RA', position_simplified),
                position_simplified = gsub(".*GRAD ASST-AA.*",'AA', position_simplified),
                position_simplified = gsub(".*GRAD ASST-OTHER.*",'other', position_simplified),
                position_simplified = replace(position_simplified,position_simplified=="GRAD ASST",'graduate assistant'),
                position_simplified = gsub(".*SENIOR LECTURER.*",'senior', position_simplified),
                position_simplified = replace(position_simplified,position_simplified=="LECTURER",'lecturer'),
                position_simplified = gsub(".*RES ASSOC.*",'research associate', position_simplified),
                position_simplified = gsub(".*FELLOW.*",'fellow', position_simplified),
                position_simplified = gsub(".*TRAINEE.*",'trainee', position_simplified))

# replace positions
df <- df %>%
  dplyr::mutate(position = gsub(".*GRAD ASST.*",'graduate assistant', position),
                position = gsub(".*LECTURER.*",'lecturer', position),
                position = gsub(".*POSTDOC.*",'postdoc', position))

# Get prof positions
profs_df <- get_profs(dataframe)

# Combine dataframes
academic_pipeline <- rbind(df,profs_df)

return(academic_pipeline)
}
