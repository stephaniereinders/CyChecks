#' Create a dataframe of salary data
#' @description Scrape ISU salary data by year from the web and turn it into a tidy dataframe
#'
#' See \href{https://github.com/vanichols/CyChecks}{CyChecks}
#'
#' @param limit How many data entries you'd like to receive (default = 1000)
#' @param offset Where you'd like the dataentries to start pulling from (default = 0)
#' @param fiscal_year The fiscal year the data are taken from. Limited to 2007-2018
#' @param token An API token. Only necessary for large amounts of datascraping. Generated from this \href{"https://dev.socrata.com/foundry/data.iowa.gov/s3p7-wy6w"}{website}
#' @return A dataframe with salary information, position, and date for Iowa State University employees
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate ymd_hms
#' @importFrom checkmate assertNumber
#' @importFrom checkmate assertTibble
#' @importFrom tibble as_tibble
#' @importFrom checkmate assertDataFrame
#' @importFrom lubridate ymd_hms
#' @importFrom forcats as_factor
#' @importFrom stringr str_trim
#' @importFrom jsonlite fromJSON
#' @details An API (or APP) token isn't necessary for scraping data, but it will help speed up the data grabbing process and will allow users to get nearly unlimited data.
#'
#' @export
#' @author Lydia English


sal_df<- function(limit= 1000, offset = 0, fiscal_year = 2007, token = NULL){
  checkmate::assertNumber(limit, lower = 0)
  checkmate::assertNumber(offset, lower = 0)
  checkmate::assertNumber(fiscal_year, lower =2007, upper = 2018)
  if (!is.null(token)){
    url <- sprintf("https://data.iowa.gov/resource/s3p7-wy6w.json?%s&$limit=%d&$offset=%d&$order=:id&department=Iowa%%20State%%20University&fiscal_year=%d", token, limit, offset, fiscal_year)
  }
  else {
    url <- sprintf("https://data.iowa.gov/resource/s3p7-wy6w.json?$limit=%d&$offset=%d&$order=:id&department=Iowa%%20State%%20University&fiscal_year=%d", limit, offset, fiscal_year)
  }
  s <- tibble::as_tibble(jsonlite::fromJSON(url))
  checkmate::assertTibble(s, min.rows = 1, ncols =10)
  sals <- s %>%
    dplyr::select(-c(base_salary,department))%>%
    dplyr::mutate(base_salary_date = lubridate::ymd_hms(base_salary_date))%>%
    dplyr::mutate_at(vars(total_salary_paid, travel_subsistence), as.numeric)%>%
    dplyr::mutate_at(vars(fiscal_year, gender, place_of_residence, position), forcats::as_factor)%>%
    dplyr::mutate(name = gsub(",","",name)) %>%
    dplyr::mutate(position = forcats::as_factor(stringr::str_trim(position, side = "right")))
  checkmate::assertDataFrame(sals)
  return(sals)
}

