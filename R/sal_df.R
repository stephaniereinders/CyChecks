#' Create a dataframe of salary data
#' @description Scrape ISU salary data by year from the web and turn it into a tidy dataframe
#'
#' See \href{https://github.com/vanichols/CyChecks}{CyChecks}
#'
#' @param limit The number of data entries (rows in the dataframe) you'd like to receive (default = 1000)
#' @param offset Where you'd like the data entries to start pulling from (default = 0). If using the
#' default of 0, then the dataset will start pulling from the beginning of alphabet.
#' This argument is useful to change when using the function multiple times.
#' @param fiscal_year The fiscal year the data are taken from. Limited to 2007-2018.
#' Can only enter one fiscal year at a time.
#' @param token An API token. Only necessary for large amounts of datascraping. Generated from this \href{"https://dev.socrata.com/foundry/data.iowa.gov/s3p7-wy6w"}{website}
#' @return A dataframe with salary information, position, and gender for Iowa State University employees in a given fiscal year.
#'
#' @importFrom dplyr mutate select vars
#' @importFrom lubridate ymd_hms
#' @importFrom checkmate assertNumber assertTibble
#' @importFrom tibble as_tibble
#' @importFrom lubridate ymd_hms
#' @importFrom stringr str_trim
#' @importFrom jsonlite fromJSON
#' @details An API (or APP) token isn't necessary for scraping data, but it will help speed up the data grabbing process and will allow users to get nearly unlimited data.
#' @author Lydia English
#' @examples
#' sal_df()
#' x <- sal_df(limit = 10, fiscal_year = 2015)
#' @export
#'

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
    dplyr::mutate_at(dplyr::vars(fiscal_year, total_salary_paid, travel_subsistence), as.numeric)%>%
    dplyr::mutate(name = gsub(",","",name)) %>%
    dplyr::mutate(position = stringr::str_trim(position, side = "right"))

  checkmate::assertTibble(sals, min.rows = 1, ncols = 8)
  return(sals)
}
