library("jsonlite")
library("tidyverse")

# Right now this is Lydia's token...
token <- "$$app_token=GJISvIEj4Jg2KmwRkV3oCGJrj"
limit <- 150000 # Max number of entries
offset <- 0 # Where to start gathering from (0 = the beginning)

### getting all data
get_dat2 <- function(token, limit, offset){
  url <- sprintf("https://data.iowa.gov/resource/s3p7-wy6w.json?%s&$limit=%d&$offset=%d&$order=:id&department=Iowa%%20State%%20University", token, limit, offset)
  s <- tibble::as_tibble(fromJSON(url))
  sals <- s %>%
    dplyr::select(-base_salary)%>%
    dplyr::mutate(base_salary_date = lubridate::ymd_hms(base_salary_date))%>%
    dplyr::mutate_at(vars(total_salary_paid, travel_subsistence), as.numeric)%>%
    dplyr::mutate_at(vars(fiscal_year, gender, place_of_residence, position), forcats::as_factor)%>%
    dplyr::mutate(name = gsub(",","",name))
  return(sals)}
all_sals <- get_dat2(token, 150000, 0)

usethis::use_data(all_sals, overwrite = TRUE)

