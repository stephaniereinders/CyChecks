library(rvest)
library(tidyverse)
library(jsonlite)

# Right now this is Lydia's token...
token <- "$$app_token=GJISvIEj4Jg2KmwRkV3oCGJrj"
limit <- 150000 # Max number of entries
offset <- 0 # Where to start gathering from (0 = the beginning)
fiscal_year <- 2018 # Will we just have different dataframes for each year?

# get 2018 data
get_dat <- function(token, limit, offset, fiscal_year){
  url <- sprintf("https://data.iowa.gov/resource/s3p7-wy6w.json?%s&$limit=%d&$offset=%d&$order=:id&department=Iowa%%20State%%20University&fiscal_year=%d", token, limit, offset, fiscal_year)
  s <- tibble::as_tibble(fromJSON(url))
  sals <- s %>%
    dplyr::select(-c(base_salary, department))%>%
    dplyr::mutate(base_salary_date = lubridate::ymd_hms(base_salary_date))%>%
    dplyr::mutate_at(vars(total_salary_paid, travel_subsistence), as.numeric)%>%
    dplyr::mutate_at(vars(fiscal_year, gender, place_of_residence, position), forcats::as_factor)%>%
    dplyr::mutate(name = gsub(",","",name))
  return(sals)}
sals18 <- get_dat(token, limit, offset, fiscal_year = 2018)

# read in department info
depts <- readxl::read_xlsx("data-raw/Copy of Employees with Department and Org 4-8-19.xlsx")%>%
  dplyr::mutate_at(vars(ORG_SHORT_NAME, DRCTY_DEPT_NAME), forcats::as_factor)%>%
  dplyr::rename_all(tolower)%>%
  dplyr::rename("organization" = "org_short_name")%>%
  dplyr::rename("department" = "drcty_dept_name")

# merge with sal 18
sals18 <- left_join(sals18, depts, by = "name")

usethis::use_data(sals18, overwrite = TRUE)

# From Susan
#
# .directory (for the whole profile)
# html_nodes(".directory a:first-child") %>% html_text() to get the names
# html_nodes(".directory") %>% html_text() %>% str_extract("Title:.*Department:") %>% str_remove("Title:") %>% str_remove("Department:") %>% str_trim() to get the title
# html_nodes(".directory") %>% html_text() %>% str_extract("Department:.*Office:") %>% str_remove("Title:") %>% str_remove("Department:") %>% str_trim() to get the Department
