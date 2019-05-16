# getting empty data for Wyatt to fill in....

library(tidyverse)
library(CyChecks)

# params
token <- "$$app_token=GJISvIEj4Jg2KmwRkV3oCGJrj"
limit <- 150000 # Max number of entries
offset <- 0 # Where to start gathering from (0 = the beginning)
fiscal_year <- 2018 # Will we just have different dataframes for each year?

### function to get all data
get_dat2 <- function(token, limit, offset){
  url <- sprintf("https://data.iowa.gov/resource/s3p7-wy6w.json?%s&$limit=%d&$offset=%d&$order=:id&department=Iowa%%20State%%20University", token, limit, offset)
  s <- tibble::as_tibble(fromJSON(url))
  sals <- s %>%
    dplyr::select(-department)%>%
    dplyr::mutate(base_salary_date = lubridate::ymd_hms(base_salary_date))%>%
    dplyr::mutate_at(vars(fiscal_year, total_salary_paid, travel_subsistence), as.numeric)%>%
    dplyr::mutate(name = gsub(",","",name))%>%
    dplyr::mutate(position = stringr::str_trim(position, side = "right"))
  return(sals)}
# getting all data
all <- get_dat2(token, 150000, 0)
load("R/sysdata.rda") # loads department info
# match by department
all_sals <- left_join(all, depts, by = "name")

# grab rows without department
empties <- all_sals %>%
  filter(is.na(department)) %>%
  arrange(name)%>%
  select(name, fiscal_year, gender, position, department, organization)
#write.csv(empties, file = "department_info.csv")

# just professor department info
prof_empties <- empties %>%
  filter(grepl("PROF", position))
# write.csv(prof_empties, file = "prof_department_info.csv")

# department options
dept_levels <- depts %>%
  select(organization, department)%>%
  distinct(department, .keep_all = TRUE)
#write.csv(dept_levels, file = "department_levels.csv")
