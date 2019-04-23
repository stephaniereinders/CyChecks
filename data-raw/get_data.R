library("jsonlite")
library("tidyverse")

# Department info... (should this be deleted now that it has been used?)
# depts <- readxl::read_xlsx("data-raw/DeptOrgInfo.xlsx")%>%
#   dplyr::mutate_at(vars(ORG_SHORT_NAME, DRCTY_DEPT_NAME), forcats::as_factor)%>%
#   dplyr::rename_all(tolower)%>%
#   dplyr::rename("organization" = "org_short_name")%>%
#   dplyr::rename("department" = "drcty_dept_name")%>%
#   dplyr::mutate(name = str_trunc(name, 20, ellipsis = ""))
#
# usethis::use_data(depts, overwrite = TRUE, internal = TRUE)

# Right now this is Lydia's token...
token <- "$$app_token=GJISvIEj4Jg2KmwRkV3oCGJrj"
limit <- 150000 # Max number of entries
offset <- 0 # Where to start gathering from (0 = the beginning)
fiscal_year <- 2018 # Will we just have different dataframes for each year?

### function to get all data
get_dat2 <- function(token, limit, offset){
  url <- sprintf("https://data.iowa.gov/resource/s3p7-wy6w.json?%s&$limit=%d&$offset=%d&$order=:id&department=Iowa%%20State%%20University", token, limit, offset)
  s <- tibble::as_tibble(fromJSON(url))
  sals <- s %>%
    dplyr::select(-c(base_salary, department))%>%
    dplyr::mutate(base_salary_date = lubridate::ymd_hms(base_salary_date))%>%
    dplyr::mutate_at(vars(total_salary_paid, travel_subsistence), as.numeric)%>%
    dplyr::mutate_at(vars(fiscal_year, gender, place_of_residence), forcats::as_factor)%>%
    dplyr::mutate(name = gsub(",","",name))%>%
    dplyr::mutate(position = forcats::as_factor(stringr::str_trim(position, side = "right")))
  return(sals)}
all <- get_dat2(token, 150000, 0)
load("R/sysdata.rda") # loads department info
all_sals <- left_join(all, depts, by = "name") # joining everything together
# anonymizing
cols_to_anon <- "name"
anonymize <- function(x, cols_to_anon, algo = "crc32"){
  if(!require(digest)) stop("digest package is required")
  to_anon <- dplyr::select(x, cols_to_anon)
  unname(apply(to_anon, 1, digest, algo = algo))
}
sals_dept <- all_sals %>%
  dplyr::mutate(id = anonymize(., cols_to_anon)) %>%
  dplyr::select(-name)


# data from just 2018

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
s18 <- get_dat(token, limit, offset, fiscal_year = 2018)
sals18 <- left_join(s18, depts, by = "name")

# re-writing tables
usethis::use_data(sals18, overwrite = TRUE)
usethis::use_data(all_sals, overwrite = TRUE)
usethis::use_data(sals_dept, overwrite = TRUE)
