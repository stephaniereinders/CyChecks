library("jsonlite")
library("tidyverse")

# Right now this is Lydia's token...
token <- "$$app_token=GJISvIEj4Jg2KmwRkV3oCGJrj"

limit <- 10000 # Max number of entries
offset <- 0 # Where to start gathering from (0 = the beginning)
fiscal_year <- 2018 # Will we just have different dataframes for each year?

get_dat <- function(token, limit, offset, fiscal_year){
  url <- sprintf("https://data.iowa.gov/resource/s3p7-wy6w.json?%s&$limit=%d&$offset=%d&$order=:id&department=Iowa%%20State%%20University&fiscal_year=%d", token, limit, offset, fiscal_year)
  sals <- fromJSON(url)
  return(sals)}

sals18 <- get_dat(token, limit, offset, fiscal_year)
use_data(sals18, overwrite = TRUE)


levels(forcats::as_factor(sals18$place_of_residence)) # 287 levels
levels(forcats::as_factor(sals18$position)) # what do to with all these different positions?

# how the positions break down
sals18 %>%
  dplyr::group_by(position)%>%
  dplyr::summarize(n = n())%>%
  arrange(desc(n))

# 201 people have no position **
