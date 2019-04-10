library(rvest)
library(tidyverse)

# read in department info
depts <- readxl::read_xlsx("data-raw/Copy of Employees with Department and Org 4-8-19.xlsx")%>%
  dplyr::mutate_at(vars(ORG_SHORT_NAME, DRCTY_DEPT_NAME), forcats::as_factor)%>%
  dplyr::rename_all(tolower)


# does it merge with sals_18?
data("sals18")
left_join(sals18, depts, by = "name")



# From Susan
#
# .directory (for the whole profile)
# html_nodes(".directory a:first-child") %>% html_text() to get the names
# html_nodes(".directory") %>% html_text() %>% str_extract("Title:.*Department:") %>% str_remove("Title:") %>% str_remove("Department:") %>% str_trim() to get the title
# html_nodes(".directory") %>% html_text() %>% str_extract("Department:.*Office:") %>% str_remove("Title:") %>% str_remove("Department:") %>% str_trim() to get the Department
