

library(rvest)
library(tidyverse)

# From Susan
#
# .directory (for the whole profile)
# html_nodes(".directory a:first-child") %>% html_text() to get the names
# html_nodes(".directory") %>% html_text() %>% str_extract("Title:.*Department:") %>% str_remove("Title:") %>% str_remove("Department:") %>% str_trim() to get the title
# html_nodes(".directory") %>% html_text() %>% str_extract("Department:.*Office:") %>% str_remove("Title:") %>% str_remove("Department:") %>% str_trim() to get the Department
