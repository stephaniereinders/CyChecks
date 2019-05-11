library(tidyverse)
library(janitor)

myprofs <- c("ASST PROF", "ASSOC PROF", "PROF")

sals18 %>%
  filter(grepl("PROF", position),
         department == "AGRONOMY") %>%
  tabyl(position) %>%
  arrange(-percent)

raw <- sals18 %>%
  filter(grepl("PROF", position)) %>%
  mutate(prof_simp = ifelse(position %in% myprofs, position, "OTHER"),
         prof_simp = factor(prof_simp, levels = c(myprofs, "OTHER"))) %>%
  filter(department == "AGRONOMY")

mns <- raw %>%
  filter(prof_simp != "OTHER") %>%
  group_by(prof_simp, gender) %>%
  summarise(total_salary_paid = mean(total_salary_paid))


ggplot(raw, aes(gender, total_salary_paid)) +
  geom_col(data = raw %>%
             filter(prof_simp != "OTHER") %>%
             group_by(prof_simp, gender) %>%
             summarise(total_salary_paid = mean(total_salary_paid)),
           aes(gender, total_salary_paid)) +
  geom_point() +
  facet_wrap(~prof_simp)

sals_dept %>%
  filter(grepl("PROF", position), department == "AGRONOMY") %>%
  group_by(fiscal_year, gender) %>%
  summarise(n = n()) %>%

  ggplot(aes(fiscal_year, n, color = gender)) +
  geom_line() +
  geom_point()
