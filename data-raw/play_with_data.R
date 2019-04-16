data("all_sals")
data("sals18")
names(sals18)

library(tidyverse)
dat <- as_tibble(sals18)

cdat %>%
  filter(department == "AGRONOMY") %>%
  filter(grepl('PROF', position)) %>%
  ggplot(aes(gender, total_salary_paid)) +
  geom_jitter(aes(color = position), size = 3, width = 0.2) +
  facet_grid(~department)


dat %>%
  #filter(department == "AGRONOMY") %>%
  filter(grepl('PROF', position),
         gender %in% c('M', 'F')) %>%
  group_by(organization) %>%
  mutate(n = n()) %>%
  filter(n > 2) %>%
  ggplot(aes(gender, total_salary_paid)) +
  geom_jitter(aes(color = position), size = 3, width = 0.2) +
  stat_summary(fun.y = mean, geom = "point") +
  guides(color = F) +
  facet_wrap(~organization)


dat %>%
  #filter(department == "AGRONOMY") %>%
  filter(grepl('ASST PROF', position),
         gender %in% c('M', 'F')) %>%
  group_by(organization) %>%
  mutate(n = n()) %>%
  filter(n > 2) %>%
  ggplot(aes(gender, total_salary_paid)) +
  geom_jitter(aes(color = position), size = 3, width = 0.2) +
  stat_summary(fun.y = mean, geom = "point") +
#  guides(color = F) +
  facet_wrap(~organization)


