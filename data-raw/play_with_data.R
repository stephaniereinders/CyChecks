data("all_sals")
data("sals18")
names(sals18)

library(tidyverse)
library(broom)

dat <- as_tibble(sals18)

dat %>%
  filter(department == "AGRONOMY") %>%
  filter(grepl('PROF', position)) %>%
  ggplot(aes(gender, total_salary_paid, color = position, group = position)) +
  geom_jitter(size = 2, width = 0.2, alpha = 0.5) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  facet_grid(~department)

# Calculate p-value of gender
dat %>%
  #filter(!is.na(department)) %>%
  filter(department == "AGRONOMY") %>%
  filter(grepl('PROF', position)) %>%
  filter(position %in% c("PROF", "ASSOC PROF", "ADJ ASST PROF")) %>%
  group_by(department) %>%
  nest() %>%
  mutate(mod = data %>% map(~lm(total_salary_paid ~ gender*position, data = .)),
         res = mod %>% map(~anova(.)),
         tidy = res %>% map(~tidy(.))) %>%
  unnest(tidy)




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


