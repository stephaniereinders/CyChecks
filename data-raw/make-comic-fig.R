devtools::install_github("https://github.com/vanichols/CyChecks")
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(CyChecks) #--I'm not sure the best way to do this...
library(here)

sals18 <- sals18 %>%
  mutate_if(is.factor, as.character)

prf <- sals18 %>%
  mutate(position = as.character(position)) %>%
  filter(grepl("PROF", position)) %>%
  filter(position %in% c("ASST PROF", "ASSOC PROF", "PROF")) %>%
  group_by(position, gender) %>%
  summarise(sal = median(total_salary_paid))

fb <- sals18 %>%
  mutate(position = as.character(position)) %>%
  filter(grepl("COACH", position)) %>%
  filter(total_salary_paid == max(total_salary_paid)) %>%
  group_by(position) %>%
  summarise(sal = median(total_salary_paid)) %>%
  mutate(gender = "DNM*",
         position = "FOOTBALL COACH\n(for real)")


ww <- sals18 %>%
  mutate(position = as.character(position)) %>%
  filter(grepl("PRESIDENT", position)) %>%
  filter(total_salary_paid == max(total_salary_paid)) %>%
  group_by(position) %>%
  summarise(sal = median(total_salary_paid)) %>%
  mutate(gender = "DNM*")


prv <- sals18 %>%
  mutate(position = as.character(position)) %>%
  filter(grepl("PROVOST", position)) %>%
  filter(total_salary_paid == max(total_salary_paid)) %>%
  mutate(position = "PROVOST") %>%
  group_by(position) %>%
  summarise(sal = median(total_salary_paid)) %>%
  mutate(gender = "DNM*")


prf %>%
  bind_rows(fb, ww, prv) %>%
  mutate(dol_sal = dollar(round(sal, 0)),
         pos_fac = factor(position, levels = c("PRESIDENT",
                                               "PROVOST",
                                               "PROF",
                                               "ASSOC PROF",
                                               "ASST PROF",
                                               "FOOTBALL COACH\n(for real)")),
         gender = recode(gender,
                         M = "Male",
                         `F` = "Female"),
         gender = factor(gender, levels = c("Male", "Female", "DNM*"))) %>%

  #nice_sal = paste("$", round(sal, 0))) %>%
  ggplot(aes(pos_fac, sal)) +
  geom_rect(xmin = 0, xmax = 7, ymin = 0, ymax = 500000,
            fill = "white", color = "black")  +
  geom_hline(yintercept = 125000, color = "gray90") +
  geom_hline(yintercept = 250000, color = "gray90") +
  geom_hline(yintercept = 375000, color = "gray90") +

  geom_text(x = 0.5, y = 800000,
            label = "Iowa State University 2018\nActual Median Salaries",
            hjust = "left", fontface = "bold") +
  geom_text(x = 0.5, y = 650000,
            label = "*DNM = Does Not Matter", hjust = "left") +
  geom_bar(aes(fill = gender), stat = "identity",
           position = position_dodge2(width = 0.8, preserve = "single"), width = 0.5) +
  geom_label_repel(aes(label = dol_sal)) +

  scale_y_continuous(labels = dollar, breaks = c(0, 250000, 500000)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_manual(values = c("darkblue", "goldenrod2", "gray70")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

ggsave("README_files/static-figures/CyChecks-comic.png", height = 10, width = 7)
