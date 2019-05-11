top10dat <- sals_dept %>%
  filter(fiscal_year == 2018)  %>%
  top_n(10, total_salary_paid) %>%
  mutate(rank = rank(total_salary_paid, ties.method="first")) %>%
  select(fiscal_year, position, total_salary_paid, rank, gender) %>%
  arrange(-rank) %>%
  mutate(rank2 = as.character(rank))

ggplot(data = top10dat,
       aes(x = reorder(rank2, -rank),
           y = total_salary_paid/1000,
           fill = gender)) +
  geom_col() +
  theme_bw() +
  labs(x = NULL, y = "Total Salary Paid\nThousands of $", fill = "Gender") +
  scale_fill_manual(values = c(M = "darkblue",
                               `F` = "goldenrod")) +

  scale_x_discrete(labels = top10dat$position) +

    theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = c(0.99, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_rect(linetype = "solid", color = "black"
    ))


sals_dept %>%
  filter(fiscal_year == 2018,
         total_salary_paid < 500000,
         department == "AGRONOMY") %>%
  group_by(gender) %>%
  mutate(rank = rank(total_salary_paid, ties.method = "first"),
         tot = max(rank),
         pct = rank/tot) %>%
  ggplot(aes(pct, total_salary_paid, color = gender)) +
  geom_point()


sals_dept %>%
  filter(fiscal_year == 2018,
         total_salary_paid < 500000,
         department == "AGRONOMY") %>%
  ggplot(aes(total_salary_paid, color = gender, fill = gender)) +
  geom_density(alpha = 0.5)
