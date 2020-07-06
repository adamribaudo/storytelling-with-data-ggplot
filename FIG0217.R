# TODO:
# - add lines between bars
# - add 2nd row of x axis labels
# - can't align bar labels to top of each bar without manually adjusting each one

library(tidyverse)

source("theme/theme_swd.R")

theme_set(theme_swd() + theme(
  axis.title = element_blank(),
  axis.title.y = element_blank(),
  axis.title.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y = element_blank(),
  axis.text.y = element_blank(),
  axis.line.x = element_line(),
  plot.subtitle = element_text(color = GRAY2, size = 9, hjust = 0),
  plot.margin = unit(c(1, 1, 1, 1), "cm")
))

df <- read_csv(file.path("data", "FIG0217.csv")) %>%
  select(-`Label 1`) %>%
  rename(Category = `Label 2`) %>%
  pivot_longer(cols = -Category, names_to = "Series", values_to = "Value") %>%
  mutate(Category = forcats::fct_relevel(factor(Category), "1/1/2014", "Hires", "Transfers In", "Transfers Out", "Exits", "12/31/2014")) %>%
  group_by(Category) %>%
  dplyr::summarise(ymin = min(Value[Series == "Invisible Series"]), ymax = ymin + min(Value[Series == "Visible Series"])) %>%
  ungroup() %>%
  mutate(
    change = ymax - ymin,
    text_label = case_when(
      Category %in% c("Hires", "Transfers In") ~ paste0("+", change),
      Category %in% c("Exits", "Transfers Out") ~ paste0("-", change),
      TRUE ~ as.character(change)
    )
  ) %>%
  unique()

joins_df <- df %>%
  arrange(Category) %>%
  mutate(
    next_category = lead(Category, 1),
    current_y = case_when(
      Category %in% c("1/1/2014", "Hires", "Transfers In") ~ ymax,
      TRUE ~ ymin
    ),
    next_y = case_when(
      Category %in% c("1/1/2014", "Hires") ~ lead(ymin, 1),
      TRUE ~ lead(ymax, 1)
    )
  ) %>%
  filter(!is.na(next_category))

pt <- df %>%
  ggplot(aes(x = Category)) +
  geom_segment(
    data = joins_df,
    aes(xend = next_category, y = current_y, yend = next_y),
    color = GRAY9,
    linetype = 2
  ) +
  scale_x_discrete(drop = FALSE) +
  geom_rect(aes(xmin = Category, xmax = Category, ymin = ymin, ymax = ymax),
    color = BLUE2,
    size = 20
  ) +
  geom_text(aes(y = ymax, label = text_label, vjust = 1.5), color = "white", size = 3.5) +
  labs(title = "2014 Headcount math", subtitle = "Though more employees transferred out of the team than transferred in,\naggressive hiring means overall headcount (HC) increased 16% over the course of the year")

ggsave(file.path("plot output", "FIG0217.png"), pt, width = 5.7, height = 4)
pt
