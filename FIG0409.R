library(tidyverse)
library(forcats)
source("helper_functions.R")
source("theme/theme_swd.R")

theme_set(theme_swd() + theme(axis.ticks = element_blank(),
                              axis.line = element_blank(),
                              plot.title = element_text(color = GRAY4, size = 16),
                              axis.title.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.text.y = element_text(color = GRAY4, size = 10),
                              axis.title.x = element_text(color = GRAY6, size = 12, hjust = .1)))

df <- read_csv(file.path("data", "FIG0407-09.csv")) %>%
  mutate(category = fct_rev(fct_relevel(factor(category), df$category))) %>%
  # Top design concerns
  mutate(fill = case_when(
    `concerns-per-1000` >= 10 ~ RED2,
    T ~ GRAY9
  )) %>%
  mutate(fill = case_when(
    category == "Tires make excessive noise while driving" ~ RED1,
    category == "Engine makes abnormal/excessive noise" ~ RED1,
    category == "Excessive wind noise" ~ RED1,
    T ~ fill
  ))

pt <- ggplot(df, aes(x = category, y = `concerns-per-1000`)) +
  geom_col(aes(fill = fill), width = .7) +
  scale_fill_identity() +
  geom_text(aes(label = `concerns-per-1000`), nudge_y = -1, nudge_x = .05, color = "white", size = 4) +
  scale_y_continuous(name = "concerns per 1,000", position = "right") +
  coord_flip() +
  labs(title = "Top 10 design concerns")

pt %>%
  save_and_show_plot(width = 6.5, height = 3.7, "FIG0409.png")
