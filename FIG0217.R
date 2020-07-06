# TODO:
# - add 2nd row of x axis labels
rm(list = ls())
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
  # Calculate Category bar sizes (used by geom_segment later)
  group_by(Category) %>%
  dplyr::summarise(ymin = min(Value[Series == "Invisible Series"]), 
                   ymax = ymin + min(Value[Series == "Visible Series"])) %>%
  # Data for waterfall bars
  mutate(
    change = ymax - ymin,
    text_label = case_when(
      Category %in% c("Hires", "Transfers In") ~ paste0("+", change),
      Category %in% c("Exits", "Transfers Out") ~ paste0("-", change),
      TRUE ~ as.character(change)
    )
  ) %>%
  # Data for the lines between the bars
  mutate(
    next_category = lead(Category, 1),
    current_y = case_when(
      # Determine whether to take the top or bottom of the current bar as the starting point
      Category %in% c("1/1/2014", "Hires", "Transfers In") ~ ymax, # Top of the current bar
      TRUE ~ ymin # Bottom of the current bar
    ),
    # Determine whether to join to the top or bottom of the next bar
    next_y = case_when(
      Category %in% c("1/1/2014", "Hires") ~ lead(ymin, 1),  # Bottom of the next bar
      TRUE ~ lead(ymax, 1) # Top of the next bar
    )
  )

pt <- df %>%
  ggplot(aes(x = Category)) +
  # The two geom_segment calls here are only so that we can have slightly different position_nudge values for + and - bars. Probably overkill
  geom_segment(
    data = df %>% filter(next_category %in% c('Hires', 'Transfers In', 'Transfers Out')),
    aes(xend = next_category, y = current_y, yend = next_y),
    color = GRAY9,
    position = position_nudge(y = -0.3, x = -0.35),
    size = 0.25,
    linetype = 2 # Dashed line
  ) +
  geom_segment(
    data = df %>% filter(next_category %in% c('Exits', '12/31/2014')),
    aes(xend = next_category, y = current_y, yend = next_y),
    color = GRAY9,
    position = position_nudge(y = 0.1, x = -0.35),
    size = 0.25,
    linetype = 2 # Dashed line
  ) +
  scale_x_discrete(drop = FALSE) +
  geom_rect(aes(xmin = Category, xmax = Category, ymin = ymin, ymax = ymax),
    color = BLUE2,
    size = 20
  ) +
  geom_text(aes(y = ymax, label = text_label, vjust = 1.5), color = "white", size = 2) +
  labs(title = "2014 Headcount math", subtitle = "Though more employees transferred out of the team than transferred in,\naggressive hiring means overall headcount (HC) increased 16% over the course of the year")

ggsave(file.path("plot output", "FIG0217.png"), pt, width = 6, height = 4)
pt
