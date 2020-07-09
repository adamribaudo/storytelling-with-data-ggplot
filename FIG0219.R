rm(list = ls())
library(tidyverse)
library(ggtext)
library(lemon)
source("helper_functions.R")
source("theme/theme_swd.R")

theme_set(theme_swd() + theme(
  axis.title.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.y = element_text(color = GRAY3, size = 8),
  panel.border = element_blank(),
  axis.line = element_line(),
  axis.title.x = element_text(hjust = 0.03, color = GRAY6),
  plot.subtitle = element_markdown(hjust = 0.65),
  axis.line.y = element_blank()
))

df <- read_csv(file.path("data", "FIG0219.csv")) %>%
  select(-Total) %>%
  rename(Item = `X1`) %>%
  pivot_longer(cols = -Item, names_to = "Answer", values_to = "Value") %>%
  mutate(
    Answer = factor(Answer, levels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    Value = parse_number(Value) / 100
  )

df$Answer <- fct_rev(df$Answer)
levels(df$Answer)

color_scale <- c(
  "Strongly Disagree" = GRAY2,
  "Disagree" = GRAY2,
  "Neutral" = GRAY9,
  "Agree" = BLUE5,
  "Strongly Agree" = BLUE5)


formatted_subtitle <- paste0(
  "<span style='color:", color_scale[1], "'>**", names(color_scale)[1], "**</span>",
  " | ",
  "<span style='color:", color_scale[2], "'>**", names(color_scale)[2], "**</span>",
  " | ",
  "<span style='color:", color_scale[3], "'>**", names(color_scale)[3], "**</span>",
  " | ",
  "<span style='color:", color_scale[4], "'>**", names(color_scale)[4], "**</span>",
  " | ",
  "<span style='color:", color_scale[5], "'>**", names(color_scale)[5], "**</span>"
)

pt <- df %>%
  ggplot(aes(y = fct_rev(Item), x = Value, fill = Answer)) +
  geom_bar(stat = "Identity", width = 0.65, color = "white") +
  scale_fill_manual(values = color_scale, guide = F) +
  labs(
    title = "Survey Results",
    subtitle = formatted_subtitle,
    x = "Percent of total"
  ) +
  scale_x_continuous(position = "top", 
                     breaks = seq(0,1,by = 0.2),
                     labels = scales::percent_format(accuracy = 1)) +
  coord_capped_cart(top = "both")

pt %>%
  save_and_show_plot(width = 6, height = 4, "FIG0219.png")