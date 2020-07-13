# TODO:
# - unclear if there's a better way to handle text alignment & position. If I use hjust = -X to move text left,
# the text after a line break is no longer left justified

library(tidyverse)
library(forcats)
library(lemon)
library(ggtext)
source("helper_functions.R")
source("theme/theme_swd.R")

theme_set(theme_swd() + theme(
  axis.title.y = element_blank(),
  plot.title = element_markdown(lineheight = 1.2),
  plot.subtitle = element_markdown(lineheight = 1.5),
  axis.text.y = element_text(color = GRAY4, size = 10),
  axis.title.x = element_text(color = GRAY5, size = 10, hjust = 0.05),  
  strip.placement = "outside",
  strip.background = element_rect(fill = NA, color = GRAY9)
))

df <- read_csv(file.path("data", "FIG0314.csv")) %>%
  mutate(value = parse_number(value) / 100) %>%
  mutate(category = fct_rev(fct_relevel(
    factor(category),
    "Demonstration of results",
    "Content expertise",
    "Local knowledge",
    "National reputation",
    "Affordability of services",
    "Previous work together",
    "Colleague recommendation"
  ))) %>%
  mutate(fill = case_when(
    category == "Demonstration of results" ~ GRAY2,
    category == "Affordability of services" ~ GRAY2,
    category == "Previous work together" ~ GRAY2,
    TRUE ~ GRAY8
  ))

pt <- ggplot(df, aes(x = category, y = value)) +
  geom_col(aes(fill = fill), width = .8) +
  scale_x_discrete() +
  scale_y_continuous(position = "right", limits = c(0, .8), labels = scales::percent) +
  scale_fill_identity() +
  coord_capped_flip(top = "both") +
  labs(
    caption = "Data source: xyz; indluces N number of surbey respondents.\nNote that respondents were able to choose up to 3 options.",
    y = "% selecting given attribute",
    title = "**Demonstrating effectiveness** is most important consideration <br/>when selecting a provider <br />",
    subtitle = "In general, **what attributes are the most important** <br/>to you in selecting a service provider ? <br/> (Choose up to 3)"
  )

pt %>% 
  save_and_show_plot(width = 6, height = 4, file_name = "FIG0314.png")
