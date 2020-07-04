# TODO: How to drop the Q4 level from 2015 only? if I set scales = "free_x" it resizes the column widths of 2015

rm(list = ls())
library(tidyverse)
library(lemon)
library(grid)
source("theme/theme_swd.R")

theme_set(theme_swd() + theme(
  axis.line.y = element_line(),
  axis.title.y = element_text(color = GRAY6),
  axis.text.x = element_text(size = 13, color = GRAY3),
  legend.position = "top",
  legend.title = element_blank(),
  legend.justification = "left",
  panel.spacing = unit(-.5, "lines"),
  # TODO, figure out how to reduce padding below the plot
  #strip.switch.pad.grid = unit(0, "lines"),
  strip.placement = "outside",
  strip.text = element_text(color = GRAY3, size = 13),

))

df <- read_csv(file.path("data", "FIG0603.csv")) %>% pivot_longer(cols = c(-Year,-Quarter), names_to = "goal", values_to = "value") %>%
  mutate(value = as.numeric(str_remove(value, "\\%"))/100)

pt <- ggplot(df, aes(x = Quarter,y = value, fill = goal)) + 
  geom_col(position = "stack", width = .7, color = "white") + 
  geom_text(data = df %>% filter(goal == "Miss", (Year == "2014" & (Quarter == "Q3" | Quarter == "Q4")) | 
                                   (Year == "2015")),
    aes(label = scales::percent(value, accuracy = 1)), position = ggplot2::position_stack(vjust = .7),
    color = "white",size = 5) + 
  scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1), label = scales::percent(x = seq(0,1,.1), accuracy = 1), expand = c(0,0)) + 
  scale_fill_manual(guide = guide_legend(), values = c(GRAY4, GRAY9, RED3), breaks = c("Exceed","Meet")) + 
  facet_grid(~Year, switch = "both", drop = T) + 
  coord_capped_cart(left = "top") + 
  labs(title = "Goal attainment over time",
       y = "% of total projects",
       x = "",
       caption = "Data source: XYZ Dashboard")

  
  
width <- 9
height <- 5.5
dev.new(width = width, height = height, noRStudioGD = T)
pt
ggsave(file.path("plot output", "FIG0603.png"), pt, width = width, height = height)
