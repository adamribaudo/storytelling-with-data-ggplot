rm(list = ls())
library(tidyverse)
library(lubridate)
source("helper_functions.R")
source("theme/theme_swd.R")

theme_set(theme_swd())

df <- read_csv(file.path("data", "FIG0209.csv")) %>% mutate(date = ymd(paste(Year, Month, 1)))

# Missing from this re-creation are the long x-axis ticks. Unclear on how to increase tick length and center the labels between ticks.
pt <- ggplot(df, aes(x = date)) +
  geom_ribbon(aes(ymin = Min, ymax = Max), fill = GRAY9) +
  geom_line(aes(y = Avg), size = 2, color = GRAY4) +
  geom_point(aes(y = Avg), size = 1, data = df %>% slice(n = 1), color = GRAY4) +
  geom_point(aes(y = Avg), size = 4, data = df %>% slice(n = n()), color = GRAY4) +
  geom_text(aes(y = Min), hjust = -0.1, vjust = -1.1, color = GRAY5, parse = T, label = sprintf("bold(MIN)"), data = df %>% slice(n = 1)) +
  geom_text(aes(y = Avg), hjust = -0.1, vjust = -0.5, parse = T, label = "bold(AVG)", data = df %>% slice(n = 1)) +
  geom_text(aes(y = Max), hjust = -0.1, vjust = 2, color = GRAY5, parse = T, label = "bold(MAX)", data = df %>% slice(n = 1)) +
  geom_text(aes(y = Avg, label = paste0("bold(", Avg, ")")), parse = T, nudge_x = 15, data = df %>% slice(n = n())) +
  scale_y_continuous(breaks = seq(0, 40, 5), limit = c(0, 40)) +
  scale_x_date(
    date_labels = "%b", breaks = df$date, expand = c(0, 0),
    limits = c(ymd("2014-08-16", ymd("2015-10-01")))
  ) +
  labs(y = "Wait time (minutes)", x = NULL, title = "Passport control wait time", subtitle = "Past 13 months")

pt %>% 
  save_and_show_plot(width = 6, height = 4, "FIG0209.png")

