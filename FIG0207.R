rm(list=ls())
library(tidyverse)
library(lemon)
source("helper_functions.R")
source("theme/theme_swd.R")

theme_set(theme_swd())

df <- read_csv(file.path("data", "FIG0206-7.csv")) %>% 
  mutate(`Cost Per Mile` = parse_number(`Cost Per Mile`),
         avg_miles = mean(`Cost Per Mile`, na.rm = TRUE),
         avg_cost = mean(`Cost Per Mile`, na.rm = TRUE),
         color = if_else(`Cost Per Mile` < avg_miles, GRAY9, ORANGE1))

avg_miles <- mean(df$`Miles Driven`)
avg_cost <- mean(df$`Cost Per Mile`)

pt <- ggplot(df) + 
  geom_point(aes(x=`Miles Driven`,y=`Cost Per Mile`, color = color), size = 3) +
  scale_color_identity() + 
  scale_y_continuous(breaks = seq(0,3,.5), limits = c(0,3),labels = scales::dollar_format()) + 
  scale_x_continuous(limits = c(0,4000), label = scales::comma) + 
  geom_hline(yintercept = avg_cost, linetype = "longdash") + 
  geom_point(x = avg_miles,y=avg_cost,size=3.5) + 
  geom_label(x = avg_miles, y = avg_cost, label = "AVG", hjust = 1.25, label.size = 0) +  
  labs(title = "Cost per mile by miles driven", x = "Miles driven per month", y = "Cost per mile") +
  coord_capped_cart(bottom = "right", left = "top")

pt %>% 
  save_and_show_plot(width = 4.5, height = 4.2, "FIG0207.png")

