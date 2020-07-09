rm(list = ls())
library(tidyverse)
library(lemon)
source("helper_functions.R")
source("theme/theme_swd.R")
theme_set(theme_swd() + theme(    axis.ticks.y = element_blank(),
                                  axis.ticks.x = element_line(color=GRAY9),
                                  axis.line.x = element_line(size = .1, color=GRAY9),
                                  axis.line.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.text.x = element_text(color = GRAY6),
                                  axis.title.x = element_text(color = GRAY5, hjust = .425),
                                  axis.title.y = element_blank(),
                                  strip.placement = "outside",
                                  strip.background = element_rect(fill=NA,color=GRAY9),
                                  plot.subtitle = element_text(color = GRAY4, size = 8, hjust=.22),
                                  plot.margin = unit(c(1,0,1,1),"cm")))

df <- read_csv(file.path("data","FIG0210-11.csv")) %>% rename(category=`X1`) %>%
  pivot_longer(cols = c(`2014`,`2015`), names_to = "year", values_to = "result") %>%
  mutate(result_pct = result) %>%
  mutate(result = as.numeric(str_remove(result, "\\%"))/100) %>%
  # Relevel the categories to ensure that "Career development" is drawn last and displays on top of the other categories
  mutate(category = forcats::fct_relevel(factor(category), "Peers", "Culture", "Work environment", "Leadership", "Rewards & recognition", "Perf management", "Career development"))

pt <- ggplot(df) + geom_point(aes(x = year, y = result),color = GRAY5,size=2.5) + 
  geom_line(aes(x=year, y=result, group=category), color = GRAY5, size = 1) +
  scale_color_identity() +
  scale_x_discrete(expand = expansion(1.9,0), limits = c("2014","2015"), labels = c(2014,2015)) +
  geom_text(aes(x=year, y=result, label = result_pct), nudge_x = -.2, color = GRAY5, data = df %>% filter(year == 2014)) +
  geom_text(aes(x=year, y=result, label = result_pct), nudge_x = .2, color = GRAY5, data = df %>% filter(year == 2015)) +
  geom_text(aes(x=year,y=result, label = category),nudge_x = -.4, color = GRAY5, data = df %>% filter(year == 2014) , hjust = 1) +
  
  labs(title = "Employee feedback over time", x = "Survey Year", subtitle = "Survey Category | Percent Favorable") +
  #coord_cartesian(clip = "off") +
  lemon::coord_capped_cart(bottom='both') + ylim(.2,.96)

pt %>% 
  save_and_show_plot(width = 6, height = 6, "FIG0211.png")