library(tidyverse)
library(lemon)

source("include.R")
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.ticks.x = element_line(color=GRAY9),
                                  axis.line.x = element_line(size = .1, color=GRAY9),
                                  axis.line.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.text.x = element_text(color = GRAY6, size = 10),
                                  axis.title.x = element_text(color = GRAY5, size = 10, hjust = .425),
                                  axis.title.y = element_blank(),
                                  strip.placement = "outside",
                                  strip.background = element_rect(fill=NA,color=GRAY9),
                                  plot.subtitle = element_text(color = GRAY4, size = 9, hjust=.22),
                                  plot.margin = unit(c(1,0,1,1),"cm")))

df <- read_csv("data\\FIG0210-11.csv") %>% rename(category=`X1`) %>%
  pivot_longer(cols = c(`2014`,`2015`), names_to = "year", values_to = "result") %>%
  mutate(result = as.numeric(str_remove(result, "\\%"))/100)

pt <- ggplot(df) + geom_point(aes(x = year, y = result),size=2.5, color = GRAY5) + 
  geom_line(aes(x=year, y=result, group=category),size = 1, color = GRAY5) +
  scale_x_discrete(expand = expansion(1.9,0), limits = c("2014","2015"), labels = c(2014,2015)) +
  geom_text(aes(x=year, y=result, label = result), nudge_x = -.2, data = df %>% filter(year == 2014), color = GRAY5) +
  geom_text(aes(x=year, y=result, label = result), nudge_x = .2, data = df %>% filter(year == 2015), color = GRAY5) +
  geom_text(aes(x=year,y=result, label = category),nudge_x = -.4, data = df %>% filter(year == "2014"), hjust = 1, color = GRAY5) +
  labs(title = "Employee feedback over time", x = "Survey Year", subtitle = "Survey Category | Percent Favorable") +
  coord_cartesian(clip = "off") +
  lemon::coord_capped_cart(bottom='both') + ylim(.2,.96)

ggsave("plot output\\FIG0210.png", pt, width = 6, height = 6)

pt
