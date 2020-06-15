library(tidyverse)
source("include.R")
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.ticks = element_blank(),
                                  axis.title = element_blank(),
                                  axis.line = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.text.x = element_blank(),
                                  plot.margin = unit(c(0,0,0,0),"cm"),
                                  ))

df <- read_csv("data\\FIG0202-3.csv") %>% mutate(Value_pct = scales::percent(Value/100))

pt <- ggplot(df) + annotate("text", hjust = 0, x=0, y=.05,label = paste0("bold('",df %>% filter(Year == 2012) %>% select(Value_pct) %>% pull(),"')"), size = 20, parse=T, color = GREEN3) +
  annotate("text", hjust = 0, x = 0, y = -.2, label = "of children had a", color = GRAY3) + 
  annotate("text", hjust = 0, x = 0, y = -.3, label = "bold('traditional stay-at-home-mom')", parse=T, color = GREEN3) + 
  annotate("text", hjust = 0, x = 0, y = -.4, label = paste0("in 2020, compared to ",df %>% filter(Year == 1970) %>% select(Value_pct) %>% pull()," in 1970"), color = GRAY3) + 
  coord_cartesian(clip = "off") + ylim(c(-.6,.6)) + xlim(c(-.1,1.75))

ggsave("plot output\\FIG0203.png", pt, width = 3, height = 2.5)
pt
