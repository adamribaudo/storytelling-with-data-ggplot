library(tidyverse)
library(lubridate)
library(grid)

source("include.R")
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.ticks.y = element_line(color=GRAY9),
                                  # Missing from this re-creation are the long x-axis ticks. Unclear on how to increase tick length and center the labels between ticks.
                                  axis.ticks.x = element_line(color=GRAY9),
                                  axis.line = element_line(size = .1, color=GRAY9),
                                  axis.text = element_text(color = GRAY6, size = 10),
                                  axis.title = element_text(color = GRAY5, size = 10),
                                  axis.title.y = element_text(size = 14, hjust = 1, margin = margin(0,6,0,15,"pt")),
                                  axis.title.x = element_blank(),
                                  strip.placement = "outside",
                                  strip.background = element_rect(fill=NA,color=GRAY9),
                                  plot.subtitle = element_text(color = GRAY4),
                                  plot.margin = unit(c(1,1,1,1),"cm")))

df <- read_csv(file.path("data","FIG0209.csv")) %>% mutate(date = ymd(paste(Year,Month,1)))


pt <- ggplot(df) + 
  geom_ribbon(aes(x=date,ymin = Min, ymax = Max), fill = GRAY9) + 
  geom_line(aes(x=date, y=Avg),size=2) + 
  geom_point(aes(x=date,y=Avg),size=4,data = df %>% slice(n=n())) + 
  geom_text(aes(x=date, y=Min),nudge_x = 20, nudge_y = 1.5, color = GRAY5, parse = T, label=sprintf("bold(MIN)"),data=df %>% slice(n=1)) + 
  geom_text(aes(x=date, y=Avg),nudge_x = 20, nudge_y = 1.5, parse = T, label="bold(AVG)",data=df %>% slice(n=1)) + 
  geom_text(aes(x=date, y=Max),nudge_x = 20, nudge_y = 0, color = GRAY5, parse = T, label="bold(MAX)",data=df %>% slice(n=1)) + 
  geom_text(aes(x=date, y=Avg, label = paste0("bold(",Avg,")")), parse=T,nudge_x = 15, data=df %>% slice(n=n())) + 
  scale_y_continuous(breaks = seq(0,40,5), limit = c(0,40)) + 
  scale_x_date(date_labels="%b", breaks=df$date, expand=c(0,0), 
               limits=c(ymd("2014-08-16",ymd("2015-10-01")))) +
  labs(y = "Wait time (minutes)", title = "Passport control wait time", subtitle = "Past 13 months") 

dev.new(width = 6.5, height = 4.2, unit = "in", noRStudioGD =T)
pt

ggsave(file.path("plot output","FIG0209.png"), pt, width = 6.5, height = 4.2)
