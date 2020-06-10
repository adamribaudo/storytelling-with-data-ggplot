library(tidyverse)
library(lubridate)

source("include.R")
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.line = element_line(size = .1, color=GRAY9),
                                  axis.text = element_text(color = GRAY7, size = 10),
                                  axis.title = element_text(color = GRAY3, size = 10),
                                  axis.title.y = element_text(hjust = 1, margin = margin(0,6,0,15,"pt")),
                                  axis.title.x = element_blank()))

df <- read_csv("data\\FIG0209.csv") %>% mutate(date = ymd(paste(Year,Month,1)))


ggplot(df) + 
  geom_ribbon(aes(x=yearmon.zoo,ymin = Min, ymax = Max), fill = GRAY9) + 
  geom_line(aes(x=yearmon.zoo, y=Avg),size=2) + 
  geom_point(aes(x=yearmon.zoo,y=Avg),size=4,data = df %>% slice(n=n())) + 
  geom_text(aes(x=yearmon.zoo, y=Min),label="Min",data=df %>% slice(n=1)) + 
  geom_text(aes(x=yearmon.zoo, y=Avg),label="Avg",data=df %>% slice(n=1)) + 
  geom_text(aes(x=yearmon.zoo, y=Max),label="Max",data=df %>% slice(n=1)) + 
  geom_text(aes(x=yearmon.zoo, y=Avg, label = Avg),data=df %>% slice(n=n())) + 
  scale_y_continuous(breaks = seq(0,40,5), limit = c(0,40)) + 
  scale_x_date(date_labels=paste(c(rep(" ",11), "%b"), collapse=""), 
               date_breaks="month", expand=c(0,0)) + 
  labs(y = "Wait time (minutes)", title = "Passport control wait time", subtitle = "Past 13 months")

ggsave("plot output\\FIG0209.png", pt, width = 4.5, height = 4.2)
pt
  