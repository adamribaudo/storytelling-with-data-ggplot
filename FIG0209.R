library(tidyverse)
library(zoo)

source("include.R")
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.line = element_line(size = .1, color=GRAY9),
                                  axis.text = element_text(color = GRAY7, size = 10),
                                  axis.title = element_text(color = GRAY3, size = 10),
                                  axis.title.y = element_text(hjust = 1, margin = margin(0,6,0,15,"pt")),
                                  axis.title.x = element_blank()))

df <- read_csv("data\\FIG0209.csv") %>% mutate(yearmon.zoo = as.yearmon(paste0(Year,Month), "%Y%b"))


ggplot(df) + 
  geom_ribbon(aes(x=yearmon.zoo,ymin = Min, ymax = Max), fill = GRAY9) + 
  geom_line(aes(x=yearmon.zoo, y=Avg),size=2) + 
  geom_point(aes(x=yearmon.zoo,y=Avg),size=4,data = df %>% slice(n=n())) + 
  geom_text(aes(x=yearmon.zoo, y=Min),label="Min",data=df %>% slice(n=1)) + 
  geom_text(aes(x=yearmon.zoo, y=Avg),label="Avg",data=df %>% slice(n=1)) + 
  geom_text(aes(x=yearmon.zoo, y=Max),label="Max",data=df %>% slice(n=1)) + 
  geom_text(aes(x=yearmon.zoo, y=Avg, label = Avg),data=df %>% slice(n=n())) + 
  scale_y_continuous(breaks = seq(0,40,5), limit = c(0,40)) + 
  scale_x_yearmon(format = "%b %Y", n = 12) + 
  labs(y = "Wait time (minutes)", title = "Passport control wait time", subtitle = "Past 13 months")
  
  
  
  geom_point(aes(x=`Miles Driven`,y=`Cost Per Mile`, color = color), size = 2.5) +
  scale_color_identity() + 
  scale_y_continuous(breaks = seq(0,3,.5), limits = c(0,3),labels = scales::dollar_format(prefix="$")) + 
  scale_x_continuous(limits = c(0,4000), label = scales::comma) + 
  geom_hline(yintercept = avg_cost, linetype = "longdash") + 
  geom_point(x = avg_miles,y=avg_cost,size=3.5) + 
  geom_label(aes(x=avg_miles, y=avg_cost), label = "AVG", nudge_x = -300, nudge_y = .01, size = 3.5, label.size = 0, label.padding = unit(.17, "lines")) + 
  labs(title = "Cost per mile by miles driven", x = "Miles driven per month", y = "Cost per mile") 

ggsave("plot output\\FIG0209.png", pt, width = 4.5, height = 4.2)
pt
  