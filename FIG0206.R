library(tidyverse)
source("include.R")
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.line = element_line(size = .1, color=GRAY9),
                                  axis.text = element_text(color = GRAY7, size = 10),
                                  axis.title = element_text(color = GRAY3, size = 10),
                                  axis.title.y = element_text(hjust = 1, margin = margin(0,6,0,15,"pt")),
                                  axis.title.x = element_text(hjust = 0, margin = margin(6,0,15,0,"pt")),
                                  plot.title = element_text(hjust = -.65)))

df <- read_csv("data\\FIG0206-7.csv") %>% 
  mutate(`Cost Per Mile` = as.numeric(str_remove_all(`Cost Per Mile`,"\\$"))) 

avg_miles <- mean(df$`Miles Driven`)
avg_cost <- mean(df$`Cost Per Mile`)

pt <- ggplot(df) + geom_point(aes(x=`Miles Driven`,y=`Cost Per Mile`), color = GRAY9, size = 2.5) +
  scale_y_continuous(breaks = seq(0,3,.5), limits = c(0,3),labels = scales::dollar_format(prefix="$")) + 
  scale_x_continuous(limits = c(0,4000), label = scales::comma) + 
  geom_point(x = avg_miles,y=avg_cost,size=3.5) + 
  geom_label(aes(x=avg_miles, y=avg_cost), label = "AVG", nudge_x = -300, nudge_y = .01, size = 3.5, label.size = 0, label.padding = unit(.17, "lines")) + 
  labs(title = "Cost per mile by miles driven", x = "Miles driven per month", y = "Cost per mile") 

ggsave("plot output\\FIG0206.png", pt, width = 4.5, height = 4.2)
pt
  