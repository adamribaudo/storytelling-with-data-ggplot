library(tidyverse)
library(imager)
source("include.R")
theme_pt1 <- theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.ticks.x = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.line.y = element_line(),
                                  axis.line.x = element_blank(),
                                  axis.text.x = element_blank(),
                                  plot.margin = unit(c(0,0,0,0),"cm"))

theme_pt2 <- theme_minimal() + theme(panel.grid.major = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     axis.title.x = element_blank(),
                                     axis.line = element_line(color = GRAY8),
                                     axis.ticks = element_line(color = GRAY8),
                                     panel.spacing.x = unit(-.15,"cm"),
                                     strip.placement = "outside",
                                     strip.switch.pad.grid = unit(0,"cm"),
                                     plot.margin = unit(c(1,0,.5,0),"cm"))

df <- read_csv("data\\FIG0226-27.csv") %>% mutate(revenue_num = as.numeric(str_remove_all(Revenue,"\\$|,"))) %>%
  mutate(year_qrt = paste(Year, Quarter))

pt1 <- ggplot(df,aes(x = year_qrt, y = `Size of Salesforce`, group = 1)) + geom_point(color = BLUE1, size = 2) + geom_line(color = BLUE1, size=1.2) +
  scale_y_continuous(limits = c(60, 120)) + 
  labs(title = "Alternative 2: pull apart vertically", y = "# of Sales\nEmplyees") + theme_pt1

pt2 <- ggplot(df, aes(x = Quarter, y = revenue_num)) + geom_col(fill = BLUE2, width = .7 ) + 
  scale_x_discrete(labels = df$Quarter,expand = c(.2,0)) + 
  scale_y_continuous(labels = scales::dollar_format(accuracy = .1, scale = .000001, prefix="$", suffix = "M"), expand = c(0,0),
                     limits = c(0, 1000000)) + 
  facet_grid(cols = vars(Year), switch = "x") +
  labs(y = "Revenue (Millions)") + 
  theme_pt2

# Save 2 diferent plots and stitch them together with imager
ggsave(file.path("plot output","FIG0227_pt1.png"), pt1, width = 4, height = 1)
ggsave(file.path("plot output","FIG0227_pt2.png"), pt2, width = 4, height = 3)

pt1_img <- load.image(file.path("plot output","FIG0227_pt1.png"))
pt2_img <- load.image(file.path("plot output","FIG0227_pt2.png"))

new_img <- imappend(list(pt1_img,pt2_img),"y")
save.image(new_img,file.path("plot output","FIG0227.png"))
