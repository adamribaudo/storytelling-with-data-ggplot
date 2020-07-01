library(tidyverse)
library(grid)
library(gridExtra)
library(gridtext)
source("include.R")

theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.line = element_line(color = GRAY9),
                                  axis.title.y = element_text(color = GRAY5, hjust = 1, size = 14),
                                  axis.title.x = element_text(color = GRAY5, hjust = 0),
                                  axis.ticks = element_line(color = GRAY9),
                                  axis.text = element_text(color = GRAY6, size = 12),
                                  plot.margin = unit(c(1,1,1,1), "cm"),
                                  plot.title = element_text(size = 18, color = GRAY2, margin = margin(0,0,.5,0,"cm")),
                                  plot.caption = element_text(color = GRAY8, hjust = 0, size = 10, margin = margin(.5,0,0,0,"cm"))
                                  ))

df <- read_csv(file.path("data","FIG0602.csv")) %>% mutate(sales = as.numeric(str_remove(sales,"\\$")))

# ACTUAL/FORECAST Annotations
grob_actual <- grobTree(richtext_grob("ACTUAL", x=.3,  y=-.11, hjust=0, gp=gpar(col = GRAY6, fontsize=13)))
grob_forecast <- grobTree(richtext_grob("FORECAST", x=.78,  y=-.11, hjust=0, gp=gpar(col = GRAY6, fontsize=13)))

# Annual growth percentage annotations
grob_2006 <- grobTree(richtext_grob(
  sprintf("<b style='color:%s'>2006-09</b> :<br>annual sale<br>growth of<br>7-8%%",BLUE2),
  x=.02,y=.99, hjust=0, gp=gpar(col = GRAY6, fontsize=13), vjust = 1))

grob_2010 <- grobTree(richtext_grob(
  sprintf("<b style='color:%s'>2010</b> :<br>more marked<br>increase<br>of 22%%<br>sales year<br>over year,<br>driven by",BLUE2),
  x=.18,y=.99, hjust=0, gp=gpar(col = GRAY6, fontsize=13), vjust = 1))

grob_2011 <- grobTree(richtext_grob(
  sprintf("<b style='color:%s'>2011-14</b> :<br>another period<br>of steady<br>frowth of<br>8-9%% annually",BLUE2),
  x=.36,y=.99, hjust=0, gp=gpar(col = GRAY6, fontsize=13), vjust = 1))

grob_2015 <- grobTree(richtext_grob(
  sprintf("<b style='color:%s'>2015 & beyond</b> : assumed<br>10%% year over year<br>increase in sales*",BLUE2),
  x=.56,y=.99, hjust=0, gp=gpar(col = GRAY6, fontsize=13), vjust = 1))

 pt <- ggplot(df, aes(x = year, y = sales)) + 
   # Forecast
   geom_line(data = df %>% filter(category == "FORECAST" | (category == "ACTUAL" & year == 2014)), size = 1, color = BLUE2, linetype = 8 ) +
   geom_point(data = df %>% filter(category == "FORECAST"), size = 3, color = BLUE2, shape = 16) +
   # Actual
   geom_line(data = df %>% filter(category == "ACTUAL"), color = BLUE2, size = 2) + 
   geom_point(data = df %>% filter(category == "ACTUAL"), size = 3, fill = "white", color = BLUE2, stroke = 2, shape = 21) +
   # Text
   geom_text(data = df %>% filter(category == "FORECAST" | (category == "ACTUAL" & year == 2014)),
             aes(label = scales::number_format(prefix = "$")(sales)), color = BLUE2, nudge_y = 9) +
   scale_x_continuous(breaks = seq(2006,2018,1)) + 
   scale_y_continuous(breaks = seq(0,180,20), labels = scales::number_format(prefix = "$"), limits = c(0,180), expand = c(0,0)) + 
   labs(y = "Sales ($Billion)", x = "",
        title = "Sales over time",
        caption = "Data source: Sales Dashboard; annual figures are as of 12/31 of hte given year.\n*Use this footnote to explain what is driving the 10% annual growth forecast assumption") + 
   coord_cartesian(clip = "off") + 
   geom_rect(xmin = 2014.5, xmax = 2018.5, ymin = -28, ymax = 0, alpha = 0.01) + 
   annotation_custom(grob_actual) + 
   annotation_custom(grob_forecast) + 
   annotation_custom(grob_2006) + 
   annotation_custom(grob_2010) + 
   annotation_custom(grob_2011) + 
   annotation_custom(grob_2015)
 
   
  
  
height <- 5.5
width <- 8
dev.new(width = width, height = height, units = "in", noRStudioGD = T)
pt
ggsave(file.path("plot output", "FIG0602.png"), pt, width = width, height = height)
