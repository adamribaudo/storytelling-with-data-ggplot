rm(list = ls())
library(tidyverse)
library(grid)
library(gridExtra)
library(gridtext)
library(lemon)
library(stringr)
library(ggtext)
source("theme/theme_swd.R")

theme_set(theme_swd() + theme(
  axis.title.x = element_text(color = GRAY5),
  axis.text.x = element_text(color = GRAY5),
  axis.line.x = element_line(color = GRAY5),
  axis.ticks.x = element_line(color = GRAY5),
  axis.title.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.y = element_text(color = GRAY1, size = 13),
  axis.line.y = element_blank(),
  plot.title = element_text(margin = margin(b = 1.2,unit="cm")),
  plot.margin = margin(.5,4.3,.5,.5, unit = "cm")
))

df <- read_csv(file.path("data", "FIG0920.csv")) %>%
  pivot_longer(cols = -feature, names_to = "results", values_to = "value")
                
  

grob_legend = grobTree(grobTree(richtext_grob(sprintf("<span style='color:%s'><b>Strongly Diagree</b></span> | Disagree | Neutral | <span style='color:%s'><b>Strongly Agree</b></span>",RED1,GREEN4), 
                                              x=0,  y=1.25, hjust=0, gp=gpar(col = GRAY6, fontsize=11))))

df %>% ggplot(aes(x = item, y = value, fill = box)) +
  geom_col() 
  
 
width <- 6.5
height <- 4
#dev.new(width = width, height = height, noRStudioGD = T)
#pt
#ggsave(file.path("plot output", "FIG0902.png"), pt, width = width, height = height)
