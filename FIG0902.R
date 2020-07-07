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

df <- read_csv(file.path("data", "FIG0902.csv")) %>% 
                 pivot_longer(cols = -item, names_to = "box", values_to = "value") %>%
                 mutate(value = as.numeric(str_remove(value, "%"))/100) %>%
  mutate(item = forcats::fct_rev(factor(item))) %>%
  mutate(box = forcats::fct_rev(factor(box))) %>%
  mutate(value_label = case_when(box == "Middle" ~ "",
                                 box == "Bottom box" & item == "Survey item A" ~ "",
                                 T ~ as.character(scales::percent(x=value)))) %>%
  mutate(value_label_a = case_when(box == "Bottom box" & item == "Survey item A" ~ as.character(scales::percent(x=value)),
                                   T ~ ""))
                
  

grob_legend = grobTree(grobTree(richtext_grob(sprintf("<span style='color:%s'><b>Strongly Diagree</b></span> | Disagree | Neutral | <span style='color:%s'><b>Strongly Agree</b></span>",RED1,GREEN4), 
                                              x=0,  y=1.25, hjust=0, gp=gpar(col = GRAY6, fontsize=11))))
grob_item_a = grobTree(grobTree(richtext_grob(sprintf("<span style='color:%s'><b>Survey tem A</b></span><br>ranked highest<br>for team X",GREEN4), 
                                              x=1.03,  y=.85, hjust=0, gp=gpar(col = GRAY6, fontsize=13))))
grob_item_d = grobTree(grobTree(richtext_grob(sprintf("Dissatisfaction<br>was greatest<br>for <span style='color:%s'><b>Survey item D</b></span>",RED1), 
                                              x=1.03,  y=.15, hjust=0, gp=gpar(col = GRAY6, fontsize=13))))

pt <- df %>% ggplot(aes(x = item, y = value, fill = box)) +
  geom_col() + 
  scale_y_continuous(position = "right",
                     labels = scales::percent,
                     breaks = seq(0,1,.2),
                     expand = c(0,0)) + 
  scale_fill_manual(guide = F, 
                    values = c(GREEN4,GRAY9,RED1)) +
  geom_text(aes(label = value_label), 
            position = position_stack(vjust = .6),
            color = "white", size = 4.5, hjust = .5) + 
  geom_text(aes(label = value_label_a), 
            position = position_stack(),
            color = RED1, size = 4.5, hjust = -.2) + 
  coord_flip(clip="off") + 
  labs(title = "Survey Results: Team X",
       y = "Percent of Total") + 
  annotation_custom(grob_legend) + 
  annotation_custom(grob_item_a) + 
  annotation_custom(grob_item_d)
 
width <- 6.5
height <- 4
#dev.new(width = width, height = height, noRStudioGD = T)
pt
ggsave(file.path("plot output", "FIG0902.png"), pt, width = width, height = height)
