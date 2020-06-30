# TODO: find the geom_text segment labels as a grob and move them to the left
# TODO: Add a vertical line to the right of each bar

library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(gridtext)
source("include.R")

theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.line.y = element_blank(),
                                  axis.line.x = element_line(color = GRAY9),
                                  axis.title = element_blank(),
                                  axis.ticks = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.text.x = element_text(size = 12),
                                  plot.margin = unit(c(1,1,1,4), "cm"),
                                  plot.title = element_text(size = 18, color = GRAY2, margin = margin(0,0,.5,0,"cm")),
                                  plot.caption = element_text(color = GRAY8, hjust = 0, margin = margin(.3,0,0,0,"cm"))
                                  ))

df <- read_csv(file.path("data","FIG0512-13.csv")) %>% drop_na() %>% 
  pivot_longer(cols = -Segment, names_to = "Population", values_to = "value") %>% 
  mutate(Segment = forcats::fct_rev(factor(Segment))) %>%
  mutate(Population = forcats::fct_rev(factor(Population))) 

grob_30 <- grobTree(richtext_grob("<b>30%</b>", x=.43,  y=.38, hjust=0, gp=gpar(col = BLUE2, fontsize=18)))
grob_50 <- grobTree(richtext_grob("<b>50%</b>", x=.88,  y=.42, hjust=0, gp=gpar(col = BLUE2, fontsize=18)))

pt <- ggplot(df, aes(x = Population, y = value, fill = Segment)) + 
  geom_col(position = "stack", color = "white", width = .6) + 
  scale_fill_manual(guide = F, values = c(GRAY9,GRAY9,BLUE2,BLUE2,BLUE2,GRAY9,GRAY9)) + 
  geom_text(position = ggplot2::position_stack(vjust = .5), 
            aes(label = scales::percent(accuracy = 1, x=value), color = Segment), size = 4.5) + 
  scale_color_manual(guide = F, values = c(GRAY2,GRAY2,GRAY9,GRAY9,GRAY9,GRAY2,GRAY2)) + 
  
 # geom_text(data = df %>% filter(Population == "US Population"), 
 #         position = ggplot2::position_stack(vjust = .5), aes(label = Segment)
 #          ) + 
  scale_y_continuous(expand = c(0,0)) + 
  coord_cartesian(clip = "off") + 
  labs(title = "Distribution by customer segment") + 
  annotation_custom(grob_30) + 
  annotation_custom(grob_50)
  
 
height <- 5.5
width <- 8
dev.new(width = width, height = height, units = "in", noRStudioGD = T)
pt
ggsave(file.path("plot output", "FIG0513.png"), pt, width = width, height = height)
