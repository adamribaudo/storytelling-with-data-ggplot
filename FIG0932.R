# TODO: would love to cap the x axis using the lemon package, but it doesn't support clip="off"

rm(list = ls())
library(tidyverse)
library(lemon)
library(grid)
library(gridExtra)
library(gridtext)
library(ggtext)
source("helper_functions.R")
source("theme/theme_swd.R")
theme_set(theme_swd() + theme(    axis.ticks.y = element_blank(),
                                  axis.ticks.x = element_line(color=GRAY6),
                                  axis.line.x = element_line(size = .1, color=GRAY6),
                                  axis.line.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.text.x = element_text(color = GRAY6, size = 12),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank(),
                                  plot.title = element_text(size = 16),
                                  plot.caption = element_text(size = 8,margin = margin(t = .8,unit = "cm")),
                                  #plot.caption.position = "panel",
                                  #plot.title.position =  "panel",
                                  plot.margin = unit(c(.5,1,.5,1),"cm")))

grob_before = grobTree(richtext_grob(sprintf("BEFORE program,<br>the <span style='color:%s'>majority of<br>children felt just OK</span><br>about science", ORANGE1), 
                                   x=.76,  y=1, vjust = 1, hjust=0, gp=gpar(col = GRAY6, fontsize=13)))

grob_after = grobTree(richtext_grob(sprintf("AFTER program,<br><span style='color:%s'>more children were<br>Kind if interested &<br>excited </span><br>about<br>science", BLUE2), 
                                     x=.76,  y=.7, vjust = 1, hjust=0, gp=gpar(col = GRAY6, fontsize=13)))

df <- read_csv(file.path("data","FIG0932.csv")) %>% rename(category=`X1`) %>%
  pivot_longer(cols = -category, names_to = "period", values_to = "value") %>%
  mutate(value_label = value) %>%
  mutate(value = as.numeric(str_remove(value, "\\%"))/100) %>%
  mutate(color = case_when(category == "OK" ~ ORANGE1, 
                           category == "Excited" | category == "Kind of interested" ~ BLUE2,
                           T ~ GRAY9)) %>% 
  # Add line break for 'kind of interested'
  mutate(category = if_else(category == "Kind of interested", "Kind of\ninterested",category)) %>%
  mutate(category = forcats::fct_relevel(factor(category), "Bored","Not great","OK","Kind of interested","Excited")) %>%
  mutate(period = forcats::fct_rev(factor(period))) 
 

pt <- ggplot(df, aes(x = period, y = value, group = category, color = color)) + 
  geom_point(size=3) + 
  geom_line(size = 2) +
  scale_color_identity() +
  scale_x_discrete(limits = c("BEFORE","AFTER"), expand = c(.2,.05,.8,.5)) +
  geom_text(aes(label = value_label), nudge_x = -.1, data = df %>% filter(period == "BEFORE")) +
  geom_text(aes(label = value_label), nudge_x = .14, data = df %>% filter(period == "AFTER")) +
  geom_text(aes(label = category), nudge_x = .24, hjust = 0, data = df %>% filter(period == "AFTER")) +
  labs(title = "How do you feel about science?", 
       caption = "Based on survey of 100 students conducted before and after pilot program (100% response rate on both surveys") +
  coord_cartesian(clip = "off") + 
  #lemon::coord_capped_cart(bottom='both') + 
  ylim(0,.4) + 
  annotation_custom(grob_before) + 
  annotation_custom(grob_after)

pt %>% save_and_show_plot(width = 8, height = 6, "FIG0932.png")
