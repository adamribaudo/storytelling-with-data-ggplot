# TODO unclear how to move axis.text.x up towards the x axis

rm(list = ls())
library(tidyverse)
library(lubridate)
library(lemon)
library(grid)
library(gridtext)
library(ggtext)
source("theme/theme_swd.R")

theme_set(theme_swd() + theme(axis.line.x = element_blank(),
                              axis.text.x = element_text(vjust = 1, color = GRAY2),
                              axis.title.x = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.title.y = element_text(hjust = .87),
                              plot.margin = margin(.5,4.5,.5,.5,"cm")))

df <- read_csv(file.path("data", "FIG0604.csv")) %>% 
  pivot_longer(cols = -Category, names_to = "period", values_to = "value") %>%
  # Add in new line
  mutate(period = case_when(period == "Today 9/30/15" ~ "Today\n9/30/15",
                            T ~ period)) %>%
  mutate(period = forcats::fct_relevel(factor(period), "Today\n9/30/15","FY16","FY17","FY18","FY19","FY20")) %>%
  mutate(Category = forcats::fct_relevel(factor(Category), "Unmet need","Directors from acquisitions","Promotions to director","Today's directors","Attrition")) %>%
  mutate(value_label = case_when(Category == "Unmet need" ~ paste("bold('",value,"')"),
                                 T ~ ""))

# Category annotations
grob_unmet_need <- grobTree(richtext_grob("<b>Unmet need (gap)", x=1,  y=.9, hjust=0, gp=gpar(col = "black", fontsize=13)))
grob_acquisitions <- grobTree(richtext_grob("Directors from<br>acquisitions", x=1,  y=.66, hjust=0, gp=gpar(col = GREEN2, fontsize=13)))
grob_promotions <- grobTree(richtext_grob("Promotions to director", x=1,  y=.565, hjust=0, gp=gpar(col = GREEN1, fontsize=13)))
grob_today <- grobTree(richtext_grob("Today's directors", x=1,  y=.5, hjust=0, gp=gpar(col = BLUE2, fontsize=13)))
grob_attrition <- grobTree(richtext_grob("Attrition", x=1,  y=.05, hjust=0, gp=gpar(col = BLUE3, fontsize=13)))


pt <- ggplot(df, aes(x = period, y = value, fill = Category)) + 
  geom_col(color = GRAY3, width = .75) + 
  # Attempted to use ggtext::geom_richtext but it didn't seem to apply position_stack() correctly after removing the label outline with fill=NA,label.color=NA
  geom_text(data = df %>% filter(period != "Today\n9/30/15"), aes(label = value_label),
            position = ggplot2::position_stack(vjust = .8), size = 4.5, parse = T) +
  geom_hline(yintercept = 0, color = GRAY9) +
  scale_fill_manual(values = c("white", GREEN2,GREEN1, BLUE2,BLUE3 ),
                    guide = F) +
  scale_y_continuous(breaks = seq(-100,250,50)) +
    labs(title = "Expected director population over time",
       y = "# of directors",
       caption = "A footnote explaiing relevant forecast assumptions and methodology would go here") +
  # Can't use coord_capped_... until it supports clip = "off"
  #coord_capped_cart(left = "both") + 
  coord_cartesian(clip = "off") + 
  annotation_custom(grob_unmet_need) + 
  annotation_custom(grob_acquisitions) + 
  annotation_custom(grob_promotions) + 
  annotation_custom(grob_today) + 
  annotation_custom(grob_attrition)


width <- 6.5
height <- 4.2
dev.new(width = width, height = height, noRStudioGD = T)
pt
ggsave(file.path("plot output", "FIG0604.png"), pt, width = width, height = height)
