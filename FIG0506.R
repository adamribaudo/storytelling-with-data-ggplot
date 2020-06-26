library(tidyverse)
library(grid)
library(gridExtra)
library(ggrepel)
library(gridtext)

source("include.R")
set.seed(1)
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.title.x = element_text(hjust = 0, color = GRAY8, size = 8),
                                  axis.title.y = element_text(hjust = 1, color = GRAY8, size = 8),
                                  axis.line = element_line(color = GRAY8),
                                  axis.text = element_text(color = GRAY8),
                                  axis.ticks = element_line(color = GRAY8),
                                  plot.margin = unit(c(3,1,1,4), "cm")
                                  ))

df <- read_csv(file.path("data","FIG0506.csv")) %>% mutate(Satisfaction = as.numeric(str_remove(Satisfaction, "%"))/100) %>%
  rename(issues = `Issues per 1,000`) %>%
  mutate(issues_formatted = scales::comma(issues)) %>%
  mutate(Satisfaction_formatted = scales::percent(Satisfaction, accuracy = 1)) %>%
  mutate(color = case_when(Model == "Prior Year Avg" ~ "black",
                           Satisfaction > .7 & issues > 900 ~ RED1,
                           Satisfaction > .7 & issues <= 900 ~ GRAY9))

grob_title <- grobTree(richtext_grob("<b>Issues</b> vs. <b>Satisfaction</b> by Model", x=-.3,  y=1.3, hjust=0,gp=gpar( fontsize=15)))
grob_satisfaction <- grobTree(textGrob("Satisfaction", x=0,  y=1.2, hjust=0,gp=gpar( fontsize=12)))
grob_low <- grobTree(richtext_grob("<b>LOW</b>", x=0,  y=1.12, hjust=0,gp=gpar(col = GRAY8, fontsize=12)))
grob_high <- grobTree(richtext_grob("<b>HIGH</b>", x=1,  y=1.1, hjust=1,gp=gpar(col = GRAY8, fontsize=12)))
grob_high_sat_low_issue <- grobTree(richtext_grob("<b>High Satisfaction,<br>Few Issues</b>", x=1,  y=.9, hjust=1,gp=gpar(col = GRAY8, fontsize=12)))
grob_high_sat_many_issue <- grobTree(
  richtext_grob(sprintf("<span style='color:%s'><b>High Satisfaction</b></span>,<br><span style='color:%s'><b>Many Issues</b></span>", GRAY8,RED1), 
                x=1,  y=0, hjust=1,gp=gpar(fontsize=12)))
grob_few <- grobTree(richtext_grob("<b>FEW</b>", x=-.15,  y=1, hjust=1,gp=gpar(col = GRAY8, fontsize=12)))
grob_things_wrong <- grobTree(textGrob("Things Gone\nWrong", x=-.15,  y=1.15, hjust=1,gp=gpar(fontsize=12)))
grob_many <- grobTree(richtext_grob("<b>MANY</b>", x=-.15,  y=0, hjust=1,gp=gpar(col = GRAY8, fontsize=12)))

pt <- ggplot(df, aes(x = Satisfaction, y = issues, color = color )) + 
  geom_point(size = 3.5) + 
  geom_vline(data = df %>% filter(Model == "Prior Year Avg"), aes(xintercept = Satisfaction)) + 
  geom_hline(data = df %>% filter(Model == "Prior Year Avg"), aes(yintercept = issues)) + 
  geom_text_repel(data = df %>% filter(Model != "Prior Year Avg"),
                  aes(label = Model), point.padding = .3, 
                  force = 1,
                  seed = 7,
                  direction = "both",
                  segment.size = NA) + 
  geom_label(data = df %>% filter(Model == "Prior Year Avg"),
             aes(label = Model), nudge_x = -.045,
             label.size = NA, size = 4) + 
  scale_color_identity() + 
  scale_y_reverse(limits = c(1400,0), breaks = seq(0,1400,200), labels = scales::comma(seq(0,1400,200))) + 
  scale_x_continuous(limits = c(.6,.9), position = "top", breaks = seq(.6,.9, .05), labels = scales::percent(seq(.6,.9,.05),accuracy = 1)) +
  labs(y = "Number of Issues per 1,000", x = "% satisfied or highly satisfied") +
  coord_cartesian(clip = "off") + 
  annotation_custom(grob_title) + 
  annotation_custom(grob_satisfaction) + 
  annotation_custom(grob_low) + 
  annotation_custom(grob_high) + 
  annotation_custom(grob_high_sat_low_issue) +
  annotation_custom(grob_high_sat_many_issue) +
  annotation_custom(grob_few) +
  annotation_custom(grob_things_wrong) +
  annotation_custom(grob_many)

height <- 6
width <- 8
dev.new(width = width, height = height, units = "in", noRStudioGD = T)
pt
# TODO: ggsave outputs the geom_text_repel labels in different positions. Can't figure out how to keep it conisistent with dev.new plot output
# ggsave(file.path("plot output","FIG0506.png"), pt, height = height, width = width)

