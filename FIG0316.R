rm(list = ls())
library(tidyverse)
library(ggtext)
library(lemon)
source("helper_functions.R")
source("theme/theme_swd.R")

theme_set(theme_swd() + theme(axis.ticks.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              axis.line = element_blank(),
                              axis.text = element_blank(),
                              plot.margin = unit(c(1,1,1,4),"cm"),
                              plot.subtitle = element_markdown(),
                              strip.text.y.left = element_text(angle = 0,hjust=1,color = GRAY2, size = 10),
                              legend.position = c(-.35,.9),
                              legend.key.size = unit(1,"lines"),
                              legend.title = element_blank(),
                              legend.text = element_markdown(size = 9,color = GRAY6),
                              legend.spacing = unit(1, "cm"),
                              plot.title.position = "panel",
                              plot.title = element_text(hjust = -.82, margin = margin(b = .5, unit = "cm"), size = 14, color = GRAY2)
                              ))

df <- read_csv(file.path("data","FIG0315-16.csv")) %>% 
  select(-Rank) %>% pivot_longer(cols = !Category, names_to = "Business", values_to = "Result") %>%
  # Reorder factors to match the original plot
  mutate(Category = forcats::fct_relevel(Category,"Price","Convenience", "Relationship", "Service", "Selection")) %>%
  mutate(Business = forcats::fct_rev(forcats::fct_relevel(Business, "Our business", "Competitor A", "Competitor B", "Competitor C", "Competitor D", "Competitor E"))) %>% 
  # Calculate ranks
  group_by(Category) %>% mutate(rank = paste((7 - rank(Result)),"of 6")) %>% mutate(rank = if_else(Business == "Our business",rank,""))

pt <- ggplot(df) + geom_col(aes(x = Business, y = Result, fill = Business), width = 1) + 
  scale_fill_manual(values = c(GRAY9,GRAY9,GRAY9,GRAY9,GRAY9, BLUE2), labels = c("Competitor E","Competitor D","Competitor C","Competitor B","Competitor A",sprintf("<span style='color:%s'>**Our business**</span>",BLUE2)), 
                    guide = guide_legend(reverse = T)) +
  facet_grid(rows=vars(Category),switch = "y") + 
  geom_text(aes(x = Business, y = Result, label = rank),nudge_y = .15,nudge_x = .2,color = BLUE2,size = 3) + 
  coord_flip(clip = "off") + 
  labs(title = "Performance overview", subtitle = sprintf("Weighted performance index | <span style='color:%s'>relative rank</span>",BLUE2))
  
pt %>% save_and_show_plot(width = 7.5, height = 5, "FIG03016.png")

