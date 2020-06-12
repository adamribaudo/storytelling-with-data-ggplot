library(tidyverse)

source("include.R")
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  plot.title = element_text(hjust = -.825),
                                  axis.ticks = element_blank(),
                                  axis.title = element_blank(),
                                  axis.line = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.text.x = element_blank(),
                                  plot.subtitle = element_text(color = GRAY4, size = 9, hjust=.22),
                                  plot.margin = unit(c(1,1,1,5),"cm"),
                                  strip.text.y.left = element_text(angle = 0,hjust=1),
                                  legend.position = c(-.4,.90),
                                  legend.key.size = unit(1,"lines"),
                                  legend.title = element_blank(),
                                  legend.spacing = unit(1.0, "cm")
                                  ))

df <- read_csv("data\\FIG0315-16.csv") %>% 
  select(-Rank) %>% pivot_longer(cols = !Category, names_to = "Business", values_to = "Result") %>%
  # Reorder factors to match the original plot
  mutate(Category = forcats::fct_relevel(Category,"Price","Convenience", "Relationship", "Service", "Selection")) %>%
  mutate(Business = forcats::fct_rev(forcats::fct_relevel(Business, "Our business", "Competitor A", "Competitor B", "Competitor C", "Competitor D", "Competitor E"))) %>% 
  # Calculate ranks
  group_by(Category) %>% mutate(rank = paste((7 - rank(Result)),"of 6")) %>% mutate(rank = if_else(Business == "Our business",rank,""))

pt <- ggplot(df) + geom_col(aes(x = Business, y = Result, fill = Business), width = 1) + 
  scale_fill_manual(values = c("Our business" = BLUE2, "Competitor A" = GRAY9, "Competitor B" = GRAY9, "Competitor C" = GRAY9, "Competitor D" = GRAY9, "Competitor E" = GRAY9), 
                    guide = guide_legend(reverse = T)) +
  facet_grid(rows=vars(Category),switch = "y") + 
  geom_text(aes(x = Business, y = Result, label = rank),nudge_y = .15,nudge_x = .2,color = BLUE2,size = 3) + 
  coord_flip(clip = "on") + 
  labs(title = "Performance overview", subtitle = "Weighted performance index | relative rank")
  
ggsave("plot output\\FIG0316.png", pt, width = 8, height = 6)
pt
