library(tidyverse)
library(forcats)
library(lemon)

source("include.R")
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.ticks.x = element_line(color=GRAY9),
                                  axis.line.x = element_line(size = .1, color=GRAY9),
                                  axis.line.y = element_blank(),
                                  axis.text.x = element_text(color = GRAY8, size = 10),
                                  axis.title.x = element_text(color = GRAY5, size = 10, hjust = 0),
                                  axis.title.y = element_blank(),
                                  strip.placement = "outside",
                                  strip.background = element_rect(fill=NA,color=GRAY9),
                                  plot.subtitle = element_text(color = GRAY4, size = 9, hjust=0),
                                  plot.caption = element_text(hjust = 0, size = 8, color = GRAY8),
                                  plot.margin = unit(c(1,4,1,1),"cm")))

df <- read_csv(file.path("data","FIG0314.csv")) %>% mutate(value = as.numeric(str_remove_all(value, "%"))/100) %>%
  mutate(category = fct_rev(fct_relevel(factor(category), "Demonstration of results", "Content expertise", "Local knowledge", "National reputation", "Affordability of services", "Previous work together", "Colleague recommendation"))) %>%
  mutate(fill = case_when(category == "Demonstration of results" ~ GRAY2,
                          category == "Affordability of services" ~ GRAY2,
                          category == "Previous work together" ~ GRAY2, 
                          TRUE ~ GRAY8))

pt <- ggplot(df, aes(x = category, y = value)) + geom_col(aes(fill = fill), width = .8) +
  scale_x_discrete() + 
  scale_y_continuous(position = "right", limits=c(0,.8), labels = scales::percent) +
  scale_fill_identity() + 
  coord_capped_flip(top = "both") + 
  labs(caption = "Data source: xyz; indluces N number of surbey respondents.\nNote that respondents were able to choose up to 3 options.",
       y = "% selecting given attribute",
       title = "Demonstrating effectiveness is most important consideration\nwhen selecting a provider",
       subtitle = "In general, what attributes are the most important\nto you in selecting a service provider\n(Choose up to 3)")
  
ggsave(file.path("plot output","FIG0314.png"), pt, width = 6, height = 4)
pt
