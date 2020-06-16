# TODO: 
# - add lines between bars
# - add 2nd row of x axis labels 
# - can't align bar labels to top of each bar without manually adjusting each one

library(tidyverse)
source("include.R")

theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.title = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.line.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.line.x = element_line(),
                                  plot.subtitle = element_text(color = GRAY2, size = 9, hjust=0),
                                  plot.margin = unit(c(1,1,1,1),"cm"),
                                  ))

df <- read_csv("data\\FIG0217.csv") %>% select(-`Label 1`) %>% rename(Category = `Label 2`) %>%
  pivot_longer(cols = -Category, names_to = "Series", values_to = "Value") %>%
  mutate(Category = forcats::fct_relevel(factor(Category), "1/1/2014", "Hires", "Transfers In", "Transfers Out", "Exits", "12/31/2014")) %>%
  mutate(Series = forcats::fct_relevel(factor(Series), "Visible Series", "Invisible Series")) %>%
  # Must hide the text values for the invisble series otherwise it will appear in geom_text
  mutate(text_value = if_else(Series == "Visible Series", Value, NULL))

  
  
pt <- ggplot(df, aes(x = Category, y = Value, fill = Series)) + geom_col(width = .5) + 
  scale_fill_manual(values = c("Visible Series" = BLUE2, "Invisible Series" = "white"), guide = NULL) + 
  geom_text(aes(label = text_value), position = position_stack(vjust = .5), color = "white", size = 3.5) +
  labs(title = "2014 Headcount math", subtitle = "Though more employees transferred out of the team than transferred in,\naggressive hiring means overall headcount (HC) increased 16% over the course of the year")
  
ggsave("plot output\\FIG0217.png", pt, width = 5.7, height = 4)
pt
