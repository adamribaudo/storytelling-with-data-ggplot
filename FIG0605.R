# TODO: 
# Add underline below title and line above caption
# Fix placement and color of value %'s over bars
# Fix alignment of plot title & caption

rm(list = ls())
library(tidyverse)
library(forcats)
library(grid)
library(ggtext)
source("theme/theme_swd.R")

theme_set(theme_swd() + theme(
  axis.title = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y = element_blank(),
  axis.title.x = element_blank(),
  axis.text = element_blank(),
  axis.line.x = element_blank(),
  plot.title = element_text(margin = margin(b = 1, unit = "cm")),
  plot.subtitle = element_text(color = GRAY3, size = 12, hjust = 0),
  plot.caption = element_text(margin = margin(t = 1, unit = "cm")),
  plot.margin = unit(c(1, 1, 1, 6), "cm")
))

df <- read_csv(file.path("data", "FIG0605.csv")) %>% 
  pivot_longer(cols = -Category, names_to = "importance", values_to = "value") %>%
  mutate(value = as.numeric(str_remove(value, "%"))/100) %>%
  mutate(Category = fct_rev(fct_relevel(factor(Category), "Education", "Agriculture & rural development","Poverty reduction","Reconstruction",
                                                "Economic growth","Health","Job creation","Governanace",
                                                "Anti-corruption","Transport","Energy","Law & Justice",
                                                "Basic infrastructure","Public sector reform","Public financial management"))) %>%
  mutate(importance = fct_rev(fct_relevel(factor(importance), "Most important", "2nd Most Important", "3rd Most Important"))) %>%
  mutate(fill = case_when(
    (Category == "Education" | Category == "Agriculture & rural development" | Category == "Poverty reduction") & (importance == "Most important") ~ BLUE1,
    (Category == "Education" | Category == "Agriculture & rural development" | Category == "Poverty reduction") & (importance == "2nd Most Important") ~ BLUE2,
    (Category == "Education" | Category == "Agriculture & rural development" | Category == "Poverty reduction") & (importance == "3rd Most Important") ~ BLUE3,
    importance == "Most important" ~ GRAY3,
    importance == "2nd Most Important" ~ GRAY6,
    T ~ GRAY9
  )) 

df_text_labels <- df %>% filter(importance == "Overall") %>% select(Category, value) %>% 
  mutate(color = case_when(Category == "Education" | Category == "Agriculture & rural development" | Category == "Poverty reduction" ~ BLUE1,
              T ~ GRAY6)) %>%
  mutate(label = case_when(Category == "Education" | Category == "Agriculture & rural development" | Category == "Poverty reduction" ~ paste0("<b>",Category,"</b>"),
                           T ~ as.character(Category))) %>%
  mutate(value_label = case_when(Category == "Education" | Category == "Agriculture & rural development" | Category == "Poverty reduction" ~ paste0("<b>",scales::percent(accuracy = 1, x = value),"</b>"),
                           T ~ as.character(scales::percent(accuracy = 1, x = value))))
  

pt <- df %>% filter(importance != "Overall") %>% 
    ggplot(aes(x = Category, y = value, group = importance, fill = fill)) + 
    geom_col(position = "stack", width = .69) + 
    scale_fill_identity(guide = F) +
    geom_text(aes(label = scales::percent(accuracy = 1, x = value)), position = position_stack(vjust = 0), 
              color = "white") +
    geom_richtext(data = df_text_labels,
              aes(label = label, x = Category, y = 0, color = color, group = NA, fill = NA), fill = NA, label.color = NA, hjust = 1, vjust = .5, nudge_y = -.05) + 
    geom_richtext(data = df_text_labels,
                  aes(label = value_label, x = Category, y = 0, color = color, group = NA, fill = NA), fill = NA, label.color = NA, hjust = 1, vjust = .5, nudge_y = -.005) + 
    scale_color_identity() + 
    coord_flip(clip = "off") + 
    labs(title = "Top 15 development priorities, according to survey",
         caption = "N = 4,392. Based on response to item, When considering the development priorities, which one development priority is the most important? Which one is\nthe second most important priority? Which one is the third most important priority? Respondents chose from a list. Top 15 shown.")
    
width <- 8
height <- 6
dev.new(width = width, height = height, noRStudioGD = T)
pt
ggsave(file.path("plot output", "FIG0605.png"), pt, width = width, height = height)

