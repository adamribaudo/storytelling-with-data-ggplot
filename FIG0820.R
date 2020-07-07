rm(list = ls())
library(tidyverse)
library(grid)
library(lemon)
library(stringr)
source("theme/theme_swd.R")

theme_set(theme_swd() + theme(
  axis.title = element_text(color = GRAY6),
  axis.title.x = element_text(hjust = .1),
  axis.text.y = element_text(color = c(GRAY6,BLUE1,BLUE1,GRAY6,GRAY6,GRAY6))
))

df <- read_csv(file.path("data", "FIG0820.csv")) %>%
  pivot_longer(cols = -product, names_to = "year", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(value = as.numeric(str_remove(value,"\\$"))) %>%
  mutate(product_letter = str_sub(product, str_length(product),str_length(product)))

df_2014_avg <- df %>% filter(year == "2014") %>% group_by(year) %>% summarise(value = mean(value))
df_product_labels <- df %>% group_by(product_letter) %>% mutate(value = if_else(year == min(as.numeric(year)), value,  0)) %>% summarise(year = as.character(min(as.numeric(year))), value = sum(value))

grob_recommended = grobTree(textGrob("Recommended", x = .015, y = .372, hjust = 0, gp = gpar(col = BLUE1)))

pt <- df %>% ggplot(aes(x = year, y = value, group = product)) +
  geom_rect(data = df %>% head(1),xmin = -Inf, xmax = "2014", ymin = 150, ymax = 200, fill = GRAY9, alpha = .5) +
  geom_line(size = 1.2, color = GRAY6) + 
  geom_point(data = df_2014_avg, aes(group = NA), size = 2, color = BLUE1) + 
  geom_text(data = df_product_labels, 
            aes(label = product_letter, group = NA),
            nudge_x = -.1, color = GRAY6, size = 3.5) +
  geom_text(data = df_2014_avg, aes(group = NA, label = "AVG"), 
            color = BLUE1, nudge_x = -.3, size = 3, nudge_y = 10) +
  scale_y_continuous(breaks = c(0,150,200,300,400,500), 
                     limits = c(0,500),
                     labels = scales::number_format(prefix = "$")) + 
  scale_x_discrete(expand = c(.15,0, .05, 0)) + 
  labs(title = "Retail price over time",
       y = "Average price",
       x = "Year") + 
  annotation_custom(grob_recommended)
 
width <- 6.5
height <- 4
dev.new(width = width, height = height, noRStudioGD = T)
pt
ggsave(file.path("plot output", "FIG0820.png"), pt, width = width, height = height)
