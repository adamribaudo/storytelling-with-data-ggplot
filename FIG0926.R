library(tidyverse)
source("include.R")

theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.title = element_blank(),
                                  axis.line.x = element_line(color = GRAY7),
                                  axis.ticks.x = element_blank(),
                                  axis.text.x = element_text(size = 12),
                                  axis.line.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  plot.margin = unit(c(1,3,1,1), "cm"),
                                  plot.subtitle = element_text(hjust = 1, color = BLUE2),
                                  plot.caption = element_text(size = 8, color = GRAY7, hjust = .5, margin = margin(15,0,0,0, "pt")),
                                  strip.text.y.left = element_text(size = 14, angle = 0,hjust=1)
                                  ))

df <- read_csv("data\\FIG0921-26.csv") %>% rename(category = Category) %>% 
  mutate(category_super = category) %>% expand(category,category_super) %>% 
  left_join(read_csv("data\\FIG0921-26.csv") %>% rename(category = Category)) %>%
  pivot_longer(cols = !c(category,category_super), names_to = "year", values_to = "value") %>%
  mutate(value = as.numeric(str_remove(value,"\\%"))/100) %>% mutate(year = as.factor(year)) %>%
  mutate(category_super_wrap = factor(stringr::str_wrap(category_super, width = 6))) %>%
  mutate(category_super_wrap = forcats::fct_relevel(category_super_wrap, "Health", "Education", "Human\nservices", "Arts &\nculture", "Other")) %>%
  mutate(value_pct = scales::percent(value))


pt <- ggplot(df) + geom_line(aes(x = year, y = value, group = category), color = GRAY9, size = 1.2) +
  scale_y_continuous(labels = scales::percent, expand = c(.2,0)) +
  geom_line(aes(x = year, y = value, group = category), color = BLUE2, size = 1.2, data = df %>% filter(category_super == category)) + 
  geom_point(aes(x = year, y = value), color = BLUE2, size = 3, data = df %>% filter(category_super == category, year == 2010 | year == 2015)) +
  geom_text(aes(x = year, y = value, label = value_pct), color = BLUE2, size = 4, nudge_x = -.35, data = df %>% filter(category_super == category, year == 2010)) +
  geom_text(aes(x = year, y = value, label = paste0("bold('",value_pct,"')")), parse = T, color = BLUE2, size = 5, nudge_x = .5, data = df %>% filter(category_super == category, year == 2015)) +
  facet_grid(rows = vars(category_super_wrap), switch = "y") +
  scale_x_discrete(position = "top") + 
  coord_cartesian(clip = "off") + 
  labs(title = "Types of non-profits supported by area funders", subtitle = "% of funders",
       caption = "Data is self-reported by funders; percents sum to greater than 100 because respondents can make multiple selections")
 
ggsave("plot output\\FIG0926.png", pt, width = 6, height = 7)
pt
