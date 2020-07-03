rm(list=ls())
library(tidyverse)
library(ggtext)

source("theme/theme_swd.R")
theme_set(theme_swd() + theme(axis.ticks = element_blank(),
                              axis.title = element_blank(),
                              axis.line = element_blank(),
                              axis.text.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank(),
                              plot.margin = unit(c(0,0,0,0),"cm")
))

df <- read_csv(file.path("data","FIG0202-3.csv")) %>% mutate(Value_pct = scales::percent(Value/100))


# Function to format a string text as markdown/HTML
format_text <- function(text, color = "black", size = 20, bold = FALSE) {
  opening_span = paste0("<span style='font-size:", size,"px; color:", color,"'>")
  if(bold) text = paste0("**", text, "**")
  closing_span = "</span>"
  paste0(opening_span, text, closing_span)
}

# Gather the formatted label into a single tibble
text_data <- tibble(
  label = paste0(format_text(df %>% filter(Year == 2012) %>% pull(Value_pct), color = GREEN3, size = 128, bold = TRUE), "<br/>",
                 format_text("of children had a", GRAY3),"<br/>",
                 format_text("traditional stay-at-home mom", color = GREEN3, bold = TRUE), "<br/>",
                 format_text(paste0("in 2012, compared to ", df %>% filter(Year == 1970) %>% pull(Value_pct)," in 1970"), color = GRAY3)))

# Use geom_richtext from the ggtext package to render the label
pt <- text_data %>% 
  ggplot(aes(x = 0, y = 0))  +
  geom_richtext(aes(label = label), 
                hjust = 0, 
                label.color = NA, 
                lineheight = 1.5) +
  xlim(0, 0.01)

ggsave(file.path("plot output","FIG0203.png"), pt, width = 3.5, height = 2.5)
pt

