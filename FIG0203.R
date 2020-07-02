rm(list=ls())
library(tidyverse)
source("include.R")
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.ticks = element_blank(),
                                  axis.title = element_blank(),
                                  axis.line = element_blank(),
                                  axis.text.y = element_blank(),
                                  axis.text.x = element_blank(),
                                  plot.margin = unit(c(0,0,0,0),"cm")
))

df <- read_csv(file.path("data","FIG0202-3.csv")) %>% mutate(Value_pct = scales::percent(Value/100))

pt <- ggplot(df) + 
  annotate("text", hjust = 0, x = 0, y = .05, label = paste0("bold('",df %>% filter(Year == 2012) %>% pull(Value_pct),"')"), size = 20, parse=T, color = GREEN3) +
  annotate("text", hjust = 0, x = 0, y = -.2, label = "of children had a", color = GRAY3) + 
  annotate("text", hjust = 0, x = 0, y = -.3, label = "bold('traditional stay-at-home-mom')", parse=T, color = GREEN3) + 
  annotate("text", hjust = 0, x = 0, y = -.4, label = paste0("in 2020, compared to ",df %>% filter(Year == 1970) %>% pull(Value_pct)," in 1970"), color = GRAY3) + 
  coord_cartesian(clip = "off") + ylim(c(-.6,.6)) + 
  xlim(c(-.1,1.75))

ggsave(file.path("plot output","FIG0203.png"), pt, width = 3, height = 2.5)


# Wal - Attempt using ggtext package
library(ggtext)

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
pt_2 <- text_data %>% 
  ggplot(aes(x = 0, y = 0))  +
  geom_richtext(aes(label = label), 
                hjust = 0, 
                label.color = NA, 
                lineheight = 1.5, 
                family = "Helvetica") +
  xlim(0, 0.01)

ggsave(file.path("plot output","FIG0203_wal.png"), pt_2, width = 3, height = 2.5)
pt_2

library(patchwork)
pt + pt_2
