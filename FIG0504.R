library(tidyverse)
library(lemon)
library(grid)
library(gridExtra)

source("include.R")

theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.title = element_blank(),
                                  axis.line.x = element_line(color = GRAY9),
                                  axis.ticks.x = element_line(color = GRAY9),
                                  axis.text.x = element_text(size = 12, GRAY9),
                                  axis.line.y = element_blank(),
                                  axis.text.y = element_blank(),
                                  plot.margin = unit(c(2,1,4,7.5), "cm"),
                                  plot.subtitle = element_text(color = GRAY4),
                                  plot.title = element_text(hjust = -3),
                                  plot.caption = element_text(size = 8, color = GRAY7, hjust = 0, margin = margin(15,0,0,0, "pt")),
                                  ))

df <- read_csv(file.path("data","FIG0502-05.csv")) %>%
  pivot_longer(cols = -Category, names_to = "year", values_to = "value") %>%
  mutate(Category = str_remove_all(Category, "\\n")) %>%
  mutate(color = case_when(Category == "Bachelor's degree or more" ~ ORANGE1,
                           T ~ GRAY9)) %>%
  filter(Category != "All")

grob_title <- grobTree(textGrob("New marriage rate by education", x=-.7,  y=1.1, hjust=0,gp=gpar( fontsize=16)))
grob_subtitle <- grobTree(textGrob("Number of newly married adults per 1,000 marriage eligible adults", x=-.7,  y=1.03, hjust=0,gp=gpar(col = GRAY6, fontsize=14)))
grob_caption <- grobTree(textGrob("Note: Marriage eligible includes the newly married plus those widowed,\ndivorced\nSource: US Census\nAdapted from PEW RESEARCH CENTER", 
                                  x=-.7,  y=-.2, hjust=0,gp=gpar(col = GRAY6, fontsize=11)))

pt <- ggplot(df, aes(x = year, y = value, group = Category, color = color )) + 
  geom_line(size = 3) +
  geom_point(data = df %>% filter(year == 2008 | year == 2012), size = 5) +
  geom_text(data = df %>% filter(year == 2008), aes(label = Category), vjust = 1, nudge_y = 1.7, hjust = 1, nudge_x = -.7, size = 6) +
  geom_text(data = df %>% filter(year == 2008), aes(label = paste0("bold(",round(value),")")), nudge_x = -.4, parse = T, size = 6) +
  geom_text(data = df %>% filter(year == 2012), aes(label = paste0("bold(",round(value),")")), nudge_x = .4, parse = T, size = 6) +
  scale_color_identity() +
  coord_cartesian(clip = "off") + 
  ylim(c(0,64)) + 
  annotation_custom(grob_title) + 
  annotation_custom(grob_subtitle) + 
  annotation_custom(grob_caption)

height <- 6
width <- 7
#dev.new(width = width, height = height, units = "in")
pt
ggsave(file.path("plot output","FIG0504.png"), pt, height = height, width = width)

