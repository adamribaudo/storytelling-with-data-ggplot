library(tidyverse)
library(grid)
library(gridExtra)
library(gridtext)
source("include.R")

theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.line = element_line(color = GRAY9),
                                  axis.title.y = element_text(color = GRAY5, hjust = 1),
                                  axis.title.x = element_text(color = GRAY5, hjust = 0),
                                  axis.ticks = element_line(color = GRAY9),
                                  axis.text = element_text(color = GRAY6, size = 9),
                                  plot.margin = unit(c(1,1,1,1), "cm"),
                                  plot.title = element_text(hjust = -.22, size = 18, color = GRAY2, margin = margin(0,0,.5,0,"cm")),
                                  plot.caption = element_text(color = GRAY8, hjust = 0, margin = margin(.3,0,0,0,"cm"))
                                  ))

df <- read_csv(file.path("data","FIG0601.csv")) %>% 
  mutate_at(vars(-`Days since launch`), ~ as.numeric(str_remove_all(., "\\$|,"))) %>%
  pivot_longer(cols = -`Days since launch`, names_to = "period", values_to = "value")

grob_goa <- grobTree(richtext_grob(
  "GOAL", 
  x=.1,  y=.836, hjust=0, gp=gpar(col = GRAY3, fontsize=11), box_gp = gpar(col = "white", fill = "white"),
  padding = margin(.1,.1,.1,.1,"in")))
grob_progress <- grobTree(richtext_grob("Progress to date", x=.19,  y=.64, hjust=0, gp=gpar(col = BLUE2, fontsize=16)))
grob_last_year <- grobTree(richtext_grob("Last year", x=.94,  y=.9, hjust=0, gp=gpar(col = BLUE2, fontsize=12)))

pt <- ggplot(df, aes(x = `Days since launch`, y = value, group = period)) + 
  # Last Year
  geom_line(data = df %>% filter(period == "Last year"), color = BLUE2) + 
  geom_point(data = df %>% filter(period == "Last year", `Days since launch` == 30), size = 2, color = BLUE2) + 
  geom_text(data = df %>% filter(period == "Last year", `Days since launch` == 30), 
            aes(label = scales::number_format(prefix = "$",big.mark=",")(value)),
            nudge_x = 2,
            size = 3.5, color = BLUE2) + 
  # Progress to Date
  geom_line(data = df %>% filter(period == "Progress to date"), color = BLUE2, size = 2) +
  geom_text(data = df %>% filter(period == "Progress to date", `Days since launch` ==10), 
            aes(label = paste0("bold('",scales::number_format(prefix = "$",big.mark=",")(value),"')")),
                color = BLUE2, 
            nudge_x = 2.5,
            parse = T,
            size = 5) + 
  geom_point(data = df %>% filter(period == "Progress to date", `Days since launch` ==10), size = 4, color = BLUE2) + 
  scale_y_continuous(labels = scales::number_format(prefix="$",big.mark = ","), 
                     expand = c(0,0),
                     limits = c(0, 60000)) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,30,5)) +
  geom_hline(yintercept = 50000, color = GRAY2) +
  
  labs(title = "Annual giving campaign promise",
       x = "Days since campaign launch",
       y = "Money raised") + 
  coord_cartesian(clip = "off") + 
  annotation_custom(grob_goa) + 
  annotation_custom(grob_progress) + 
  annotation_custom(grob_last_year)
  
height <- 5.5
width <- 8
dev.new(width = width, height = height, units = "in", noRStudioGD = T)
pt
ggsave(file.path("plot output", "FIG0601.png"), pt, width = width, height = height)
