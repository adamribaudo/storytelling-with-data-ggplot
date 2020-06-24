library(tidyverse)
library(forcats)
library(grid)
library(gridExtra)

source("include.R")
theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.ticks = element_blank(),
                                  axis.line = element_blank(),
                                  axis.title = element_blank(),
                                  axis.text.x = element_blank(),
                                  axis.text.y = element_text(color = "black"),
                                  axis.title.x = element_blank(),
                                  legend.position = "top",
                                  legend.spacing.x = unit(.1,"lines"),
                                  legend.title = element_text(color = GRAY8,margin = margin(0,1,0,0,"lines")),
                                  legend.margin = margin(0,1,1,1,"lines"),
                                  #legend.title.align = 1,
                                  legend.key.size = unit(1,"lines"),
                                  legend.key.width = unit(2.5,"lines"),
                                  plot.margin = unit(c(1,1,1,1),"cm")))

df <- read_csv(file.path("data","FIG0415.csv")) %>% pivot_longer(cols = -Country, names_to = "drug", values_to = "value") %>%
  mutate(Country = fct_rev(factor(Country))) %>% 
  mutate(value_fct = factor(case_when(value == 1 ~ "1",
                               value == 2 ~ "2",
                               value == 3 ~ "3",
                               value == 4 ~ "4",
                               value >= 5 ~ "5+")))

grob <- grobTree(textGrob("COUNTRY | DRUG", x=-.22,  y=1.05, hjust=0,gp=gpar(col=GRAY8, fontsize=11)))
pt <- ggplot(df) +
  geom_tile(aes(x = drug, y = Country, fill = value_fct)) + 
  geom_text(aes(x = drug, y = Country, label = value, color = (value_fct == "1" | value_fct == "2" | value_fct == "3")), size = 3.5) + 
  scale_fill_manual(guide = guide_legend(label.position = "top", 
                                         title.vjust = .2,
                                         title = "RANK"), values = rev(scales::brewer_pal("seq")(5))) + 
  scale_color_manual(guide = FALSE, values = c(GRAY6,"white")) + 
  labs(title = "Country Level Sales Rank") + 
  annotation_custom(grob) + 
  coord_cartesian(clip = "off")

width <- 5.5
height <- 6
#dev.new(width = width, height = height, unit = "in", noRStudioGD =T)
pt
ggsave(file.path("plot output","FIG0415.png"), pt, width = width, height = height)

