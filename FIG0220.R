library(tidyverse)
library(ggtext)
source("theme/theme_swd.R")

theme_set(theme_swd() + theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line = element_blank(),
  axis.text = element_blank(),
  plot.margin = unit(c(1, 4, 1, 1), "cm")
))

df_phone <- expand.grid(x = 0:9, y = 0:9)
df_interview <- expand.grid(x = 5:9, y = 0:4)
df_offer <- expand.grid(x = 7:9, y = 0:2)

pt <- ggplot() +
  geom_tile(aes(x, y), data = df_phone, color = "white", fill = GRAY9) +
  geom_tile(aes(x, y), data = df_interview, color = "white", fill = GRAY6) +
  geom_tile(aes(x, y), data = df_offer, color = "white", fill = GREEN3) +
  labs(title = "Interview Breakdown") +
  coord_cartesian(clip = "off") +
  geom_richtext(aes(x = 10, y = 8), label = "Out of every **100<br/>phone screens**...", color = GRAY9, hjust = 0, label.color = NA) +
  geom_richtext(aes(x = 10, y = 3.5), label = "we bring **25 <br/>candidates onsite** <br/>for\ninterviews...", color = GRAY6, hjust = 0, label.color = NA) +
  geom_richtext(aes(x = 10, y = 0), label = "and <br /> **extend 9 offers.**", color = GREEN3, hjust = 0, label.color = NA)

ggsave(file.path("plot output", "FIG0220.png"), pt, width = 5, height = 4)
pt
