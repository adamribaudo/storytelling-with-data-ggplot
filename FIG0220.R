# TODO: 
# Having trouble mixing bold text into plain text while also making the line wrap around the entire sentence. 
# Example: "plain('we bring\n')~bold('25\ncandidates\nonsite')~plain('for interviews...')"


library(tidyverse)
source("include.R")

theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.title = element_blank(),
                                  axis.ticks = element_blank(),
                                  axis.line = element_blank(),
                                  axis.text = element_blank(),
                                  plot.title = element_text(hjust=.1),
                                  plot.margin = unit(c(1,4,1,1),"cm"),
                                  ))

df_phone <- expand.grid(x = 0:9,y=0:9)
df_interview <- expand.grid(x = 5:9,y=0:4)
df_offer <- expand.grid(x = 7:9,y=0:2)

pt <- ggplot() + 
  geom_tile(aes(x,y),data=df_phone,color = "white", fill = GRAY9) +
  geom_tile(aes(x,y),data=df_interview,color = "white", fill = GRAY6) + 
  geom_tile(aes(x,y),data=df_offer,color = "white", fill = GREEN3) + 
  labs(title = "Interview Breakdown") + coord_cartesian(clip = "off") +
  annotate("text", label = "Out of every 100\nphone screens...", x = 10, y = 8, color= GRAY9, hjust = 0) + 
  annotate("text", label = "we bring 25\ncandidates\nonsite for\ninterviews...", x = 10, y = 3.5, color= GRAY6, hjust = 0) +
  annotate("text", label = "and", parse = F, x = 10, y = 0, color= GREEN3, hjust = 0) +
  
  annotate("text", label = "bold('extend 9 offers.')", parse = T, x = 10, y = -.6, color= GREEN3, hjust = 0) 

ggsave("plot output\\FIG0220.png", pt, width = 5, height = 4)
pt
