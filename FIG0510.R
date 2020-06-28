library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
library(gridtext)
source("include.R")

theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.line = element_line(color = GRAY9),
                                  axis.title.y = element_text(color = GRAY8, hjust = 1),
                                  axis.title.x = element_text(color = GRAY8, hjust = 0),
                                  axis.ticks = element_line(color = GRAY9),
                                  axis.text = element_text(color = GRAY8, size = 12),
                                  plot.margin = unit(c(1,4,1,1), "cm"),
                                  plot.title = element_text(size = 18),
                                  plot.caption = element_text(color = GRAY8, hjust = 0, margin = margin(.3,0,0,0,"cm"))
                                  ))

df <- read_csv(file.path("data","FIG0410-14.csv")) %>% rename(ticket_type = X1) %>%
  mutate(ticket_type = forcats::fct_rev(factor(case_when(ticket_type == "Ticket Volume Received" ~ "Received",
                                 ticket_type == "Ticket Volume Processed" ~ "Processed")))) %>%
  pivot_longer(cols = !ticket_type, names_to = "month", values_to = "tickets") %>%
  mutate(date = lubridate::ymd(paste0("2000-",match(month,month.abb),"-01"))) 

grob_explanation <- grobTree(richtext_grob(
  "<span style='background-color:white'><b>2 employees quit in May.</b> We nearly kept up with<br>incoming bolume in the following two months, but fell<br>behind with the increase in Aug and haven't been able<br>to catch up since</span>", 
  x=.35,  y=.9, hjust=0, gp=gpar(col = GRAY3, fontsize=11), box_gp = gpar(col = "white", fill = "white"),
  padding = margin(.4,0,0,0,"in")))

pt <- ggplot(df) + geom_line(aes(x = date, y = tickets, color = ticket_type),size = 2) + 
  geom_point(aes(x = date, y = tickets, color = ticket_type),size = 4,data = df %>% filter(date >= ymd("2000-08-01"))) +
  geom_text(aes(x = date, y = tickets, color = ticket_type, label = tickets), nudge_y = 15, size = 4,data = df %>% filter(date >= ymd("2000-08-01"), ticket_type == "Received")) +
  geom_text(aes(x = date, y = tickets, color = ticket_type, label = tickets), nudge_y = -15, size = 4,data = df %>% filter(date >= ymd("2000-08-01"), ticket_type == "Processed")) +
  geom_text(aes(x = date, y = tickets, color = ticket_type, label = ticket_type), nudge_x = 5, hjust = 0, size = 5, data = df %>% filter(month == "Dec")) + 
  geom_vline(xintercept = ymd("2000-05-01"), color = GRAY8) + 
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0,0)) + 
  scale_y_continuous(breaks = seq(0,300,50), limits = c(0,300)) + 
  scale_color_manual(values = c("Received" = GRAY7, "Processed" = BLUE1), guide = NULL) + 
  coord_cartesian(clip = "off") + 
  labs(title = "Ticket volume over time", 
       caption = "Data source: XYZ Dashboard, as of 12/31/2014 | A detailed analysis on tickets processed per\nperson and time to resolve issues was undertaken to inform this request and can be provided.",
       x = "2014",
       y = "Number of tickets") +
  annotation_custom(grob_explanation)
 
height <- 5.5
width <- 8
dev.new(width = width, height = height, units = "in", noRStudioGD = T)
pt
ggsave(file.path("plot output", "FIG0510.png"), pt, width = width, height = height)
