library(tidyverse)
library(lubridate)
source("include.R")

theme_set(theme_minimal() + theme(panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  axis.line = element_line(color = GRAY9),
                                  axis.title = element_blank(),
                                  axis.ticks = element_line(color = GRAY9),
                                  axis.text = element_text(color = GRAY8, size = 12),
                                  plot.margin = unit(c(1,4,1,1), "cm")
                                  ))

df <- read_csv("data\\FIG0410-14.csv") %>% rename(ticket_type = X1) %>%
  mutate(ticket_type = case_when(ticket_type == "Ticket Volume Received" ~ "Received",
                                 ticket_type == "Ticket Volume Processed" ~ "Processed")) %>%
  pivot_longer(cols = !ticket_type, names_to = "month", values_to = "tickets") %>%
  mutate(date = lubridate::ymd(paste0("2000-",match(month,month.abb),"-01")))

pt <- ggplot(df) + geom_line(aes(x = date, y = tickets, color = ticket_type),size = 2) + 
  geom_point(aes(x = date, y = tickets, color = ticket_type),size = 4,data = df %>% filter(date >= ymd("2000-08-01"))) +
  geom_text(aes(x = date, y = tickets, color = ticket_type, label = tickets), nudge_y = 15, size = 4,data = df %>% filter(date >= ymd("2000-08-01"), ticket_type == "Received")) +
  geom_text(aes(x = date, y = tickets, color = ticket_type, label = tickets), nudge_y = -15, size = 4,data = df %>% filter(date >= ymd("2000-08-01"), ticket_type == "Processed")) +
  geom_text(aes(x = date, y = tickets, color = ticket_type, label = ticket_type), nudge_x = 5, hjust = 0, size = 5, data = df %>% filter(month == "Dec")) + 
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0,0)) + 
  scale_y_continuous(breaks = seq(0,300,50), limits = c(0,300)) + 
  scale_color_manual(values = c("Received" = GRAY3, "Processed" = BLUE2), guide = NULL) + 
  coord_cartesian(clip = "off")
 
  
ggsave("plot output\\FIG0414.png", pt, width = 8, height = 6)
pt
