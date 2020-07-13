library(tidyverse)
library(lubridate)
source("helper_functions.R")
source("theme/theme_swd.R")

theme_set(theme_swd() + theme(
  axis.line = element_line(color = GRAY9),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks = element_line(color = GRAY9),
  axis.text = element_text(color = GRAY8, size = 12),
  plot.margin = unit(c(1, 4, 1, 1), "cm")
))

df <- read_csv(file.path("data", "FIG0410-14.csv")) %>%
  rename(ticket_type = X1) %>%
  mutate(ticket_type = case_when(
    ticket_type == "Ticket Volume Received" ~ "Received",
    ticket_type == "Ticket Volume Processed" ~ "Processed"
  )) %>%
  pivot_longer(cols = !ticket_type, names_to = "month", values_to = "tickets") %>%
  mutate(date = lubridate::ymd(paste0("2000-", match(month, month.abb), "-01")))

pt <- ggplot(df, aes(x = date, y = tickets, color = ticket_type)) +
  geom_line(size = 2) +
  geom_point(size = 4, data = df %>% filter(date >= ymd("2000-08-01"))) +
  geom_text(aes(label = tickets), nudge_y = 15, size = 4, data = df %>% filter(date >= ymd("2000-08-01"), ticket_type == "Received")) +
  geom_text(aes(label = tickets), nudge_y = -15, size = 4, data = df %>% filter(date >= ymd("2000-08-01"), ticket_type == "Processed")) +
  geom_text(aes(label = ticket_type), nudge_x = 5, hjust = -0.1, size = 5, data = df %>% filter(month == "Dec")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 300, 50), limits = c(0, 300)) +
  scale_color_manual(values = c("Received" = GRAY6, "Processed" = BLUE1), guide = NULL) +
  coord_cartesian(clip = "off")

pt %>%
  save_and_show_plot(width = 8, height = 6, "FIG0414.png")
