library(tidyverse)
library(patchwork)
library(lemon)
source("helper_functions.R")
source("theme/theme_swd.R")

df <- read_csv(file.path("data", "FIG0226-27.csv")) %>%
  mutate(revenue_num = parse_number(Revenue), 
         year_qrt = paste(Year, Quarter))

# Theme for first plot
theme_pt1 <- theme_swd() + theme(
  axis.title.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.line.x = element_blank(),
  axis.text.x = element_blank())

pt1 <- ggplot(df, aes(x = year_qrt, y = `Size of Salesforce`, group = 1)) +
  geom_point(color = BLUE1, size = 2) +
  geom_line(color = BLUE1, size = 1.2) +
  scale_y_continuous(limits = c(60, 120)) +
  coord_capped_cart(left = "both") +
  labs(title = "Alternative 2: pull apart vertically", y = "# of Sales\nEmplyees") +
  theme_pt1

# Theme for second plot
theme_pt2 <- theme_swd() + theme(
  axis.title.x = element_blank(),
  axis.line = element_line(color = GRAY8),
  axis.ticks = element_line(color = GRAY8),
  panel.spacing.x = unit(-.15,"cm"),
  strip.switch.pad.grid = unit(0,"cm"))

pt2 <- ggplot(df, aes(x = Quarter, y = revenue_num)) +
  geom_col(fill = BLUE2, width = .7) +
  scale_x_discrete(labels = df$Quarter, expand = c(.2, 0)) +
  scale_y_continuous(
    labels = scales::dollar_format(accuracy = .1, scale = .000001),
    limits = c(0, 1000000)
  ) +
  facet_grid(cols = vars(Year), switch = "x") +
  labs(y = "Revenue (Millions)") +
  coord_capped_cart(left = "both", bottom = "both") +
  theme_pt2

# Combine charts into single plot (using patchwork)
combined_plot <- pt1 + pt2 + plot_layout(ncol = 1)

combined_plot %>%
  save_and_show_plot(width = 6, height = 4, "FIG0227.png")
