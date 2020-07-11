# TODO:
# Unclear how to generate a legend above the plot that shows a different number of elements than the scale itself
# Technically, there are 11 unique fill values, but we only want to show 6 because 5 of them are to highlight distinct elements on the plot
# We could use custom annoations to 'fake' a legend, but I was hoping to find a way using guide_legend

rm(list = ls())
library(tidyverse)
library(grid)
library(gridExtra)
library(gridtext)
library(lemon)
library(stringr)
library(ggtext)
source("theme/theme_swd.R")
source("helper_functions.R")

theme_set(theme_swd() + theme(
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.line.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank(),
  axis.text.y = element_text(size = 12, color = GRAY2),
  axis.line.y = element_blank(),
  plot.title = element_markdown(),
  plot.margin = margin(.5,5,.5,.5,unit = "cm"),
  legend.text = element_markdown(color = GRAY9,size = 9),
  legend.position = "top"
))


fill_values <- c("Have not used" = GREEN5,
                 "Have not used|highlighted" = GREEN3,
                 "Not satisfied at all" = ORANGE2,
                 "Not satisfied at all|highlighted" = ORANGE1,
                 "Not very satisfied" = ORANGE2,
                 "Not very satisfied|highlighted" = ORANGE1,
                 "Somewhat satisfied" = GRAY9,
                 "Very satisfied" = BLUE4,
                 "Very satisfied|highlighted" = BLUE5,
                 "Completely satisfied" = BLUE4,
                 "Completely satisfied|highlighted" = BLUE5
)
fill_labels <- rev(names(fill_values))

df <- read_csv(file.path("data", "FIG0920.csv")) %>%
  pivot_longer(cols = -feature, names_to = "results", values_to = "value") %>%
  mutate(feature = forcats::fct_rev(factor(feature))) %>%
  mutate(results = forcats::fct_rev(forcats::fct_relevel(factor(results), "Have not used", "Not satisfied at all", "Not very satisfied", "Somewhat satisfied", "Very satisfied", "Completely satisfied"))) %>%
  # Denote highlighted portions of the bar chart based on the original graph
  mutate(highlighted = case_when((feature == "Feature A" | feature == "Feature B") & (results == "Very satisfied" | results == "Completely satisfied") ~ T,
                                 (feature == "Feature J" | feature == "Feature N") & (results == "Not satisfied at all" | results == "Not very satisfied") ~ T,
                                 (feature == "Feature O") & (results == "Have not used") ~ T,
                                 T ~ F)) %>%
  # Create a new factor for the fill aesthetic that takes into account the highlighting
  mutate(fill = factor(case_when(
                          highlighted ~ paste0(results,"|highlighted"),
                          T ~ as.character(results)
                          ))) %>%
  mutate(fill = forcats::fct_relevel(fill, fill_labels)) %>%
  mutate(highlight_label = case_when(highlighted ~ scales::percent(x = value,accuracy = 1),
                                      T ~ ""))


grob_blue = grobTree(richtext_grob("Feature A and B<br>continue to top user<br>satisfaction", 
                                   padding = unit(c(5,5,5,5),"pt"),
                                              x=.98,  y= 1, vjust = 1, hjust=0, box_gp = gpar(fill = BLUE5), gp=gpar(col = "white", fontsize=13)))
grob_orange = grobTree(richtext_grob("Users are least<br>satisfied with<br>Features J and N;<br>what improvements<br>can we make here for<br>a better user<br>experience?", 
                                   padding = unit(c(5,5,5,5),"pt"),
                                   x=.98,  y=.5, vjust = .5, hjust=0, box_gp = gpar(fill = ORANGE1), gp=gpar(col = "white", fontsize=13)))

grob_green = grobTree(richtext_grob("Feature O is least<br>used. What steps can<br>we proactively take<br>with existing users to<br>increase utilization?", 
                                     padding = unit(c(5,5,5,5),"pt"),
                                     x=.98,  y=0, vjust = 0, hjust=0, box_gp = gpar(fill = GREEN3), gp=gpar(col = "white", fontsize=13)))


# Function to apply color to legend labels using markdown
assign_label_color_markdown <- function(colors, labels)
{
  sprintf("<span style='color:%s'>%s</span>",colors,labels)
}

pt <- df %>% ggplot(aes(x = feature, y = value, fill = fill)) +
  geom_col(color = "white", width = .7) + 
  geom_text(aes(label = highlight_label), color = "white", position = position_stack(vjust = .8),vjust = .45) +
  coord_flip(clip = "off") + 
  scale_fill_manual(
    values = fill_values,
    labels = assign_label_color_markdown(rev(fill_values), fill_labels),
                    guide = guide_legend(title = "",
                                         nrow = 1)) + 
  labs(title = "Product X User Satisfaction: **Features**") + 
  annotation_custom(grob_blue) + 
  annotation_custom(grob_orange) + 
  annotation_custom(grob_green)

pt %>% save_and_show_plot(width = 9.5, height = 6, "FIG0920.png")
