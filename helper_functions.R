# Function to both save a ggplot to file but also render it to the screen in the specified
# height and width
save_and_show_plot <- function(plot, width, height, file_name) {
  # Uncomment below to display the plot in a new window with the specified width/height which should match the ggsave output
  # dev.new(width = width, height = height, noRStudioGD = T)
  print(plot)
  ggsave(filename = file.path("plot output", file_name), plot = plot, width = width, height = height)
}
