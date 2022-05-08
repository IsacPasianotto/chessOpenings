library(shiny)
library(shinyjs)
library(rchess)
library(stringr)
library(tools)
library(ggplot2)



# Black - White(Draw) - (White) - extremly Dark (texts in plots) - title color - plot background
my_cols <- c('#04945f', '#047f5a42', '#74cdcb', '#004300', '#047f5a', '#b5d9b522')

my_theme <- function() {
  theme(
    plot.background = element_rect(fill = my_cols[6], color = my_cols[6]),
    panel.background = element_rect(fill = my_cols[6], color = my_cols[6]),
    legend.background = element_rect(fill = my_cols[6], color = my_cols[6]),
    plot.title = element_text(colour = my_cols[5], size = 16, face = 'bold.italic'),
    axis.title = element_text(colour = my_cols[4], size = 12, face = 'bold'),
    axis.text = element_text(colour = my_cols[4], size = 11, face = 'plain'),
    legend.text = element_text(color = my_cols[4], size = 11, face = "plain"),
    legend.title = element_text(color = my_cols[4], size = 12, face = "italic"),
    legend.position = "bottom",
    # ...
  )
}
