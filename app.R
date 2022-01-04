# Introduction ------------------------------------------------------------
library(shiny)
library(googlesheets4)
library(ggplot2)
library(dplyr)


# Functions ---------------------------------------------------------------
# To keep things neat, making some plotting functions up here
# Add in color variable here too 
make_plot <- function(pollen_data, bench_num){
  output_plot <- pollen_data %>%
    filter(bench == bench_num) %>%
    ggplot(aes(x, y,
               fill = good_run_count_34,
               label = paste0(accession, "\n", good_run_count_34))) +
    geom_tile(aes(height = height), color = "black", size = 2) +
    geom_text(color = "black", fontface = "bold", size = 5) +
    scale_fill_gradient(low = "white",
                        high = "#ff00f7",
                        na.value = "green",
                        lim = c(min(pollen_data$good_run_count_34), 7)) +
    theme_void() +
    theme(legend.position = "none")
  return(output_plot)
}


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  plotOutput("plot")
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Figure out where to store credentials
  gs4_deauth()
  pollen_data_sheet <- read_sheet("15oanRivQrhWl0EFmv4zxZqsIB1pLp9InEP43pjqkfGs")
  output$plot <- renderPlot({
    make_plot(pollen_data_sheet, 5)
  })
}

shinyApp(ui, server)