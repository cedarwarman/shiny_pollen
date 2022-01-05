# Introduction ------------------------------------------------------------
library(shiny)
library(googlesheets4)
library(ggplot2)
library(dplyr)


# Functions ---------------------------------------------------------------
# To keep things neat, making some plotting functions up here
make_plot <- function(pollen_data, bench_num, color_vec, column_choice, bench_label){
  output_plot <- pollen_data %>%
    filter(bench == bench_num) %>%
    ggplot(aes(x, y,
               fill = .data[[column_choice]],
               label = paste0(accession, "\n", .data[[column_choice]]))) +
    geom_tile(aes(height = height), color = "black", size = 2) +
    geom_text(color = "black", fontface = "bold", size = 5) +
    geom_segment(aes(x = 0.5, y = 0.5, xend = 0.5, yend = 4.5), size = 2) +
    geom_segment(aes(x = 5.5, y = 0.5, xend = 5.5, yend = 4.5), size = 2) +
    annotate("text", x = 3, y = 2.85, label = bench_num, fontface = "bold", size = 25) +
    annotate("text", x = 3, y = 2.1, label = bench_label, fontface = "bold", size = 15) +
    scale_fill_gradient(low = color_vec[1],
                        high = color_vec[2],
                        na.value = color_vec[3],
                        lim = c(0, 7)) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(t = 0, r = 0, b = 30, l = 0))
  return(output_plot)
}


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$style(type='text/css', "label { font-size: 28px; 
                                                  line-height: 28px;
                                                  font-weight: bold; }
                               .selectize-input { font-size: 28px; 
                                                  line-height: 28px;
                                                  font-weight: bold; } 
                               .selectize-dropdown { font-size: 28px; 
                                                     line-height: 28px; 
                                                     font-weight: bold; }"
  ),
  fluidRow(
    column(12, align = "center", 
      selectInput("slider_choice", "Select data:", c("26 °C", "34 °C", "Flowers"))
    )
  ),
  fluidRow(
    plotOutput("plot_8")
  ),
  fluidRow(
    plotOutput("plot_7")
  ),
  fluidRow(
    plotOutput("plot_6")
  ),
  fluidRow(
    plotOutput("plot_5")
  ),
  fluidRow(
    plotOutput("plot_4")
  ),
  fluidRow(
    plotOutput("plot_3")
  ),
  fluidRow(
    plotOutput("plot_2")
  ),
  fluidRow(
    plotOutput("plot_1")
  )
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Figure out where to store credentials
  gs4_deauth()
  pollen_data_sheet <- read_sheet("15oanRivQrhWl0EFmv4zxZqsIB1pLp9InEP43pjqkfGs")
  
  # if statement for color_vec and column_choice based on slider input choices
  color_vec_value <- reactive({
    if (input$slider_choice == "26 °C"){
      color_vec_value <- c("white", "#3bceff", "green")
    }
    if (input$slider_choice == "34 °C"){
      color_vec_value <- c("white", "#ff00f7", "green")
    }
    if (input$slider_choice == "Flowers"){
      color_vec_value <- c("white", "#e5ff3b", "green")
    }
    return(color_vec_value)
  })
  column_choice_value <- reactive({
    if (input$slider_choice == "26 °C"){
      column_choice_value <- "good_run_count_26"
    }
    if (input$slider_choice == "34 °C"){
      column_choice_value <- "good_run_count_34"
    }
    if (input$slider_choice == "Flowers"){
      column_choice_value <- "flowers_measured"
    }
    return(column_choice_value)
  })
  
  output$plot_8 <- renderPlot({
    make_plot(pollen_data_sheet, 8, color_vec_value(), column_choice_value(), input$slider_choice)
  })
  output$plot_7 <- renderPlot({
    make_plot(pollen_data_sheet, 7, color_vec_value(), column_choice_value(), input$slider_choice)
  })
  output$plot_6 <- renderPlot({
    make_plot(pollen_data_sheet, 6, color_vec_value(), column_choice_value(), input$slider_choice)
  })
  output$plot_5 <- renderPlot({
    make_plot(pollen_data_sheet, 5, color_vec_value(), column_choice_value(), input$slider_choice)
  })
  output$plot_4 <- renderPlot({
    make_plot(pollen_data_sheet, 4, color_vec_value(), column_choice_value(), input$slider_choice)
  })
  output$plot_3 <- renderPlot({
    make_plot(pollen_data_sheet, 3, color_vec_value(), column_choice_value(), input$slider_choice)
  })
  output$plot_2 <- renderPlot({
    make_plot(pollen_data_sheet, 2, color_vec_value(), column_choice_value(), input$slider_choice)
  })
  output$plot_1 <- renderPlot({
    make_plot(pollen_data_sheet, 1, color_vec_value(), column_choice_value(), input$slider_choice)
  })
  
}

shinyApp(ui, server)