# Introduction ------------------------------------------------------------
library(shiny)
library(googlesheets4)
library(ggplot2)
library(ggnewscale)
library(dplyr)
library(bslib)


# Functions ---------------------------------------------------------------
# To keep things neat, making some plotting functions up here
make_plot <- function(pollen_data, bench_num, color_vec, column_choice, bench_label, target_num){
  if (nrow(pollen_data %>% filter(bench == bench_num)) == 0){
    output_plot <- ggplot() +
      geom_segment(aes(x = 0.5, y = 0.5, xend = 0.5, yend = 4.5), size = 2) +
      geom_segment(aes(x = 5.5, y = 0.5, xend = 5.5, yend = 4.5), size = 2) +
      geom_segment(aes(x = 0.5, y = 0.5, xend = 5.5, yend = 0.5), size = 2) +
      geom_segment(aes(x = 0.5, y = 4.5, xend = 5.5, yend = 4.5), size = 2) +
      annotate("text", x = 3, y = 2.85, label = bench_num, fontface = "bold", size = 25) +
      annotate("text", x = 3, y = 2.1, label = bench_label, fontface = "bold", size = 15) +
      coord_fixed() +
      theme_void() +
      theme(legend.position = "none",
            plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
  } else if (column_choice == "frozen_pollen"){
  output_plot <- pollen_data %>%
    filter(bench == bench_num) %>%
    ggplot(aes(x, y,
               fill = .data[[column_choice]],
               label = paste0(accession, "\n", .data[[column_choice]]))) +
    geom_tile(aes(height = height), color = "black", size = 2) +
    scale_fill_gradient(low = color_vec[1],
                    high = color_vec[2],
                    na.value = color_vec[3],
                    lim = c(0, target_num - 1)) +
    new_scale_fill() +
    geom_point(aes(x - 0.36, 
                   y + 0.36, 
                   shape = ready_for_frozen_pollen, 
                   fill = ready_for_frozen_pollen,
                   size = ready_for_frozen_pollen), 
               stroke = 2) +
    scale_shape_manual(values = c(NA, 23, 21)) +
    scale_fill_manual(values = c(NA, "yellow", "green")) +
    scale_size_manual(values = c(NA, 2.2, 3)) +
    # Crazy that this works
    {if(nrow(pollen_data %>% filter(bench == bench_num) %>% filter(plant_removed == "plant_removed")))
      geom_segment(data = pollen_data %>% filter(bench == bench_num) %>% filter(plant_removed == "plant_removed"),
                   aes(x = x - 0.45, y = y - 0.45, xend = x + 0.45, yend = y + 0.45),
                   size = 2,
                   color = "red")} +
    {if(nrow(pollen_data %>% filter(bench == bench_num) %>% filter(plant_removed == "plant_removed")))
      geom_segment(data = pollen_data %>% filter(bench == bench_num) %>% filter(plant_removed == "plant_removed"),
                   aes(x = x - 0.45, y = y + 0.45, xend = x + 0.45, yend = y - 0.45),
                   size = 2,
                   color = "red")} +
    geom_text(color = "black", fontface = "bold", size = 5) +
    geom_segment(aes(x = 0.5, y = 0.5, xend = 0.5, yend = 4.5), size = 2) +
    geom_segment(aes(x = 5.5, y = 0.5, xend = 5.5, yend = 4.5), size = 2) +
    geom_segment(aes(x = 0.5, y = 0.5, xend = 5.5, yend = 0.5), size = 2) +
    geom_segment(aes(x = 0.5, y = 4.5, xend = 5.5, yend = 4.5), size = 2) +
    annotate("text", x = 3, y = 2.85, label = bench_num, fontface = "bold", size = 25) +
    annotate("text", x = 3, y = 2.1, label = bench_label, fontface = "bold", size = 15) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
  } else {
  output_plot <- pollen_data %>%
    filter(bench == bench_num) %>%
    ggplot(aes(x, y,
               fill = .data[[column_choice]],
               label = paste0(accession, "\n", .data[[column_choice]]))) +
    geom_tile(aes(height = height), color = "black", size = 2) +
    geom_text(color = "black", fontface = "bold", size = 5) +
    geom_segment(aes(x = 0.5, y = 0.5, xend = 0.5, yend = 4.5), size = 2) +
    geom_segment(aes(x = 5.5, y = 0.5, xend = 5.5, yend = 4.5), size = 2) +
    geom_segment(aes(x = 0.5, y = 0.5, xend = 5.5, yend = 0.5), size = 2) +
    geom_segment(aes(x = 0.5, y = 4.5, xend = 5.5, yend = 4.5), size = 2) +
    annotate("text", x = 3, y = 2.85, label = bench_num, fontface = "bold", size = 25) +
    annotate("text", x = 3, y = 2.1, label = bench_label, fontface = "bold", size = 15) +
    scale_fill_gradient(low = color_vec[1],
                        high = color_vec[2],
                        na.value = color_vec[3],
                        lim = c(0, target_num - 1)) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
  }
  return(output_plot)
}


# UI ----------------------------------------------------------------------
# Making the input dropdown a little better and fixing the spacing
ui <- bootstrapPage(
  tags$style(type='text/css', "label { font-size: 28px;
                                                line-height: 28px;
                                                font-weight: bold;
                                                margin-top: 20px; }
                             .selectize-input { font-size: 28px;
                                                line-height: 40px;
                                                font-weight: bold;
                                                vertical-align: middle;
                                                text-align: center }
                             .selectize-dropdown { font-size: 28px;
                                                   line-height: 40px;
                                                   font-weight: bold;
                                                   vertical-align: middle;
                                                   text-align: center}"),
  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
  theme  = bs_theme(version = 5),
  div(class = "container-fluid",
      div(class = "row justify-content-center",
          align = "center",
          selectInput("slider_choice", "Select data:", c("26 °C", "34 °C", "Flowers", "Freezer"))
      ),
      div(class = "row justify-content-center",
          align = "center",
          div(class = "col-xl-4",
              h1(strong("West")),
              plotOutput("plot_1", height = "300px"),
              plotOutput("plot_3", height = "300px"),
              plotOutput("plot_5", height = "300px"),
              plotOutput("plot_7", height = "300px")
          ),
          div(class = "col-xl-4",
              h1(strong("East")),
              plotOutput("plot_2", height = "300px"),
              plotOutput("plot_4", height = "300px"),
              plotOutput("plot_6", height = "300px"),
              plotOutput("plot_8", height = "300px")
           )
      )
  )
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Figure out where to store credentials
  gs4_deauth()
  
  # Real sheet
  pollen_data_sheet <- read_sheet("15oanRivQrhWl0EFmv4zxZqsIB1pLp9InEP43pjqkfGs")
  
  # Test sheet. This should be commented out
  # pollen_data_sheet <- read_sheet("1b2TgPBwmNqq-RkeSDeP4nS60JQ7QZCPP8G4u6s98d1c")
  
  # if statements for color_vec, column_choice, target_num based on slider 
  # input choices
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
    if (input$slider_choice == "Freezer"){
      color_vec_value <- c("white", "#ff789e", "green")
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
    if (input$slider_choice == "Freezer"){
      column_choice_value <- "frozen_pollen"
    }
    return(column_choice_value)
  })
  target_num <- reactive({
    if (input$slider_choice == "26 °C"){
      target_num <- 8
    }
    if (input$slider_choice == "34 °C"){
      target_num <- 8
    }
    if (input$slider_choice == "Flowers"){
      target_num <- 12
    }
    if (input$slider_choice == "Freezer"){
      target_num <- 4
    }
    return(target_num)
  })
  
  # Making the plots
  output$plot_8 <- renderPlot({
    make_plot(pollen_data_sheet, 8, color_vec_value(), column_choice_value(), input$slider_choice, target_num())
  })
  output$plot_7 <- renderPlot({
    make_plot(pollen_data_sheet, 7, color_vec_value(), column_choice_value(), input$slider_choice, target_num())
  })
  output$plot_6 <- renderPlot({
    make_plot(pollen_data_sheet, 6, color_vec_value(), column_choice_value(), input$slider_choice, target_num())
  })
  output$plot_5 <- renderPlot({
    make_plot(pollen_data_sheet, 5, color_vec_value(), column_choice_value(), input$slider_choice, target_num())
  })
  output$plot_4 <- renderPlot({
    make_plot(pollen_data_sheet, 4, color_vec_value(), column_choice_value(), input$slider_choice, target_num())
  })
  output$plot_3 <- renderPlot({
    make_plot(pollen_data_sheet, 3, color_vec_value(), column_choice_value(), input$slider_choice, target_num())
  })
  output$plot_2 <- renderPlot({
    make_plot(pollen_data_sheet, 2, color_vec_value(), column_choice_value(), input$slider_choice, target_num())
  })
  output$plot_1 <- renderPlot({
    make_plot(pollen_data_sheet, 1, color_vec_value(), column_choice_value(), input$slider_choice, target_num())
  })
  
}

shinyApp(ui, server)