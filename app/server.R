library(shiny)
library(tidyverse)
library(ggplot2)

server <- function(input, output, global) {
  
  output$correlation_plot <- renderPlot({
    ggplot(data=data, aes(x = .data[[input$var_x]], y = .data[[input$var_y]], color = as.factor(.data[[input$var_z]]))) +
      geom_point() +
      labs(x = input$var_x, y = input$var_y) +
      theme_bw()
  })
  
  output$correlation <- renderText({
    paste("Correlation coefficient:", round(cor(data[[input$var_x]], data[[input$var_y]])[1],2), sep=" ")
  }
  )
  output$data_head <- renderTable({data %>% group_by(sex) %>% summarize_all(mean)}, spacing = "xs")
}