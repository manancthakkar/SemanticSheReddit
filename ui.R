library(shiny)

ui <- fluidPage(
  
  titlePanel(title = "Interactive correlation plots"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(inputId = "var_x",
                  label = "Select X:",
                  choices = colnames(data)[-2]),
      
      selectInput(inputId = "var_y",
                  label = "Select Y:",
                  choices = rev(colnames(data)[-2])),
      
      selectInput(inputId = "var_z",
                  label = "Select variable for color:",
                  choices = colnames(data)[-1]),
    ),
    mainPanel(
      
      plotOutput("correlation_plot"),
      
      tableOutput("data_head"),
      
      textOutput("correlation")
      
      
    )
  )
)