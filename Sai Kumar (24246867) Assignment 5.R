# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Load the dataset
dig_data <- read.csv("DIG.csv")

# Define UI
ui <- fluidPage(
  titlePanel("DIG Trial Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Select X-axis Variable:", 
                  choices = names(dig_data), selected = "AGE"),
      selectInput("yvar", "Select Y-axis Variable:", 
                  choices = names(dig_data), selected = "BMI"),
      selectInput("groupvar", "Group by Variable:", 
                  choices = c("None", "TRTMT", "SEX", "RACE"), selected = "TRTMT"),
      checkboxInput("show_summary", "Show Summary Statistics", value = TRUE)
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      tableOutput("summaryTable")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Generate scatter plot
  output$scatterPlot <- renderPlot({
    plot_data <- dig_data %>%
      filter(!is.na(.data[[input$xvar]]), !is.na(.data[[input$yvar]]))
    
    ggplot(plot_data, aes_string(x = input$xvar, y = input$yvar, color = input$groupvar)) +
      geom_point(alpha = 0.7) +
      theme_minimal() +
      labs(title = "Scatter Plot", x = input$xvar, y = input$yvar)
  })
  
  # Generate summary table
  output$summaryTable <- renderTable({
    if (input$show_summary) {
      dig_data %>%
        select(.data[[input$xvar]], .data[[input$yvar]]) %>%
        summarise_all(list(mean = ~mean(., na.rm = TRUE),
                           median = ~median(., na.rm = TRUE),
                           sd = ~sd(., na.rm = TRUE))) %>%
        as.data.frame()
    }
  })
}


