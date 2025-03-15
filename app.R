#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

combined_data <- read_csv("data/combined_data.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("Dengue Cases and Temperature Data"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = unique(combined_data$Year)),
      selectInput("temp_var", "Select Temperature Variable:", 
                  choices = list("AvgMeanTemp" = "AvgMeanTemp", 
                                 "MaxTemp" = "MaxTemp", 
                                 "MinTemp" = "MinTemp"))
    ),
    
    mainPanel(
      plotOutput("data_plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  filtered_data <- reactive({
    combined_data %>%
      filter(Year == input$year)
  })
  
  output$data_plot <- renderPlot({
    temp_var <- input$temp_var
    
    # Calculate scaling factor for temperature
    scaling_factor <- (8200 - 100) / (40 - 20)
    
    ggplot(filtered_data(), aes(x = Month)) +
      geom_line(aes(y = denguecases, color = "Dengue Cases")) +
      geom_line(aes(y = ((!!sym(temp_var) - 20) * scaling_factor) + 100, color = "Temperature Variable")) +
      labs(title = paste("Dengue Cases and Temperature Data for Year", input$year),
           x = "Month", y = "Dengue Cases", color = "Legend") +
      scale_color_manual(values = c("Dengue Cases" = "blue", "Temperature Variable" = "red")) +
      theme_minimal() +
      scale_y_continuous(
        name = "Dengue Cases", 
        limits = c(100, 8200),
        sec.axis = sec_axis(~ (. - 100) / scaling_factor + 20, name = "Temperature", breaks = seq(20, 40, by = 2))
      ) +
      scale_x_continuous(breaks = 1:12, limits = c(1, 12))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
