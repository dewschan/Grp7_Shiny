#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(tsibble)
library(fabletools)
library(lubridate)
library(plotly)
library(readr)

# Load your pre-processed combined data RDS file
combined_data <- read_csv("data/SATH/combined_data.csv")

# UI
ui <- fluidPage(
  titlePanel("Dengue, Electricity, and Weather - Time Series Overview"),
  
  mainPanel(
    plotlyOutput("ts_overview_plot")
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive: preprocess + create tsibble
  tsibble_data <- reactive({
    combined_data %>%
      mutate(Date = yearmonth(MonthYear)) %>%
      group_by(Date) %>%
      summarise(
        denguecases = sum(denguecases, na.rm = TRUE),
        Electricity_KWh = sum(Electricity_KWh, na.rm = TRUE),
        AvgMeanTemp = mean(AvgMeanTemp, na.rm = TRUE),
        TotalDailyRain = sum(TotalDailyRain, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      as_tsibble(index = Date)
  })
  
  # Render Plot
  output$ts_overview_plot <- renderPlotly({
    ts_data <- tsibble_data()
    
    p <- ts_data %>%
      select(Date, denguecases, Electricity_KWh, AvgMeanTemp, TotalDailyRain) %>%
      pivot_longer(cols = -Date, names_to = "Variable", values_to = "Value") %>%
      ggplot(aes(x = Date, y = Value)) +
      geom_line(color = "#2c3e50") +
      facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
      labs(
        title = "Time Series of Dengue, Electricity, and Weather Variables",
        x = "Date", y = "Value"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the app
shinyApp(ui, server)