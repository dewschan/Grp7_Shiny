#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(ggstatsplot)
library(GGally)

# Load data
combined_monthly_data <- read_rds("data/rds/combined_Monthly_data.rds")
combined_Yearly_data <- read_rds("data/rds/combined_Yearly_data.rds")

# Define a mapping for variable names to descriptive labels
variable_labels <- list(
  denguecases = "Dengue Cases",
  AvgMeanTemp = "Average Mean Temperature (°C)",
  MaxTemp = "Maximum Temperature (°C)",
  MinTemp = "Minimum Temperature (°C)",
  total_rainfall = "Total Rainfall (mm)",
  Highest30minRainfall = "Highest 30-min Rainfall (mm)",
  Highest60minRainfall = "Highest 60-min Rainfall (mm)",
  Highest120minRainfall = "Highest 120-min Rainfall (mm)",
  DaysAbove35 = "Days Above 35°C",
  Monthly_Elec_consump = "Monthly Electricity Consumption (kWh)",
  Monthly_Elec_per_Household = "Electricity Consumption per Household (kWh)"
)

# Define UI
ui <- fluidPage(
  titlePanel("Visualizations"),
  tabsetPanel(id = "mainTabs",
              tabPanel(
                "Combined Plots",
                sidebarLayout(
                  sidebarPanel(
                    selectInput("variable", "Select Variable:",
                                choices = names(variable_labels),
                                selected = "total_rainfall"),
                    radioButtons("anovaType", "Select ANOVA Type:",
                                 choices = c("Parametric" = "p", "Non-Parametric" = "np"),
                                 selected = "np")
                  ),
                  mainPanel(
                    fluidRow(
                      column(6, plotOutput("histPlot")),
                      column(6, plotOutput("linePlot"))
                    ),
                    h3("ANOVA Results"),
                    plotOutput("anovaPlot")
                  )
                )
              ),
              tabPanel(
                "Correlation Plot",
                h3("Correlation Matrix"),
                uiOutput("corrVars"),
                plotOutput("corrPlot")
              ),
              tabPanel(
                "RUN MLR",
                h3("Multiple Linear Regression"),
                selectInput("dependent", "Select Dependent Variable (Y):",
                            choices = c("Dengue Cases" = "denguecases", "Electricity Consumption" = "Monthly_Elec_consump"),
                            selected = "denguecases"),
                uiOutput("independentVars"),
                actionButton("runMLR", "RUN REGRESSION"),
                verbatimTextOutput("mlrResults")
              )
  )
)

# Define Server
server <- function(input, output, session) {
  # Render histogram
  output$histPlot <- renderPlot({
    selected_label <- variable_labels[[input$variable]]
    
    ggplot(combined_monthly_data, aes_string(x = input$variable)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
      stat_function(fun = dnorm, args = list(mean = mean(combined_monthly_data[[input$variable]], na.rm = TRUE),
                                             sd = sd(combined_monthly_data[[input$variable]], na.rm = TRUE)),
                    color = "red", size = 1) +
      labs(title = "Histogram with Normal Curve", x = selected_label, y = "Density") +
      theme_minimal()
  })
  
  # Render line graph
  output$linePlot <- renderPlot({
    selected_label <- variable_labels[[input$variable]]
    
    ggplot(combined_Yearly_data, aes(x = Year, y = .data[[input$variable]])) +
      geom_point(alpha = 0.6, size = 3) +
      geom_smooth(method = "lm", linetype = "dashed", size = 1) +
      labs(
        title = paste("Yearly Trends in", selected_label),
        x = "Year",
        y = selected_label
      ) +
      theme_minimal()
  })
  
  # Render Anova plot
  output$anovaPlot <- renderPlot({
    selected_label <- variable_labels[[input$variable]]
    ggbetweenstats(
      data = combined_monthly_data,
      x = Year,
      y = !!input$variable,
      type = input$anovaType,
      mean.ci = TRUE,
      pairwise.comparisons = FALSE,
      pairwise.display = "none",
      messages = FALSE
    ) +
      labs(
        title = "Violin Plot with ANOVA Results",
        subtitle = paste("ANOVA Type:", ifelse(input$anovaType == "p", "Parametric", "Non-Parametric"))
      ) +
      theme_minimal() +
      guides(fill = "none", color = "none", shape = "none") +
      theme(legend.position = "none")
  })
  
  # Generate variable selection for correlation plot
  output$corrVars <- renderUI({
    checkboxGroupInput("corrVariables", "Select Variables for Correlation:",
                       choices = setdiff(names(combined_monthly_data), c("Month", "Year")),
                       selected = names(combined_monthly_data)[3:13])
  })
  
  # Render correlation plot
  output$corrPlot <- renderPlot({
    req(input$corrVariables)
    numeric_vars <- combined_monthly_data %>% select(all_of(input$corrVariables))
    ggcorr(numeric_vars, label = TRUE, label_size = 4, hjust = 0.75, size = 3) +
      labs(title = "Correlation Matrix") +
      theme_minimal()
  })
  
  # Dynamically generate independent variable checkboxes (excluding "Year" and "Month")
  output$independentVars <- renderUI({
    valid_vars <- setdiff(names(combined_monthly_data), c(input$dependent, "Year", "Month"))
    checkboxGroupInput("independent", "Select Independent Variables (X):",
                       choices = valid_vars,
                       selected = NULL)
  })
  
  # Perform Multiple Linear Regression
  observeEvent(input$runMLR, {
    req(input$independent)
    
    formula <- as.formula(paste(input$dependent, "~", paste(input$independent, collapse = "+")))
    model <- lm(formula, data = combined_monthly_data)
    
    output$mlrResults <- renderPrint({
      summary(model)
    })
  })
  
  # Switch to Combined Plots tab when variable changes
  observeEvent(input$variable, {
    updateTabsetPanel(session, "mainTabs", selected = "Combined Plots")
  })
  
  # Switch to Combined Plots tab when ANOVA type changes
  observeEvent(input$anovaType, {
    updateTabsetPanel(session, "mainTabs", selected = "Combined Plots")
  })
}

# Run the Application
shinyApp(ui = ui, server = server)



