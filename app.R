#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load all libraries up front
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(ggstatsplot)
library(GGally)
library(tidyverse)
library(tsibble)
library(fabletools)
library(lubridate)
library(plotly)
library(sf)
library(sfdep)
library(tmap)
library(terra)
library(gstat)
library(automap)

# ==== Load Datasets ====
# EDA/CDA
combined_monthly_data <- read_rds("data/DS/rds/combined_Monthly_data.rds")
combined_Yearly_data <- read_rds("data/DS/rds/combined_Yearly_data.rds")

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

# Time Series
combined_data <- read_csv("data/SATH/combined_data.csv")

# Geospatial
weather <- read_rds("data/SS/rds/weather.rds")
mpsz2019 <- read_rds("data/SS/rds/mpsz.rds")

# ==== UI ====
ui <- dashboardPage(
  dashboardHeader(title = "Dengue & Climate Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("EDA & CDA", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Time Series", tabName = "ts", icon = icon("clock")),
      menuItem("Geospatial", tabName = "geo", icon = icon("globe"))
    )
  ),
  dashboardBody(
    tabItems(
      # ----- EDA & CDA -----
      tabItem(tabName = "eda",
              fluidPage(
                tabsetPanel(id = "mainTabs",
                            tabPanel("Combined Plots",
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
                            tabPanel("Correlation Plot",
                                     uiOutput("corrVars"),
                                     plotOutput("corrPlot")),
                            tabPanel("RUN MLR",
                                     selectInput("dependent", "Dependent Variable:",
                                                 choices = c("Dengue Cases" = "denguecases", "Electricity" = "Monthly_Elec_consump")),
                                     uiOutput("independentVars"),
                                     actionButton("runMLR", "RUN REGRESSION"),
                                     verbatimTextOutput("mlrResults"))
                )
              )),
      
      # ----- Time Series -----
      tabItem(tabName = "ts",
              fluidPage(
                plotlyOutput("ts_overview_plot")
              )),
      
      # ----- Geospatial -----
      tabItem(tabName = "geo",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectInput("geo_variable", "Select Variable:",
                                choices = c("Total Rainfall (mm)" = "MonthlyRainfall",
                                            "Mean Temperature (°C)" = "MonthlyMeanTemp",
                                            "Mean Wind Speed (km/h)" = "MonthlyMeanWindSpeed")),
                    selectInput("month_year", "Select Month-Year:",
                                choices = format(seq(as.Date("2021-01-01"), as.Date("2024-04-01"), by = "month"), "%b-%Y")),
                    selectInput("stat", "Select Statistic:",
                                choices = c("Local Moran I" = "ii", "P-value" = "p_ii", "Std Deviation" = "z_ii", "Variance" = "var_ii", "Expectation" = "eii")),
                    sliderInput("nsim", "Number of Simulations:", min = 99, max = 399, value = 99),
                    selectInput("lisa_class", "Select LISA Classification:",
                                choices = c("Mean" = "mean", "Median" = "median", "Pysal" = "pysal")),
                    actionButton("show_result", "Show Result")
                  ),
                  mainPanel(
                    fluidRow(
                      column(6, tmapOutput("stat_plot", height = "350px")),
                      column(6, tmapOutput("lisa_map", height = "350px"))
                    ),
                    fluidRow(
                      column(12, wellPanel(
                        h5("Interpretation of Maps:"),
                        p("The first map displays the selected statistic ..."),
                        p("The second map is the LISA map ...")
                      ))
                    )
                  )
                )
              ))
    )
  )
)

# ==== SERVER ====
server <- function(input, output, session) {
  # ---------------- EDA/CDA LOGIC ----------------
  
  output$histPlot <- renderPlot({
    ggplot(combined_monthly_data, aes_string(x = input$variable)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
      stat_function(fun = dnorm,
                    args = list(mean = mean(combined_monthly_data[[input$variable]], na.rm = TRUE),
                                sd = sd(combined_monthly_data[[input$variable]], na.rm = TRUE)),
                    color = "red") +
      labs(x = variable_labels[[input$variable]], y = "Density") +
      theme_minimal()
  })
  
  output$linePlot <- renderPlot({
    ggplot(combined_Yearly_data, aes(x = Year, y = .data[[input$variable]])) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(x = "Year", y = variable_labels[[input$variable]]) +
      theme_minimal()
  })
  
  output$anovaPlot <- renderPlot({
    ggbetweenstats(
      data = combined_monthly_data,
      x = Year,
      y = !!input$variable,
      type = input$anovaType,
      pairwise.display = "none",
      messages = FALSE
    ) + theme_minimal()
  })
  
  output$corrVars <- renderUI({
    checkboxGroupInput("corrVariables", "Select Variables:",
                       choices = setdiff(names(combined_monthly_data), c("Month", "Year")),
                       selected = names(combined_monthly_data)[3:13])
  })
  
  output$corrPlot <- renderPlot({
    req(input$corrVariables)
    ggcorr(combined_monthly_data %>% select(all_of(input$corrVariables)), label = TRUE)
  })
  
  output$independentVars <- renderUI({
    checkboxGroupInput("independent", "Independent Variables:",
                       choices = setdiff(names(combined_monthly_data), c(input$dependent, "Year", "Month")))
  })
  
  observeEvent(input$runMLR, {
    req(input$independent)
    model <- lm(as.formula(paste(input$dependent, "~", paste(input$independent, collapse = "+"))),
                data = combined_monthly_data)
    output$mlrResults <- renderPrint({ summary(model) })
  })
  
  # ---------------- TIME SERIES ----------------
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
  
  output$ts_overview_plot <- renderPlotly({
    ts_data <- tsibble_data()
    p <- ts_data %>%
      pivot_longer(-Date, names_to = "Variable", values_to = "Value") %>%
      ggplot(aes(x = Date, y = Value)) +
      geom_line() +
      facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
      theme_minimal()
    ggplotly(p)
  })
  
  # ---------------- GEOSPATIAL ----------------
  filtered_data <- eventReactive(input$show_result, {
    weather_month <- weather %>%
      filter(format(as.Date(paste(Year, Month, "01", sep = "-")), "%b-%Y") == input$month_year)
    
    weather_aggregated <- weather_month %>%
      group_by(Station) %>%
      summarise(
        MonthlyRainfall = sum(DailyRainfall, na.rm = TRUE),
        MonthlyMeanTemp = mean(MeanTemperature, na.rm = TRUE),
        MonthlyMeanWindSpeed = mean(MeanWindSpeed, na.rm = TRUE),
        .groups = "drop"
      )
    
    weather_sf <- left_join(weather_aggregated, mpsz2019, by = "Station") %>% st_as_sf()
    formula <- as.formula(paste(input$geo_variable, "~ 1"))
    kriging_result <- krige(formula, weather_sf, mpsz2019, model = vgm("Exp"))
    kriged_sf <- st_as_sf(kriging_result)
    
    knn <- st_knn(st_centroid(mpsz2019), k = 3)
    weights <- st_weights(knn, style = "W")
    
    local_moran_result <- local_moran(as.numeric(kriged_sf$var1.pred), nb = knn, wt = weights, nsim = input$nsim)
    kriged_sf <- bind_cols(kriged_sf, as.data.frame(local_moran_result))
    return(kriged_sf)
  })
  
  output$stat_plot <- renderTmap({
    req(filtered_data())
    tm_shape(filtered_data()) +
      tm_fill(input$stat, palette = "blues") +
      tm_borders() +
      tm_layout(legend.outside = TRUE)
  })
  
  output$lisa_map <- renderTmap({
    req(filtered_data())
    selected_class <- input$lisa_class
    data <- filtered_data()
    data[[selected_class]] <- as.factor(data[[selected_class]])
    tm_shape(data) +
      tm_fill(selected_class, palette = "Set2", title = "LISA Classification") +
      tm_borders()
  })
}

# ==== Run the App ====
shinyApp(ui, server)
