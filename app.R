#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load all libraries up front
# ==== Load Libraries ====
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(ggstatsplot)
library(GGally)
library(ggExtra)
library(tidyverse)
library(tsibble)
library(fabletools)
library(lubridate)
library(feasts)
library(fable)
library(plotly)
library(sf)
library(sfdep)
library(tmap)
library(terra)
library(gstat)
library(automap)



# ==== Load Datasets ====
combined_monthly_data <- read_rds("data/DS/rds/combined_Monthly_data.rds")
combined_Yearly_data <- read_rds("data/DS/rds/combined_Yearly_data.rds")
combined_data <- read_csv("data/SATH/combined_data.csv", show_col_types = FALSE)
weather_filtered <- read_rds("data/SS/rds/weather_filtered.rds")
mpsz2019 <- read_rds("data/SS/rds/mpsz2019.rds")
weather <- read_rds("data/SS/rds/weather.rds")
mpsz <- read_rds("data/SS/rds/mpsz.rds")

variable_labels <- list(
  denguecases = "Dengue Cases",
  AvgMeanTemp = "Average Mean Temperature (\u00b0C)",
  MaxTemp = "Maximum Temperature (\u00b0C)",
  MinTemp = "Minimum Temperature (\u00b0C)",
  total_rainfall = "Total Rainfall (mm)",
  Highest30minRainfall = "Highest 30-min Rainfall (mm)",
  Highest60minRainfall = "Highest 60-min Rainfall (mm)",
  Highest120minRainfall = "Highest 120-min Rainfall (mm)",
  DaysAbove35 = "Days Above 35\u00b0C",
  Elec_consump = "Electricity Consumption (kWh)",
  Elec_per_Household = "Electricity Consumption per Household (kWh)"
)
allowed_scatter_vars <- setdiff(names(combined_monthly_data), c("Year", "Month"))
combined_data <- read_csv("data/SATH/combined_data.csv")
weather_filtered <- read_rds("data/SS/rds/weather_filtered.rds")
mpsz2019 <- read_rds("data/SS/rds/mpsz2019.rds")
weather <- read_rds("data/SS/rds/weather.rds")
mpsz <- read_rds("data/SS/rds/mpsz.rds")

# ==== UI ====
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "SG ClimateViz"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("EDA & CDA", tabName = "eda", icon = icon("chart-bar")),
      menuItem("Time Series Forecasting", tabName = "ts", icon = icon("clock")),
      menuItem("Geospatial Analysis", tabName = "geo", icon = icon("globe"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # ----- EDA & CDA -----
      tabItem(tabName = "eda",
              fluidPage(
                titlePanel("Visualizations"),
                tabsetPanel(id = "mainTabs",
                            
                            tabPanel("Combined Plots",
                                     fluidRow(
                                       column(12, p("📊 Select the variables of interest. The histogram shows the distribution, the line graph displays the yearly trend, and the ANOVA plot provides statistical test results."))
                                     ),
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
                                     fluidRow(
                                       column(12, p("🔗 Select variables to explore correlations. Crosses (X) indicate *non-significant* correlations."))
                                     ),
                                     h3("Correlation Matrix"),
                                     uiOutput("corrVars"),
                                     plotOutput("corrPlot")
                            ),
                            
                            tabPanel("RUN MLR",
                                     fluidRow(
                                       column(12, p("📈 Build a Multiple Linear Regression (MLR) model by selecting a dependent variable (Y) and one or more independent variables (X)."))
                                     ),
                                     h3("Multiple Linear Regression"),
                                     selectInput("dependent", "Select Dependent Variable (Y):",
                                                 choices = c("Dengue Cases" = "denguecases",
                                                             "Electricity Consumption" = "Monthly_Elec_consump"),
                                                 selected = "denguecases"),
                                     uiOutput("independentVars"),
                                     actionButton("runMLR", "RUN REGRESSION"),
                                     verbatimTextOutput("mlrResults")
                            ),
                            
                            tabPanel("Scatter with Marginals",
                                     fluidRow(
                                       column(12, p("📌 Based on the results from MLR, select two variables of significance to visualize their relationship in a scatter plot, along with marginal histograms or boxplots."))
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("xvar", "Select X Variable:",
                                                     choices = allowed_scatter_vars,
                                                     selected = allowed_scatter_vars[1]),
                                         selectInput("yvar", "Select Y Variable:",
                                                     choices = allowed_scatter_vars,
                                                     selected = allowed_scatter_vars[2]),
                                         radioButtons("marginalType", "Marginal Plot Type:",
                                                      choices = c("Histogram" = "histogram",
                                                                  "Boxplot" = "boxplot"),
                                                      selected = "histogram")
                                       ),
                                       mainPanel(
                                         plotOutput("scatterMarginalPlot")
                                       )
                                     )
                            )
                )
              )
      ),
      
      # ----- Time Series -----
      tabItem(tabName = "ts",
              fluidPage(
                theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
                titlePanel("Dengue, Electricity & Weather Dashboard"),
                
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("year_range", "Select Year Range:",
                                min = 2013, max = 2025,
                                value = c(2013, 2025), step = 1,
                                sep = "", ticks = FALSE)
                  ),
                  mainPanel(
                    column(
                      width = 12,
                      tabsetPanel(
                        tabPanel("Time Series Overview", plotlyOutput("ts_overview_plot", height = "1000px", width = "100%")),
                        tabPanel("Seasonality",
                                 plotlyOutput("seasonal_plot", height = "1000px", width = "100%"),
                                 plotlyOutput("subseries_plot", height = "900px"),
                                 plotlyOutput("stl_weather_plot", height = "900px")),
                        tabPanel("Cross-Correlation Analysis",
                                 h3("Dengue vs Total Rainfall"),
                                 plotlyOutput("ccf_rainfall"),
                                 h3("Dengue vs Avg Temp"),
                                 plotlyOutput("ccf_temperature")),
                        tabPanel("Dengue Analysis",
                                 h3("ACF & PACF"),
                                 plotlyOutput("acf_dengue"),
                                 plotlyOutput("pacf_dengue"),
                                 h3("STL Decomposition"),
                                 plotlyOutput("stl_dengue"),
                                 h3("Classical Decomposition"),
                                 plotlyOutput("classical_dengue"),
                                 h3("ETS Forecast"),
                                 plotlyOutput("ets_dengue"),
                                 h3("ETS vs ARIMA Forecast Comparison"),
                                 plotlyOutput("compare_dengue")),
                        tabPanel("Temperature Analysis",
                                 h3("ACF & PACF"),
                                 plotlyOutput("acf_temp"),
                                 plotlyOutput("pacf_temp"),
                                 h3("STL Decomposition"),
                                 plotlyOutput("stl_temp"),
                                 h3("Classical Decomposition"),
                                 plotlyOutput("classical_temp"),
                                 h3("ETS Forecast"),
                                 plotlyOutput("ets_temp"),
                                 h3("ETS vs ARIMA Forecast Comparison"),
                                 plotlyOutput("compare_temp")),
                        tabPanel("Rainfall Analysis",
                                 h3("ACF & PACF"),
                                 plotlyOutput("acf_rain"),
                                 plotlyOutput("pacf_rain"),
                                 h3("STL Decomposition"),
                                 plotlyOutput("stl_rain"),
                                 h3("Classical Decomposition"),
                                 plotlyOutput("classical_rain"),
                                 h3("ETS Forecast"),
                                 plotlyOutput("ets_rain"),
                                 h3("ETS vs ARIMA Forecast Comparison"),
                                 plotlyOutput("compare_rain")),
                        tabPanel("Electricity Analysis",
                                 h3("ACF & PACF"),
                                 plotlyOutput("acf_elec"),
                                 plotlyOutput("pacf_elec"),
                                 h3("STL Decomposition"),
                                 plotlyOutput("stl_elec"),
                                 h3("Classical Decomposition"),
                                 plotlyOutput("classical_elec"),
                                 h3("ETS Forecast"),
                                 plotlyOutput("ets_elec"),
                                 h3("ETS vs ARIMA Forecast Comparison"),
                                 plotlyOutput("compare_elec")),
                        tabPanel("Weather Forecast Summary",
                                 h3("ETS Forecast for Weather & Electricity (Next 12 Months)"),
                                 plotlyOutput("weather_forecast_summary")),
                        tabPanel("Forecast Accuracy Comparison",
                                 h3("ETS vs ARIMA Forecast Accuracy: Rainfall & Electricity"),
                                 plotlyOutput("forecast_accuracy_plot"))
                      )
                    )
                  )
                ))
      ),
      
      # ----- Geospatial -----
      tabItem(tabName = "geo",
              fluidPage(
                titlePanel("Geospatial Analysis"),
                tabsetPanel(
                  
                  tabPanel("Distributions after Spatial Interpolation",
                           fluidRow(
                             column(4,
                                    selectInput("spatial_variable", "Select Variable:",
                                                choices = c("Total Rainfall (mm)" = "MonthlyRainfall",
                                                            "Mean Temperature (°C)" = "MonthlyMeanTemp",
                                                            "Mean Wind Speed (km/h)" = "MonthlyMeanWindSpeed"),
                                                selected = "MonthlyMeanTemp"),
                                    selectInput("month_year", "Select Month-Year:",
                                                choices = format(seq(as.Date("2021-01-01"),
                                                                     as.Date("2024-04-01"),
                                                                     by = "month"),
                                                                 "%b-%Y"),
                                                selected = "Jan-2021"),
                                    tabsetPanel(
                                      id = "tabset_variogram",
                                      tabPanel("Automatic Variogram", p("Auto-fitted variogram automatically determines the optimal variogram model and parameters (psill, range, nugget) based on the data, so users don't need to manually input these values.")),
                                      tabPanel("Manual Adjustment",
                                               sliderInput("psill", "Psill:", min = 0, max = 5, value = 0.5),
                                               selectInput("model", "Model Type:",
                                                           choices = c("Spherical" = "Sph", "Exponential" = "Exp", "Gaussian" = "Gau"),
                                                           selected = "Sph"),
                                               sliderInput("range", "Range:", min = 100, max = 10000, value = 5000),
                                               sliderInput("nugget", "Nugget:", min = 0, max = 1, value = 0.1)
                                      )
                                    ),
                                    actionButton("show_result", "Show Result")
                             ),
                             column(8,
                                    fluidRow(
                                      column(6, wellPanel(
                                        style = "border: 4px solid #004aad; padding: 5px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                                        tmapOutput("mean_temp_plot")
                                      )),
                                      column(6, wellPanel(
                                        style = "border: 4px solid #004aad; padding: 5px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                                        tmapOutput("variance_plot")
                                      ))
                                    )
                             )
                           ),
                           fluidRow(
                             column(12, wellPanel(
                               h4("Introduction to Spatial Interpolation"),
                               p("Spatial interpolation is the process of predicting values for unmeasured locations based on known values from nearby locations."),
                               p("Kriging is a method of spatial interpolation that uses statistical models to predict the value of a variable at unmeasured points, taking into account the spatial correlation between data points."),
                               h4("How to Interpret the Plots"),
                               p("The first plot represents the predicted values (e.g., temperature, rainfall, or wind speed) for the selected variable at different locations."),
                               p("The second plot shows the Kriging variance, which represents the uncertainty of the predictions. Higher variance indicates less confidence in the prediction.")
                             ))
                           )
                  ),
                  
                  tabPanel("Local Measure of Spatial Autocorrelation",
                           fluidRow(
                             column(4,
                                    selectInput("stat_variable", "Select Variable:",
                                                choices = c("Total Rainfall (mm)" = "MonthlyRainfall",
                                                            "Mean Temperature (°C)" = "MonthlyMeanTemp",
                                                            "Mean Wind Speed (km/h)" = "MonthlyMeanWindSpeed"),
                                                selected = "MonthlyMeanTemp"),
                                    selectInput("stat_month_year", "Select Month-Year:",
                                                choices = format(seq(as.Date("2021-01-01"),
                                                                     as.Date("2024-04-01"),
                                                                     by = "month"),
                                                                 "%b-%Y"),
                                                selected = "Jan-2021"),
                                    selectInput("stat", "Select Statistic:",
                                                choices = c("Local Moran I" = "ii",
                                                            "P-value" = "p_ii",
                                                            "Std Deviation" = "z_ii",
                                                            "Variance" = "var_ii",
                                                            "Expectation" = "eii"),
                                                selected = "ii"),
                                    sliderInput("nsim", "Number of Simulations:", min = 99, max = 399, value = 99, step = 1),
                                    selectInput("lisa_class", "Select LISA Classification:",
                                                choices = c("Mean" = "mean", "Median" = "median", "Pysal" = "pysal"),
                                                selected = "mean")
                             ),
                             column(4,
                                    wellPanel(
                                      style = "border: 4px solid #004aad; padding: 5px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                                      tmapOutput("stat_plot", height = "350px")
                                    )
                             ),
                             column(4,
                                    wellPanel(
                                      style = "border: 4px solid #004aad; padding: 5px; box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);",
                                      tmapOutput("lisa_map", height = "350px")
                                    )
                             )
                           ),
                           fluidRow(
                             column(12, wellPanel(
                               h5("Interpretation of Maps:"),
                               p("The first map displays the selected statistic (e.g., Local Moran's I, P-value, Std Deviation, etc.) for the chosen variable (e.g., Total Rainfall, Mean Temperature, Mean Wind Speed). Each color in the map represents the value of the statistic for a particular geographic region, where darker or lighter colors indicate higher or lower values of the statistic, respectively."),
                               p("The second map is the LISA (Local Indicators of Spatial Association) map, which shows significant clusters of similar values for the selected variable. LISA highlights areas where high values are clustered together, and similarly, low values are grouped in specific locations.")
                             ))
                           )
                  )
                )
              )
      )
    )
  )
)



# ==== SERVER ====
server <- function(input, output, session) {
  # ---------------- EDA/CDA LOGIC ----------------
  # Render histogram
  output$histPlot <- renderPlot({
    selected_label <- variable_labels[[input$variable]]
    
    ggplot(combined_monthly_data, aes_string(x = input$variable)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.7) +
      stat_function(fun = dnorm, args = list(
        mean = mean(combined_monthly_data[[input$variable]], na.rm = TRUE),
        sd = sd(combined_monthly_data[[input$variable]], na.rm = TRUE)
      ), color = "red", size = 1) +
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
  
  # Render ANOVA plot
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
  
  # Render correlation UI
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
  
  # Render independent variable selector
  output$independentVars <- renderUI({
    valid_vars <- setdiff(names(combined_monthly_data), c(input$dependent, "Year", "Month"))
    checkboxGroupInput("independent", "Select Independent Variables (X):",
                       choices = valid_vars,
                       selected = NULL)
  })
  
  # Run MLR
  observeEvent(input$runMLR, {
    req(input$independent)
    
    formula <- as.formula(paste(input$dependent, "~", paste(input$independent, collapse = "+")))
    model <- lm(formula, data = combined_monthly_data)
    
    output$mlrResults <- renderPrint({
      summary(model)
    })
  })
  
  # Render Scatter Marginal Plot
  output$scatterMarginalPlot <- renderPlot({
    req(input$xvar, input$yvar)
    
    base_plot <- ggplot(combined_monthly_data, aes(x = .data[[input$xvar]], y = .data[[input$yvar]])) +
      geom_point(color = "steelblue", alpha = 0.6) +
      theme_minimal()
    
    ggMarginal(
      base_plot,
      type = input$marginalType,
      margins = "both",
      xparams = list(fill = "steelblue", color = "steelblue", alpha = 0.5),
      yparams = list(fill = "steelblue", color = "steelblue", alpha = 0.5)
    )
  })
  
  # Auto-switch back to Combined Plots when inputs change
  observeEvent(input$variable, {
    updateTabsetPanel(session, "mainTabs", selected = "Combined Plots")
  })
  
  observeEvent(input$anovaType, {
    updateTabsetPanel(session, "mainTabs", selected = "Combined Plots")
  })
  
  # ---------------- TIME SERIES ----------------
  tsibble_data <- reactive({
    yr_range <- input$year_range
    
    combined_data %>%
      mutate(Date = yearmonth(MonthYear)) %>%
      filter(year(Date) >= yr_range[1], year(Date) <= yr_range[2]) %>%
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
      pivot_longer(cols = c(denguecases, Electricity_KWh, AvgMeanTemp, TotalDailyRain),
                   names_to = "Variable", values_to = "Value") %>%
      ggplot(aes(x = Date, y = Value)) +
      geom_line(color = "#2c3e50") +
      facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
      labs(title = "Time Series of Dengue, Electricity, and Weather Variables",
           x = "Date", y = "Value") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$seasonal_plot <- renderPlotly({
    ts_long <- tsibble_data() %>%
      pivot_longer(cols = c(denguecases, Electricity_KWh, AvgMeanTemp, TotalDailyRain),
                   names_to = "Variable", values_to = "Value")
    
    p <- gg_season(ts_long, Value) +
      facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
      labs(title = "Seasonal Patterns of Dengue, Electricity & Weather Variables") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$subseries_plot <- renderPlotly({
    ts_longer <- tsibble_data() %>%
      pivot_longer(-Date, names_to = "Variable", values_to = "Value")
    p <- gg_subseries(ts_longer, Value) +
      facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
      labs(title = "Cycle Subseries Plots") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$stl_weather_plot <- renderPlotly({
    weather_stl <- tsibble_data() %>%
      pivot_longer(cols = c(AvgMeanTemp, Electricity_KWh, TotalDailyRain),
                   names_to = "Variable", values_to = "Value") %>%
      group_by(Variable) %>%
      model(STL(Value)) %>%
      components() %>%
      autoplot() +
      labs(title = "STL Decomposition of Weather and Electricity Variables") +
      theme_minimal()
    ggplotly(weather_stl)
  })
  
  output$ccf_rainfall <- renderPlotly({
    ccf_plot <- tsibble_data() %>%
      CCF(denguecases, TotalDailyRain) %>%
      autoplot() +
      labs(title = "CCF: Dengue Cases vs Total Rainfall") +
      theme_minimal()
    ggplotly(ccf_plot)
  })
  
  output$ccf_temperature <- renderPlotly({
    ccf_plot <- tsibble_data() %>%
      CCF(denguecases, AvgMeanTemp) %>%
      autoplot() +
      labs(title = "CCF: Dengue Cases vs Avg Temp") +
      theme_minimal()
    ggplotly(ccf_plot)
  })
  
  plot_all <- function(var, id) {
    output[[paste0("acf_", id)]] <- renderPlotly({
      ggplotly(autoplot(ACF(tsibble_data(), !!sym(var))))
    })
    
    output[[paste0("pacf_", id)]] <- renderPlotly({
      ggplotly(autoplot(PACF(tsibble_data(), !!sym(var))))
    })
    
    output[[paste0("stl_", id)]] <- renderPlotly({
      model_stl <- tsibble_data() %>% model(STL(!!sym(var)))
      ggplotly(autoplot(components(model_stl)))
    })
    
    output[[paste0("classical_", id)]] <- renderPlotly({
      model_classical <- tsibble_data() %>% model(classical_decomposition(!!sym(var), type = "additive"))
      ggplotly(autoplot(components(model_classical)))
    })
    
    output[[paste0("ets_", id)]] <- renderPlotly({
      train <- tsibble_data() %>% filter(Date < yearmonth("2024 Jan"))
      model_ets <- train %>% model(ETS(!!sym(var)))
      forecast_ets <- forecast(model_ets, h = "12 months")
      fitted_df <- augment(model_ets)
      
      p <- ggplot(tsibble_data(), aes(x = Date, y = !!sym(var))) +
        geom_line(color = "grey40") +
        autolayer(forecast_ets, series = "Forecast", alpha = 0.6) +
        geom_line(data = fitted_df, aes(y = .fitted), color = "blue") +
        labs(title = paste("ETS Forecast for", var)) +
        theme_minimal()
      ggplotly(p)
    })
    
    output[[paste0("compare_", id)]] <- renderPlotly({
      train <- tsibble_data() %>% filter(Date < yearmonth("2024 Jan"))
      model_ets <- train %>% model(ETS(!!sym(var)))
      model_arima <- train %>% model(ARIMA(!!sym(var)))
      forecast_ets <- forecast(model_ets, h = "12 months")
      forecast_arima <- forecast(model_arima, h = "12 months")
      
      p <- autoplot(forecast_ets, tsibble_data(), level = NULL) +
        autolayer(forecast_arima, colour = "red", level = NULL) +
        labs(title = paste("Forecast Comparison: ETS vs ARIMA for", var), y = var, x = "Date") +
        scale_color_manual(values = c("Forecast" = "blue", "ARIMA" = "red")) +
        theme_minimal()
      ggplotly(p)
    })
  }
  
  plot_all("denguecases", "dengue")
  plot_all("AvgMeanTemp", "temp")
  plot_all("TotalDailyRain", "rain")
  plot_all("Electricity_KWh", "elec")
  
  output$weather_forecast_summary <- renderPlotly({
    train_data <- tsibble_data() %>%
      filter(Date < yearmonth("2024 Jan"))
    
    model_temp <- train_data %>% model(ETS(AvgMeanTemp))
    model_rain <- train_data %>% model(ETS(TotalDailyRain))
    model_elec <- train_data %>% model(ETS(Electricity_KWh))
    
    fc_temp <- model_temp %>% forecast(h = "12 months")
    fc_rain <- model_rain %>% forecast(h = "12 months")
    fc_elec <- model_elec %>% forecast(h = "12 months")
    
    fc_temp_df <- fc_temp %>% as_tibble() %>% select(Date, .mean) %>% mutate(Variable = "AvgMeanTemp")
    fc_rain_df <- fc_rain %>% as_tibble() %>% select(Date, .mean) %>% mutate(Variable = "TotalDailyRain")
    fc_elec_df <- fc_elec %>% as_tibble() %>% select(Date, .mean) %>% mutate(Variable = "Electricity_KWh")
    
    weather_fc <- bind_rows(fc_temp_df, fc_rain_df, fc_elec_df)
    
    p <- weather_fc %>%
      ggplot(aes(x = Date, y = .mean, color = Variable)) +
      geom_line(size = 1) +
      facet_wrap(~ Variable, scales = "free_y") +
      labs(
        title = "Forecasts: Weather & Electricity (Next 12 Months)",
        y = "Forecasted Value",
        x = "Date"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$forecast_accuracy_plot <- renderPlotly({
    train_data <- tsibble_data() %>% filter(Date < yearmonth("2024 Jan"))
    test_data <- tsibble_data() %>% filter(Date >= yearmonth("2024 Jan"))
    
    model_rain_ets <- train_data %>% model(ETS(TotalDailyRain))
    model_elec_ets <- train_data %>% model(ETS(Electricity_KWh))
    model_rain_arima <- train_data %>% model(ARIMA(TotalDailyRain))
    model_elec_arima <- train_data %>% model(ARIMA(Electricity_KWh))
    
    fc_rain_ets <- forecast(model_rain_ets, h = "12 months")
    fc_elec_ets <- forecast(model_elec_ets, h = "12 months")
    fc_rain_arima <- forecast(model_rain_arima, h = "12 months")
    fc_elec_arima <- forecast(model_elec_arima, h = "12 months")
    
    acc_rain_compare <- bind_rows(
      accuracy(fc_rain_ets, test_data %>% select(Date, TotalDailyRain)) %>% mutate(Variable = "TotalDailyRain", Model = "ETS"),
      accuracy(fc_rain_arima, test_data %>% select(Date, TotalDailyRain)) %>% mutate(Variable = "TotalDailyRain", Model = "ARIMA")
    )
    
    acc_elec_compare <- bind_rows(
      accuracy(fc_elec_ets, test_data %>% select(Date, Electricity_KWh)) %>% mutate(Variable = "Electricity_KWh", Model = "ETS"),
      accuracy(fc_elec_arima, test_data %>% select(Date, Electricity_KWh)) %>% mutate(Variable = "Electricity_KWh", Model = "ARIMA")
    )
    
    weather_compare <- bind_rows(acc_rain_compare, acc_elec_compare) %>%
      select(Variable, Model, RMSE, MAE, MAPE)
    
    p <- weather_compare %>%
      pivot_longer(cols = c(RMSE, MAE, MAPE), names_to = "Metric", values_to = "Value") %>%
      ggplot(aes(x = Model, y = Value, fill = Model)) +
      geom_col(position = "dodge") +
      facet_grid(Metric ~ Variable, scales = "free_y") +
      labs(
        title = "ETS vs ARIMA Forecast Accuracy: Rainfall & Electricity",
        y = "Error Metric Value",
        x = "Model Type"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # ---------------- GEOSPATIAL ----------------
  #Distributions after Kriging
  # Create the grid data
  grid <- terra::rast(mpsz2019, nrows = 400, ncols = 700)  
  
  # Create xy from the grid
  xy <- terra::xyFromCell(grid, 1:ncell(grid))
  
  # Create coop spatial points data frame 
  coop <- st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = st_crs(mpsz2019))
  coop <- st_filter(coop, mpsz2019)
  
  # Reactive dataset based on selected Month-Year
  weather_month <- eventReactive(input$show_result, {
    selected_date <- as.Date(paste("01", input$month_year), format = "%d %b-%Y")
    selected_year <- format(selected_date, "%Y")
    selected_month <- format(selected_date, "%m")
    
    weather_filtered %>%
      filter(Year == as.numeric(selected_year), Month == as.numeric(selected_month)) %>%
      group_by(Station) %>%
      summarise(
        MonthlyRainfall = sum(DailyRainfall, na.rm = TRUE),
        MonthlyMeanTemp = mean(MeanTemperature, na.rm = TRUE),
        MonthlyMeanWindSpeed = mean(MeanWindSpeed, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      filter(Station %in% c("Admiralty", "Ang Mo Kio", "Changi", "Choa Chu Kang (South)", 
                            "East Coast Parkway", "Jurong (West)", "Jurong Island", "Newton", 
                            "Pasir Panjang", "Pulau Ubin", "Seletar", "Sentosa Island", 
                            "Tai Seng", "Tuas South"))
  })
  
  # Kriging 
  kriging_results <- eventReactive(input$show_result, {
    req(weather_month())
    
    model_type <- switch(input$model, "Sph" = "Sph", "Exp" = "Exp", "Gau" = "Gau")
    
    # Check which tab is active
    if (input$tabset_variogram == "Automatic Variogram") {
      # Automatically compute variogram model
      auto_vgm <- variogram(as.formula(paste(input$spatial_variable, "~ 1")), data = weather_month())
      v_model <- fit.variogram(auto_vgm, vgm(model = model_type))
    } else {
      # Use manually specified values for variogram parameters
      v_model <- vgm(psill = input$psill, model = model_type, range = input$range, nugget = input$nugget)
    }
    
    krige_model <- gstat(formula = as.formula(paste(input$spatial_variable, "~ 1")), 
                         model = v_model, 
                         data = weather_month())
    
    predictions <- predict(krige_model, coop)
    predictions$x <- st_coordinates(predictions)[,1]
    predictions$y <- st_coordinates(predictions)[,2]
    predictions$pred <- predictions$var1.pred
    predictions$variance <- predictions$var1.var
    
    kpred <- terra::rasterize(predictions, grid, field = "pred")
    kpred_var <- terra::rasterize(predictions, grid, field = "variance")
    
    list(pred_raster = kpred, var_raster = kpred_var, selected_variable = input$spatial_variable, selected_month_year = input$month_year)
  })
  
  #Local Measures of Spatial Autocorrelation
  # Event to react only when "Show Result" button is clicked
  filtered_data <- reactive({
    # Capture all the inputs inside the eventReactive to trigger only on button click
    month_year_selected <- input$stat_month_year
    variable_selected <- input$stat_variable
    stat_selected <- input$stat 
    nsim_selected <- input$nsim
    lisa_class_selected <- input$lisa_class
    
    weather_month <- weather %>%
      filter(format(as.Date(paste(Year, Month, "01", sep = "-")), "%b-%Y") == month_year_selected)
    
    keepstations <- c("Admiralty", "Ang Mo Kio", "Changi", "Choa Chu Kang (South)", "East Coast Parkway", 
                      "Jurong (West)", "Jurong Island", "Newton", "Pasir Panjang", "Pulau Ubin", 
                      "Seletar", "Sentosa Island", "Tai Seng", "Tuas South")
    
    weather_month <- weather_month %>% filter(Station %in% keepstations)
    
    # Aggregate data
    weather_aggregated <- weather_month %>%
      group_by(Station) %>%
      summarise(
        MonthlyRainfall = sum(DailyRainfall, na.rm = TRUE),
        MonthlyMeanTemp = mean(MeanTemperature, na.rm = TRUE),
        MonthlyMeanWindSpeed = mean(MeanWindSpeed, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Merge with spatial data
    weather_sf <- left_join(weather_aggregated, mpsz, by = "Station") %>%
      st_as_sf()
    
    if (!(variable_selected %in% colnames(weather_sf))) {
      stop(paste("Error: Variable", variable_selected, "not found in dataset."))
    }
    
    formula <- as.formula(paste(variable_selected, "~ 1"))
    
    variogram_model <- variogram(formula, weather_sf)
    fit_model <- tryCatch({
      fit.variogram(variogram_model, model = vgm("Exp"), fit.method = 6)
    }, error = function(e) {
      NULL
    })
    
    if (is.null(fit_model)) {
      stop("Error: Variogram fitting failed.")
    }
    
    # Perform Kriging interpolation
    kriging_result <- krige(formula, weather_sf, mpsz, model = fit_model)
    
    # Convert kriging result to sf object
    kriged_sf <- st_as_sf(kriging_result)
    
    # Compute spatial weights using sfdep
    mpsz_centroids <- st_centroid(mpsz)
    knn_result <- st_knn(mpsz_centroids, k = 3)
    knn_weights <- st_weights(knn_result, style = "W")
    
    # Compute Local Moran’s I
    local_moran_res <- local_moran(as.numeric(kriged_sf$var1.pred), nb = knn_result, wt = knn_weights, nsim = nsim_selected)
    local_moran_df <- as.data.frame(local_moran_res)
    
    kriged_sf <- kriged_sf %>%
      mutate(
        ii = local_moran_df$ii,
        p_ii = local_moran_df$p_ii,
        z_ii = local_moran_df$z_ii,
        var_ii = local_moran_df$var_ii,
        eii = local_moran_df$eii,
        mean = local_moran_df$mean,
        median = local_moran_df$median,
        pysal = local_moran_df$pysal
      )
    
    return(kriged_sf)
  })
  
  
  # Render Kriging Plot
  output$mean_temp_plot <- renderTmap({
    req(kriging_results())
    
    variable_title <- switch(kriging_results()$selected_variable,
                             "MonthlyMeanTemp" = "Mean Temperature (°C)",
                             "MonthlyRainfall" = "Total Rainfall (mm)",
                             "MonthlyMeanWindSpeed" = "Mean Wind Speed (km/h)")
    
    tm_shape(kriging_results()$pred_raster) + 
      tm_raster(col_alpha = 0.6, palette = "YlOrRd", title = paste(variable_title)) +
      tm_layout(main.title = paste("Distribution of", variable_title, "for", kriging_results()$selected_month_year), frame = TRUE, 
                legend.position = c("left", "top"), legend.frame = FALSE,
                asp = 1) +
      tm_compass(type = "8star", size = 2) +
      tm_grid(alpha = 0.2)
  })
  
  # Render Kriging Variance Plot
  output$variance_plot <- renderTmap({
    req(kriging_results())
    
    variable_title <- switch(kriging_results()$selected_variable,
                             "MonthlyMeanTemp" = "Mean Temperature (°C)",
                             "MonthlyRainfall" = "Total Rainfall (mm)",
                             "MonthlyMeanWindSpeed" = "Mean Wind Speed (km/h)")
    
    tm_shape(kriging_results()$var_raster) + 
      tm_raster(col_alpha = 0.6, palette = "YlGnBu", title = paste("Kriging Variance of", variable_title)) +
      tm_layout(main.title = paste("Kriging Variance of", variable_title, "for", kriging_results()$selected_month_year), frame = TRUE, 
                legend.position = c("left", "top"), legend.frame = FALSE,
                asp = 1) +
      tm_compass(type = "8star", size = 2) +
      tm_grid(alpha = 0.2)
  })
  
  
  # Statistic Map
  output$stat_plot <- renderTmap({
    req(filtered_data()) 
    data <- filtered_data()
    
    
    variable_title <- switch(input$stat_variable,
                             "MonthlyMeanTemp" = "Mean Temperature (°C)",
                             "MonthlyRainfall" = "Total Rainfall (mm)",
                             "MonthlyMeanWindSpeed" = "Mean Wind Speed (km/h)")
    
    stat_title <- switch(input$stat,
                         "ii" = "Local Moran I",
                         "p_ii" = "P-value",
                         "z_ii" = "Std Deviation",
                         "var_ii" = "Variance",
                         "eii" = "Expectation")
    
    
    tm_shape(data) +
      tm_fill(input$stat, palette = "brewer.blues", legend.show = TRUE) +  
      tm_borders() +
      tm_title(paste(stat_title, "of", variable_title, ",", input$stat_month_year)) +
      tm_layout(
        legend.position = c("right", "bottom"),  
        legend.frame = FALSE,                     
        legend.title.size = 1.1,                 
        legend.text.size = 0.8                  
      )
  })
  
  # LISA Map
  output$lisa_map <- renderTmap({
    req(filtered_data())  
    data <- filtered_data()
    
    selected_class <- input$lisa_class
    
    if (!(selected_class %in% colnames(data))) {
      stop("Error: Selected LISA classification not found in dataset.")
    }
    
    # Ensure it's treated as a factor for proper categorical mapping
    data[[selected_class]] <- as.factor(data[[selected_class]])
    
    # Filter significant areas (p-value < 0.05)
    lisa_sig <- data %>% filter(p_ii < 0.05)  
    
    # Define a color palette for categorical classes
    classification_palette <- c(
      "High-High" = "#f2677d",   
      "Low-Low" = "#2166AC",     
      "High-Low" = "#fcadb9",    
      "Low-High" = "#9ccee9",    
      "Not Significant" = "#D9D9D9"  
    )
    
    variable_title <- switch(input$stat_variable,
                             "MonthlyMeanTemp" = "Mean Temperature (°C)",
                             "MonthlyRainfall" = "Total Rainfall (mm)",
                             "MonthlyMeanWindSpeed" = "Mean Wind Speed (km/h)")
    
    
    tm_shape(data) +
      tm_polygons() +
      tm_borders(fill_alpha = 0.5) +
      tm_shape(lisa_sig) +
      tm_fill(selected_class, palette = classification_palette, title = "LISA Classification") + 
      tm_borders(fill_alpha = 0.4) +
      tm_title(paste("LISA Map of", variable_title, "using", selected_class)) +
      tm_layout(
        legend.position = c("right", "bottom"),  
        legend.frame = FALSE,                    
        legend.title.size = 1.1,                 
        legend.text.size = 0.8                 
      )
  })
}

# ==== Run the App ====
shinyApp(ui, server)

