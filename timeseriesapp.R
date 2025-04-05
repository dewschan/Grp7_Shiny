library(shiny)
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)
library(fabletools)
library(lubridate)
library(readr)
library(Rcpp)
library(RcppArmadillo)
library(distributional)
library(urca)

# Read data silently
combined_data <- read_csv("data/combined_data.csv", show_col_types = FALSE)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML(".nav-pills { padding-right: 0; }
      .nav-pills .nav-link { display: block; width: 100%; border-radius: 8px; margin-bottom: 8px; padding: 14px 20px; background-color: #ecf0f1; color: #2c3e50; font-weight: 500; text-align: left; transition: all 0.2s ease-in-out; box-sizing: border-box; }
      .nav-pills .nav-link.active { background-color: #3498db !important; color: white !important; box-shadow: 0 0 0 2px #3498db33; }
      .nav-pills .nav-link:hover { background-color: #bdc3c7; color: black; }
      .nav-pills .nav-item { margin: 0 !important; padding: 0 !important; }"))
  ),
  
  titlePanel("Dengue, Electricity & Weather Time Series Forecasting Dashboard"),
  
  fluidRow(
    column(
      width = 3,
      conditionalPanel(
        condition = "['Time Series Overview', 'Seasonality', 'Dengue Analysis', 'Temperature Analysis', 'Rainfall Analysis', 'Electricity Analysis'].includes(input.tab)",
        sliderInput("year_range", "Select Year Range:", min = 2013, max = 2025, value = c(2013, 2025), step = 1, sep = "", ticks = FALSE)
      ),
      navlistPanel(
        id = "tab",
        well = FALSE,
        tabPanel("Time Series Overview", value = "Time Series Overview"),
        tabPanel("Seasonality", value = "Seasonality"),
        tabPanel("Cross-Correlation Analysis", value = "Cross-Correlation Analysis"),
        tabPanel("Dengue Analysis", value = "Dengue Analysis"),
        tabPanel("Temperature Analysis", value = "Temperature Analysis"),
        tabPanel("Rainfall Analysis", value = "Rainfall Analysis"),
        tabPanel("Electricity Analysis", value = "Electricity Analysis"),
        tabPanel("Weather Forecast Summary", value = "Weather Forecast Summary"),
        tabPanel("Forecast Accuracy Comparison", value = "Forecast Accuracy Comparison")
      )
    ),
    column(width = 9, uiOutput("main_plot_output"))
  )
)

server <- function(input, output, session) {
  tsibble_filtered <- reactive({
    req(input$year_range)
    combined_data %>%
      mutate(Date = yearmonth(MonthYear)) %>%
      filter(year(Date) >= input$year_range[1], year(Date) <= input$year_range[2]) %>%
      group_by(Date) %>%
      summarise(across(c(denguecases, Electricity_KWh, AvgMeanTemp, TotalDailyRain), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
      as_tsibble(index = Date)
  })
  
  tsibble_full <- reactive({
    combined_data %>%
      mutate(Date = yearmonth(MonthYear)) %>%
      group_by(Date) %>%
      summarise(across(c(denguecases, Electricity_KWh, AvgMeanTemp, TotalDailyRain), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
      as_tsibble(index = Date)
  })
  
  output$main_plot_output <- renderUI({
    switch(input$tab,
           "Time Series Overview" = plotOutput("ts_overview_plot", height = "800px"),
           "Seasonality" = tagList(h4("Seasonal Plot"), plotOutput("seasonal_plot", height = "400px"), br(), h4("Subseries Plot"), plotOutput("subseries_plot", height = "400px"), br(), h4("STL Decomposition (Weather & Electricity)"), plotOutput("stl_weather_plot", height = "450px")),
           "Cross-Correlation Analysis" = tagList(h4("Dengue vs Total Rainfall"), plotOutput("ccf_rainfall"), h4("Dengue vs Average Temperature"), plotOutput("ccf_temperature")),
           "Dengue Analysis" = plot_all_ui("dengue"),
           "Temperature Analysis" = plot_all_ui("temp"),
           "Rainfall Analysis" = plot_all_ui("rain"),
           "Electricity Analysis" = plot_all_ui("elec"),
           "Weather Forecast Summary" = plotOutput("weather_forecast_summary", height = "800px"),
           "Forecast Accuracy Comparison" = plotOutput("forecast_accuracy_plot", height = "800px")
    )
  })
  
  output$ts_overview_plot <- renderPlot({
    tsibble_filtered() %>%
      pivot_longer(cols = -Date, names_to = "Variable", values_to = "Value") %>%
      ggplot(aes(x = Date, y = Value)) +
      geom_line(linewidth = 1) +
      facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
      theme_minimal() +
      labs(title = "Time Series Overview")
  })
  
  output$seasonal_plot <- renderPlot({
    tsibble_filtered() %>%
      pivot_longer(cols = -Date, names_to = "Variable", values_to = "Value") %>%
      gg_season(Value) +
      facet_wrap(~ Variable, scales = "free_y") +
      theme_minimal()
  })
  
  output$subseries_plot <- renderPlot({
    tsibble_filtered() %>%
      pivot_longer(cols = -Date, names_to = "Variable", values_to = "Value") %>%
      gg_subseries(Value) +
      facet_wrap(~ Variable, scales = "free_y") +
      theme_minimal() +
      labs(caption = "Blue lines represent seasonal means across periods for each month.")
  })
  
  output$stl_weather_plot <- renderPlot({
    tsibble_filtered() %>%
      pivot_longer(cols = c(AvgMeanTemp, Electricity_KWh, TotalDailyRain), names_to = "Variable", values_to = "Value") %>%
      group_by(Variable) %>%
      model(STL(Value)) %>%
      components() %>%
      autoplot() +
      theme_minimal()
  })
  
  output$ccf_rainfall <- renderPlot({
    tsibble_filtered() %>% CCF(denguecases, TotalDailyRain) %>% autoplot() + theme_minimal()
  })
  output$ccf_temperature <- renderPlot({
    tsibble_filtered() %>% CCF(denguecases, AvgMeanTemp) %>% autoplot() + theme_minimal()
  })
  
  plot_all <- function(var, id) {
    output[[paste0("acf_", id)]] <- renderPlot({ autoplot(ACF(tsibble_filtered(), !!sym(var))) })
    output[[paste0("pacf_", id)]] <- renderPlot({ autoplot(PACF(tsibble_filtered(), !!sym(var))) })
    output[[paste0("stl_", id)]] <- renderPlot({ model_stl <- tsibble_filtered() %>% model(STL(!!sym(var))); autoplot(components(model_stl)) })
    output[[paste0("classical_", id)]] <- renderPlot({ model_classical <- tsibble_filtered() %>% model(classical_decomposition(!!sym(var))); autoplot(components(model_classical)) })
    
    output[[paste0("ets_", id)]] <- renderPlot({
      train <- tsibble_filtered() %>% filter(Date < yearmonth("2024 Jan"))
      model_ets <- train %>% model(ETS(!!sym(var)))
      fc <- forecast(model_ets, h = "12 months")
      fitted <- augment(model_ets)
      ggplot(tsibble_filtered(), aes(x = Date)) +
        geom_line(aes(y = !!sym(var), color = "Actual")) +
        geom_line(data = fitted, aes(y = .fitted, color = "Fitted")) +
        autolayer(fc, aes(color = "Forecast"), alpha = 0.4) +
        scale_color_manual(name = "Series", values = c("Actual" = "gray40", "Fitted" = "blue", "Forecast" = "blue")) +
        labs(title = paste("ETS Forecast for", var), y = var, x = "Date") +
        theme_minimal()
    })
    
    output[[paste0("compare_", id)]] <- renderPlot({
      train <- tsibble_filtered() %>% filter(Date < yearmonth("2024 Jan"))
      var_sym <- sym(var)
      
      model_ets <- train %>% model(ETS(!!var_sym))
      model_arima <- tryCatch({
        train %>% model(ARIMA(!!var_sym))
      }, error = function(e) NULL)
      
      fc_ets <- forecast(model_ets, h = "12 months") %>%
        as_tsibble() %>% as_tibble() %>%
        filter(!is.na(.mean)) %>% mutate(Model = "ETS")
      
      fc_arima <- if (!is.null(model_arima)) {
        forecast(model_arima, h = "12 months") %>%
          as_tsibble() %>% as_tibble() %>%
          filter(!is.na(.mean)) %>% mutate(Model = "ARIMA")
      } else {
        NULL
      }
      
      actual <- tsibble_filtered() %>%
        select(Date, !!var_sym) %>%
        rename(Value = !!var_sym) %>%
        mutate(Model = "Actual")
      
      all_fc <- bind_rows(
        fc_ets %>% select(Date, Value = .mean, Model),
        if (!is.null(fc_arima)) fc_arima %>% select(Date, Value = .mean, Model)
      )
      
      ggplot() +
        geom_line(data = actual, aes(x = Date, y = Value, color = "Actual"), linewidth = 1, alpha = 0.6) +
        geom_line(data = all_fc, aes(x = Date, y = Value, color = Model), linewidth = 1.2) +
        scale_color_manual(name = "Forecast Model", values = c("Actual" = "gray40", "ETS" = "blue", "ARIMA" = "red")) +
        labs(title = paste("Forecast Comparison: ETS vs ARIMA for", var), y = var, x = "Date") +
        theme_minimal()
    })
  }
  
  plot_all_ui <- function(id) {
    tagList(
      h4("ACF & PACF"), plotOutput(paste0("acf_", id)), plotOutput(paste0("pacf_", id)),
      h4("STL Decomposition"), plotOutput(paste0("stl_", id)),
      h4("Classical Decomposition"), plotOutput(paste0("classical_", id)),
      h4("ETS Forecast"), plotOutput(paste0("ets_", id)),
      h4("ETS vs ARIMA Forecast Comparison"), plotOutput(paste0("compare_", id))
    )
  }
  
  plot_all("denguecases", "dengue")
  plot_all("AvgMeanTemp", "temp")
  plot_all("TotalDailyRain", "rain")
  plot_all("Electricity_KWh", "elec")
  
  output$weather_forecast_summary <- renderPlot({
    train <- tsibble_full() %>% filter(Date < yearmonth("2024 Jan"))
    vars <- c("AvgMeanTemp", "TotalDailyRain", "Electricity_KWh")
    forecasts <- map_dfr(vars, function(var) {
      model <- train %>% model(ETS(!!sym(var)))
      forecast(model, h = "12 months") %>%
        as_tibble() %>%
        select(Date, .mean) %>%
        mutate(Variable = var)
    })
    forecasts %>%
      ggplot(aes(x = Date, y = .mean, color = Variable)) +
      geom_line(linewidth = 1) +
      facet_wrap(~ Variable, scales = "free_y") +
      labs(title = "12-Month Forecast (ETS)") +
      theme_minimal()
  })
  
  output$forecast_accuracy_plot <- renderPlot({
    train <- tsibble_full() %>% filter(Date < yearmonth("2024 Jan"))
    test <- tsibble_full() %>% filter(Date >= yearmonth("2024 Jan"))
    vars <- c("TotalDailyRain", "Electricity_KWh")
    acc <- map_dfr(vars, function(var) {
      m1 <- train %>% model(ETS(!!sym(var)))
      m2 <- tryCatch(train %>% model(ARIMA(!!sym(var))), error = function(e) NULL)
      f1 <- forecast(m1, h = "12 months")
      f2 <- if (!is.null(m2)) forecast(m2, h = "12 months") else NULL
      bind_rows(
        accuracy(f1, test %>% select(Date, !!sym(var))) %>% mutate(Model = "ETS"),
        if (!is.null(f2)) accuracy(f2, test %>% select(Date, !!sym(var))) %>% mutate(Model = "ARIMA")
      ) %>% mutate(Variable = var)
    })
    acc %>%
      pivot_longer(cols = c(RMSE, MAE, MAPE), names_to = "Metric", values_to = "Value") %>%
      ggplot(aes(x = Model, y = Value, fill = Model)) +
      geom_col(position = "dodge") +
      facet_grid(Metric ~ Variable, scales = "free_y") +
      labs(title = "Forecast Accuracy: ETS vs ARIMA") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)
