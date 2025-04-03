pacman::p_load(shiny, readr, dplyr, tidyverse, sf, sfdep, tmap, terra, gstat, automap)

# Define UI
library(shiny)
library(readr)
library(dplyr)
library(tidyverse)
library(sf)
library(plotly)
library(tmap)
library(terra)
library(gstat)
library(automap)
library(sfdep)

# Load data
weather_filtered <- read_rds("rds/weather_filtered.rds")
mpsz2019 <- read_rds("rds/mpsz2019.rds")
weather <- read_rds("rds/weather.rds")
mpsz <- read_rds("rds/mpsz.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Geospatial Analysis"),
  
  tabsetPanel(
    tabPanel("Spatial Interpolation",
             fluidRow(
               column(4,
                      selectInput("variable", "Select Variable:",
                                  choices = c("Total Rainfall (mm)" = "MonthlyRainfall",
                                              "Mean Temperature (°C)" = "MonthlyMeanTemp",
                                              "Mean Wind Speed (km/h)" = "MonthlyMeanWindSpeed"),
                                  selected = "MonthlyMeanTemp"),
                      selectInput("month_year", "Select Month-Year:",
                                  choices = format(seq(as.Date("2021-01-01"), as.Date("2024-04-01"), by = "month"), "%b-%Y"),
                                  selected = "Jan-2021"),
                      tabsetPanel(
                        id = "tabset_variogram",
                        tabPanel("Automatic Variogram", p("Auto-fitted variogram automatically determines the optimal variogram model and parameters (psill, range, nugget) based on the data, so users don't need to manually input these values.")),
                        tabPanel("Manual Adjustment",
                                 sliderInput("psill", "Psill:", min = 0, max = 5, value = 0.5),
                                 selectInput("model", "Model Type:", choices = c("Spherical" = "Sph", "Exponential" = "Exp", "Gaussian" = "Gau"), selected = "Sph"),
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
               ,
               fluidRow(
                 column(12, wellPanel(
                 h4("Introduction to Spatial Interpolation"),
                 p("Spatial interpolation is the process of predicting values for unmeasured locations based on known values from nearby locations."),
                 p("Kriging is a method of spatial interpolation that uses statistical models to predict the value of a variable at unmeasured points, taking into account the spatial correlation between data points."),
                 h4("How to Interpret the Plots"),
                 p("The first plot represents the predicted values (e.g., temperature, rainfall, or wind speed) for the selected variable at different locations."),
                 p("The second plot shows the Kriging variance, which represents the uncertainty of the predictions. Higher variance indicates less confidence in the prediction.")
             )
    )))),
    
    tabPanel("Local Measure of Spatial Autocorrelation",
             fluidRow(
               column(4,
                      selectInput("stat_variable", "Select Variable:",
                                  choices = c("Total Rainfall (mm)" = "MonthlyRainfall",
                                              "Mean Temperature (°C)" = "MonthlyMeanTemp",
                                              "Mean Wind Speed (km/h)" = "MonthlyMeanWindSpeed"),
                                  selected = "MonthlyMeanTemp"),
                      selectInput("stat_month_year", "Select Month-Year:",
                                  choices = format(seq(as.Date("2021-01-01"), as.Date("2024-04-01"), by = "month"), "%b-%Y"),
                                  selected = "Jan-2021"),
                      selectInput("stat", "Select Statistic:",
                                  choices = c("Local Moran I" = "ii", "P-value" = "p_ii", "Std Deviation" = "z_ii", "Variance" = "var_ii", "Expectation" = "eii"),
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
             
             # Interpretation text box below the maps
             fluidRow(
               column(12, wellPanel(
                 h5("Interpretation of Maps:"),
                 p("The first map displays the selected statistic (e.g., Local Moran's I, P-value, Std Deviation, etc.) for the chosen variable (e.g., Total Rainfall, Mean Temperature, Mean Wind Speed). Each color in the map represents the value of the statistic for a particular geographic region, where darker or lighter colors indicate higher or lower values of the statistic, respectively. This allows you to observe the spatial distribution and variability of the statistic across the different regions of Singapore."),
                 p("The second map is the LISA (Local Indicators of Spatial Association) map, which shows significant clusters of similar values for the selected variable. LISA highlights areas where high values are clustered together, and similarly, low values are grouped in specific locations. The LISA map helps to identify whether the spatial patterns are random or whether they exhibit some form of spatial autocorrelation (i.e., whether nearby areas tend to have similar or dissimilar values).")
               ))
             )
    )
))
# Define server logic
server <- function(input, output, session) {
  
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
      auto_vgm <- variogram(as.formula(paste(input$variable, "~ 1")), data = weather_month())
      v_model <- fit.variogram(auto_vgm, vgm(model = model_type))
    } else {
      # Use manually specified values for variogram parameters
      v_model <- vgm(psill = input$psill, model = model_type, range = input$range, nugget = input$nugget)
    }
    
    krige_model <- gstat(formula = as.formula(paste(input$variable, "~ 1")), 
                         model = v_model, 
                         data = weather_month())
    
    predictions <- predict(krige_model, coop)
    predictions$x <- st_coordinates(predictions)[,1]
    predictions$y <- st_coordinates(predictions)[,2]
    predictions$pred <- predictions$var1.pred
    predictions$variance <- predictions$var1.var
    
    kpred <- terra::rasterize(predictions, grid, field = "pred")
    kpred_var <- terra::rasterize(predictions, grid, field = "variance")
    
    list(pred_raster = kpred, var_raster = kpred_var, selected_variable = input$variable, selected_month_year = input$month_year)
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

shinyApp(ui = ui, server = server)



