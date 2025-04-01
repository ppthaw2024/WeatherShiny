#fit the page
# Load packages
pacman::p_load(shiny, tidyverse, lubridate, tsibble, fable, feasts, fable.prophet, plotly, DT, zoo)

# Load and impute data
weather_data <- read_csv("data/weather_data_cleaned.csv")

weather_tsbl <- as_tsibble(weather_data, key = Station, index = Date) %>%
  group_by_key() %>%
  fill_gaps(.full = TRUE) %>%
  mutate(
    `Daily Rainfall Total (mm)` = zoo::na.approx(`Daily Rainfall Total (mm)`, x = Date, na.rm = FALSE),
    `Mean Temperature (°C)` = zoo::na.approx(`Mean Temperature (°C)`, x = Date, na.rm = FALSE),
    `Maximum Temperature (°C)` = zoo::na.approx(`Maximum Temperature (°C)`, x = Date, na.rm = FALSE),
    `Minimum Temperature (°C)` = zoo::na.approx(`Minimum Temperature (°C)`, x = Date, na.rm = FALSE)
  ) %>% ungroup()

variables_select <- c(
  "Daily Rainfall Total (mm)" = "Daily Rainfall Total (mm)",
  "Mean Temperature (°C)" = "Mean Temperature (°C)",
  "Minimum Temperature (°C)" = "Minimum Temperature (°C)",
  "Maximum Temperature (°C)" = "Maximum Temperature (°C)"
)

# UI
ui <- navbarPage("Singapore Weather App",
                 
                 tabPanel("Time Series Visualization",
                          fluidPage(
                            fluidRow(
                              column(2,
                                     wellPanel(
                                       selectInput("variable", "Weather Variable:", choices = variables_select),
                                       radioButtons("resolution", "Time Resolution:",
                                                    choices = c("Daily", "Weekly", "Monthly"), selected = "Daily"),
                                       selectInput("station", "Select Station(s):",
                                                   choices = c("All Stations", unique(weather_tsbl$Station)),
                                                   selected = "All Stations", multiple = TRUE),
                                       dateRangeInput("daterange", "Date Range:",
                                                      start = min(weather_tsbl$Date), end = max(weather_tsbl$Date))
                                     )
                              ),
                              column(10,
                                     plotlyOutput("ts_plot", height = "500px")
                              )
                            )
                          )
                 ),
                 
                 navbarMenu("Time Series Forecasting",
                            
                            # Forecast & Validation tab
                            tabPanel("Forecast & Validation",
                                     fluidPage(
                                       fluidRow(
                                         column(2,
                                                wellPanel(
                                                  selectInput("forecast_var", "Weather Variable:", choices = variables_select),
                                                  selectInput("forecast_station", "Select Station:", choices = unique(weather_tsbl$Station)),
                                                  radioButtons("time_resolution", "Time Resolution:", choices = c("Daily", "Weekly", "Monthly")),
                                                  sliderInput("train_ratio", "Train-Test Split:", min = 0.6, max = 0.9, value = 0.8),
                                                  checkboxGroupInput("models", "Forecasting Models:",
                                                                     choices = c("STL + Naive" = "STLNaive",
                                                                                 "STL + ARIMA" = "STLArima",
                                                                                 "STL + ETS" = "STLETS",
                                                                                 "Auto ARIMA" = "AUTOARIMA",
                                                                                 "Auto Prophet" = "AUTOprophet",
                                                                                 "Auto ETS" = "AUTOETS"),
                                                                     selected = c("AUTOARIMA", "AUTOETS")),
                                                  actionButton("run_forecast", "Generate Forecast")
                                                )
                                         ),
                                         column(10,
                                                fluidRow(
                                                  column(6,
                                                         uiOutput("forecast_title"),
                                                         plotlyOutput("forecast_plot", height = "300px")
                                                  ),
                                                  column(6,
                                                         uiOutput("residual_title"),
                                                         plotOutput("residual_plot", height = "300px")
                                                  )
                                                ),
                                                fluidRow(
                                                  column(6,
                                                         uiOutput("test_title"),
                                                         plotOutput("test_plot", height = "300px")
                                                  ),
                                                  column(6,
                                                         uiOutput("accuracy_title"),
                                                         DTOutput("accuracy_tbl", height = "300px")
                                                  )
                                                )
                                         )
                                       )
                                     )
                            ),
                            
                            # Future Forecast tab
                            tabPanel("Future Forecast (Refitted)",
                                     fluidPage(
                                       fluidRow(
                                         column(2,
                                                wellPanel(
                                                  selectInput("full_var", "Weather Variable:", choices = variables_select),
                                                  selectInput("full_station", "Select Station:", choices = unique(weather_tsbl$Station)),
                                                  radioButtons("horizon_unit", "Forecast Period Unit:",
                                                               choices = c("Days" = "day", "Weeks" = "week"),
                                                               selected = "day"),
                                                  sliderInput("full_horizon", "Select Forecast Period:", min = 1, max = 91, value = 30),
                                                  checkboxGroupInput("full_models", "Forecasting Models:",
                                                                     choices = c("STL + Naive" = "STLNaive",
                                                                                 "STL + ARIMA" = "STLArima",
                                                                                 "STL + ETS" = "STLETS",
                                                                                 "Auto ARIMA" = "AUTOARIMA",
                                                                                 "Auto Prophet" = "AUTOprophet",
                                                                                 "Auto ETS" = "AUTOETS"),
                                                                     selected = c("AUTOARIMA", "AUTOETS")),
                                                  actionButton("run_future", "Forecast")
                                                )
                                         ),
                                         column(10,
                                                uiOutput("future_title"),
                                                plotlyOutput("future_forecast_plot", height = "300px"),
                                                DTOutput("future_table", height = "300px")
                                         )
                                       )
                                     )
                            )
                 )
)

# Server
server <- function(input, output, session) {
  
  # Visualization Tab
  selected_data <- reactive({
    data <- weather_tsbl
    if (!("All Stations" %in% input$station)) {
      data <- data %>% filter(Station %in% input$station)
    }
    data <- data %>% filter(Date >= input$daterange[1], Date <= input$daterange[2])
    
    if (input$resolution == "Weekly") {
      data %>%
        mutate(Period = floor_date(Date, "week")) %>%
        group_by(Station, Period) %>%
        summarise(Value = mean(.data[[input$variable]], na.rm = TRUE), .groups = "drop")
    } else if (input$resolution == "Monthly") {
      data %>%
        mutate(Period = floor_date(Date, "month")) %>%
        group_by(Station, Period) %>%
        summarise(Value = mean(.data[[input$variable]], na.rm = TRUE), .groups = "drop")
    } else {
      data %>% mutate(Period = Date, Value = .data[[input$variable]]) %>%
        select(Station, Period, Value)
    }
  })
  
  output$ts_plot <- renderPlotly({
    p <- ggplot(selected_data(), aes(x = Period, y = Value, color = Station)) +
      geom_line() + theme_minimal() +
      labs(title = paste("Time Series of", input$variable), x = "Date", y = input$variable)
    ggplotly(p)
  })
  
  # Forecast & Validation
  observeEvent(input$run_forecast, {
    req(input$models)
    
    station_data <- weather_tsbl %>%
      filter(Station == input$forecast_station) %>%
      select(Date, value = all_of(input$forecast_var))
    
    train_n <- floor(nrow(station_data) * input$train_ratio)
    train_ts <- as_tsibble(station_data[1:train_n, ], index = Date)
    test_ts <- as_tsibble(station_data[(train_n + 1):nrow(station_data), ], index = Date)
    full_ts <- as_tsibble(station_data, index = Date)
    
    models <- train_ts %>%
      model(
        STLNaive = decomposition_model(STL(value), NAIVE(season_adjust)),
        STLArima = decomposition_model(STL(value), ARIMA(season_adjust)),
        STLETS = decomposition_model(STL(value), ETS(season_adjust ~ season("N"))),
        AUTOARIMA = ARIMA(value),
        AUTOprophet = prophet(value),
        AUTOETS = ETS(value)
      ) %>% select(all_of(input$models))
    
    fc <- forecast(models, h = nrow(test_ts))
    
    output$forecast_plot <- renderPlotly({
      p <- autoplot(full_ts, value) +
        autolayer(fc, level = NULL) +
        labs(title = NULL, x = "Date", y = input$forecast_var) +
        theme_minimal()
      ggplotly(p)
    })
    
    output$residual_plot <- renderPlot({
      augment(models) %>%
        filter(!is.na(.resid)) %>%
        ggplot(aes(x = .resid)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black") +
        facet_wrap(~.model, scales = "free") +
        theme_minimal()
    })
    
    output$test_plot <- renderPlot({
      autoplot(test_ts, value) +
        autolayer(fc, level = NULL) +
        labs(title = "Test Forecast vs Actual", x = "Date", y = input$forecast_var) +
        theme_minimal()
    })
    
    output$accuracy_tbl <- renderDT({
      accuracy(fc, test_ts) %>% select(.model, RMSE, MAE, MAPE)
    })
    
    output$forecast_title <- renderUI({
      var <- names(variables_select)[variables_select == input$forecast_var]
      h4(paste("Forecast Validation for", var, "of", input$forecast_station))
    })
    output$residual_title <- renderUI(h4("Residual Plot"))
    output$test_title <- renderUI(h4("Test Forecast vs Actual"))
    output$accuracy_title <- renderUI(h4("Forecast Accuracy"))
  })
  
  # Future Forecast (Refitted)
  observeEvent(input$run_future, {
    req(input$full_models)
    
    raw_data <- weather_tsbl %>% filter(Station == input$full_station)
    
    if (input$full_var == "Mean Temperature (°C)" && input$horizon_unit %in% c("week", "month")) {
      full_ts <- raw_data %>%
        mutate(period = floor_date(Date, unit = input$horizon_unit)) %>%
        index_by(period) %>%
        summarise(value = mean(`Mean Temperature (°C)`, na.rm = TRUE), .groups = "drop") %>%
        rename(Date = period)
    } else if (input$full_var == "Daily Rainfall Total (mm)" && input$horizon_unit %in% c("week", "month")) {
      full_ts <- raw_data %>%
        mutate(period = floor_date(Date, unit = input$horizon_unit)) %>%
        index_by(period) %>%
        summarise(value = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop") %>%
        rename(Date = period)
    } else {
      full_ts <- raw_data %>% select(Date, value = all_of(input$full_var))
    }
    
    full_ts <- as_tsibble(full_ts, index = Date)
    
    models <- full_ts %>%
      model(
        STLNaive = decomposition_model(STL(value), NAIVE(season_adjust)),
        STLArima = decomposition_model(STL(value), ARIMA(season_adjust)),
        STLETS = decomposition_model(STL(value), ETS(season_adjust ~ season("N"))),
        AUTOARIMA = ARIMA(value),
        AUTOprophet = prophet(value),
        AUTOETS = ETS(value)
      ) %>% select(all_of(input$full_models))
    
    fc <- forecast(models, h = paste(input$full_horizon, input$horizon_unit))
    
    output$future_forecast_plot <- renderPlotly({
      var_label <- names(variables_select)[variables_select == input$full_var]
      p <- autoplot(fc, level = 95) +
        labs(title = NULL, x = "Date", y = var_label) +
        theme_minimal()
      ggplotly(p, tooltip = c("x", "y", ".model"))
    })
    
    output$future_table <- renderDT({
      fc_tbl <- as_tibble(fc)
      
      if (input$horizon_unit == "week") {
        fc_tbl <- fc_tbl %>%
          mutate(year_week = paste(year(Date), "w", sprintf("%02d", isoweek(Date)), sep = "")) %>%
          select(.model, year_week, .mean) %>%
          rename(Forecast = .mean) %>%
          mutate(
            Forecast = round(Forecast, 2),
            .model = as.factor(.model),
            year_week = as.factor(year_week)
          )
      } else {
        fc_tbl <- fc_tbl %>%
          mutate(year_month = format(Date, "%Y-%m")) %>%
          select(.model, year_month, .mean) %>%
          rename(Forecast = .mean) %>%
          mutate(
            Forecast = round(Forecast, 2),
            .model = as.factor(.model),
            year_month = as.factor(year_month)
          )
      }
      
      datatable(fc_tbl,
                class = "hover",
                rownames = FALSE,
                filter = 'top',
                width = "100%",
                options = list(pageLength = 6, scrollX = TRUE)
      )
    })
    
    output$future_title <- renderUI({
      var_label <- names(variables_select)[variables_select == input$full_var]
      h4(paste("Future Forecast for", var_label, "of", input$full_station))
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)










