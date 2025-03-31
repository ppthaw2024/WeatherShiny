# 3 Load packages
pacman::p_load(shiny, tidyverse, lubridate, tsibble, fable, feasts, fable.prophet, plotly, DT)

# Load data
weather_data <- read_csv("data/weather_data_cleaned.csv")
weather_tsbl <- as_tsibble(weather_data, key = Station, index = Date)

variables_select <- c(
  "Daily Rainfall Total (mm)" = "Daily Rainfall Total (mm)",
  "Mean Temperature (°C)" = "Mean Temperature (°C)",
  "Minimum Temperature (°C)" = "Minimum Temperature (°C)",
  "Maximum Temperature (°C)" = "Maximum Temperature (°C)",
  "Temp_Daily" = "Mean Temperature (°C)"
)

# UI
ui <- navbarPage(
  "Singapore Weather App",
  
  # --- Tab 1: Visualization ---
  tabPanel("Time Series Visualization",
           fluidPage(
             fluidRow(
               column(width = 2,
                      wellPanel(
                        selectInput("variable", "Weather Variable:", choices = variables_select),
                        radioButtons("resolution", "Time Resolution:",
                                     choices = c("Daily" = "day", "Weekly" = "week", "Monthly" = "month"),
                                     selected = "day"),
                        selectInput("station", "Select Station(s):",
                                    choices = unique(weather_tsbl$Station),
                                    selected = unique(weather_tsbl$Station)[1],
                                    multiple = TRUE),
                        dateRangeInput("daterange", "Select Date Range:",
                                       start = min(weather_data$Date),
                                       end = max(weather_data$Date))
                      )
               ),
               column(width = 10,
                      plotlyOutput("ts_plot", height = "500px"),
                      conditionalPanel(
                        condition = "input.variable == 'Daily Rainfall Total (mm)' && input.resolution == 'week'",
                        h4("Weekly Rainfall Totals"),
                        DT::dataTableOutput("weekly_rain_tbl")
                      ),
                      conditionalPanel(
                        condition = "input.variable == 'Daily Rainfall Total (mm)' && input.resolution == 'month'",
                        h4("Monthly Rainfall Totals"),
                        DT::dataTableOutput("monthly_rain_tbl")
                      )
               )
             )
           )
  ),
  
  # --- Tab 2: Forecasting ---
  tabPanel("Time Series Forecasting",
           fluidPage(
             fluidRow(
               column(width = 2,
                      wellPanel(
                        selectInput("forecast_var", "Weather Variable:", 
                                    choices = variables_select),
                        selectInput("forecast_station", "Select Station:", choices = unique(weather_tsbl$Station)),
                        radioButtons("time_resolution", "Time Resolution:", choices = c("Daily", "Weekly", "Monthly"), selected = "Daily"),
                        sliderInput("train_ratio", "Train-Test Split:", min = 0.6, max = 0.9, value = 0.8, step = 0.05),
                        checkboxGroupInput("models", "Select Forecasting Models:",
                                           choices = c("STL + Naive" = "STLNaive",
                                                       "STL + ARIMA" = "STLArima",
                                                       "STL + ETS" = "STLETS",
                                                       "Auto ARIMA" = "AUTOARIMA",
                                                       "Auto Prophet" = "AUTOprophet",
                                                       "Auto ETS" = "AUTOETS"),
                                           selected = c("STLNaive", "AUTOARIMA")
                        ),
                        actionButton("run_forecast", "Generate Forecast")
                      )
               ),
               column(width = 10,
                      plotlyOutput("forecast_plot", height = "500px")
               )
             )
           )
  )
)

# Server
server <- function(input, output, session) {
  
  # --- Tab 1: Visualization ---
  selected_data <- reactive({
    data <- weather_tsbl %>%
      filter(Station %in% input$station,
             Date >= input$daterange[1],
             Date <= input$daterange[2])
    
    if (input$resolution == "week") {
      data %>%
        mutate(Period = floor_date(Date, "week")) %>%
        group_by(Station, Period) %>%
        summarise(Value = mean(.data[[input$variable]], na.rm = TRUE), .groups = "drop")
    } else if (input$resolution == "month") {
      data %>%
        mutate(Period = floor_date(Date, "month")) %>%
        group_by(Station, Period) %>%
        summarise(Value = mean(.data[[input$variable]], na.rm = TRUE), .groups = "drop")
    } else {
      data %>%
        mutate(Period = Date, Value = .data[[input$variable]]) %>%
        select(Station, Period, Value)
    }
  })
  
  output$ts_plot <- renderPlotly({
    p <- ggplot(selected_data(), aes(x = Period, y = Value, color = Station)) +
      geom_line(size = 1) +
      labs(title = paste("Time Series of", input$variable),
           x = "Date", y = input$variable,
           color = "Station") +
      theme_minimal()
    
    ggplotly(p, source = "tsplot")
  })
  
  observeEvent(event_data("plotly_legendclick", source = "tsplot"), {
    clicked_station <- event_data("plotly_legendclick", source = "tsplot")$name
    if (!is.null(clicked_station)) {
      updateSelectInput(session, "station", selected = clicked_station)
    }
  })
  
  rain_totals_weekly <- reactive({
    req(input$variable == "Daily Rainfall Total (mm)")
    weather_tsbl %>%
      filter(Station %in% input$station,
             Date >= input$daterange[1],
             Date <= input$daterange[2]) %>%
      mutate(Week = floor_date(Date, "week")) %>%
      group_by(Station, Week) %>%
      summarise(Weekly_Rainfall = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop")
  })
  
  rain_totals_monthly <- reactive({
    req(input$variable == "Daily Rainfall Total (mm)")
    weather_tsbl %>%
      filter(Station %in% input$station,
             Date >= input$daterange[1],
             Date <= input$daterange[2]) %>%
      mutate(Month = floor_date(Date, "month")) %>%
      group_by(Station, Month) %>%
      summarise(Monthly_Rainfall = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop")
  })
  
  output$weekly_rain_tbl <- DT::renderDataTable({
    rain_totals_weekly()
  })
  
  output$monthly_rain_tbl <- DT::renderDataTable({
    rain_totals_monthly()
  })
  
  # --- Tab 2: Forecasting ---
  observeEvent(input$run_forecast, {
    output$forecast_plot <- renderPlotly({
      selected_var <- input$forecast_var
      station_data <- weather_tsbl %>%
        filter(Station == input$forecast_station) %>%
        select(Date, value = all_of(selected_var))
      
      total_n <- nrow(station_data)
      train_n <- floor(total_n * input$train_ratio)
      train_data <- station_data[1:train_n, ]
      test_data <- station_data[(train_n + 1):total_n, ]
      h <- nrow(test_data)
      
      train_ts <- as_tsibble(train_data, index = Date)
      test_ts <- as_tsibble(test_data, index = Date)
      
      # Model definitions
      fitted_models <- train_ts %>%
        model(
          STLNaive = decomposition_model(STL(value), NAIVE(season_adjust)),
          STLArima = decomposition_model(STL(value), ARIMA(season_adjust)),
          STLETS = decomposition_model(STL(value), ETS(season_adjust ~ season("N"))),
          AUTOARIMA = ARIMA(value),
          AUTOprophet = prophet(value),
          AUTOETS = ETS(value)
        )
      
      fc <- forecast(fitted_models, h = h) %>%
        filter(.model %in% input$models)
      
      p <- autoplot(train_ts, value, color = "black") +
        autolayer(test_ts, value, series = "Test Data", color = "red", size = 1) +
        autolayer(fc, level = NULL, size = 1) +
        labs(
          title = paste("Forecast for", selected_var, "at", input$forecast_station),
          x = "Date", y = selected_var,
          color = "Model"
        ) +
        theme_minimal()
      
      ggplotly(p, tooltip = c("x", "y", ".model"))
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
