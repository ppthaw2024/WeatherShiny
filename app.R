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
  ) %>%
  ungroup()

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
                                                plotlyOutput("future_forecast_plot", height = "500px")
                                         )
                                       )
                                     )
                            )
                 )
)

# Server
server <- function(input, output, session) {
  
  # Time Series Visualization ----
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
  
  # Future Forecast (Refitted) ----
  observeEvent(input$run_future, {
    req(input$full_models)
    
    period_unit <- isolate(input$horizon_unit)
    
    raw_data <- weather_tsbl %>%
      filter(Station == input$full_station)
    
    if (input$full_var == "Mean Temperature (°C)" && period_unit %in% c("week", "month")) {
      full_ts <- raw_data %>%
        mutate(period = floor_date(Date, unit = period_unit)) %>%
        index_by(period) %>%
        summarise(value = mean(`Mean Temperature (°C)`, na.rm = TRUE), .groups = "drop") %>%
        rename(Date = period)
      
    } else if (input$full_var == "Daily Rainfall Total (mm)" && period_unit %in% c("week", "month")) {
      full_ts <- raw_data %>%
        mutate(period = floor_date(Date, unit = period_unit)) %>%
        index_by(period) %>%
        summarise(value = sum(`Daily Rainfall Total (mm)`, na.rm = TRUE), .groups = "drop") %>%
        rename(Date = period)
      
    } else {
      full_ts <- raw_data %>%
        select(Date, value = all_of(input$full_var))
    }
    
    full_ts <- as_tsibble(full_ts, index = Date)
    
    full_fit <- full_ts %>%
      model(
        STLNaive = decomposition_model(STL(value ~ season(window = "periodic")), NAIVE(season_adjust)),
        STLArima = decomposition_model(STL(value ~ season(window = "periodic")), ARIMA(season_adjust)),
        STLETS = decomposition_model(STL(value ~ season(window = "periodic")), ETS(season_adjust ~ season("N"))),
        AUTOARIMA = ARIMA(value),
        AUTOprophet = prophet(value),
        AUTOETS = ETS(value)
      ) %>%
      select(all_of(input$full_models))
    
    horizon_str <- paste0(input$full_horizon, " ", period_unit)
    future_fc <- forecast(full_fit, h = horizon_str)
    
    output$future_forecast_plot <- renderPlotly({
      var_label <- names(variables_select)[variables_select == input$full_var]
      title_txt <- paste("Future Forecast for", var_label, "of", input$full_station)
      
      p <- autoplot(future_fc, level = 95) +
        labs(title = title_txt, x = "Date", y = var_label) +
        theme_minimal()
      
      ggplotly(p, tooltip = c("x", "y", ".model"))
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
