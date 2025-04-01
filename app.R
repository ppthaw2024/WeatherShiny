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
                 
                 # Tab 1: Time Series Visualization
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
                 
                 # Tab 2: Updated Decomposition
                 tabPanel("Decomposition",
                          fluidPage(
                            fluidRow(
                              column(2,
                                     wellPanel(
                                       selectInput("decomp_var", "Weather Variable:", choices = variables_select),
                                       selectInput("decomp_station", "Select Station:", choices = unique(weather_tsbl$Station)),
                                       sliderInput("acf_lag", "ACF/PACF Lag:", min = 10, max = 100, value = 30, step = 1)
                                     )
                              ),
                              column(10,
                                     fluidRow(
                                       column(6, plotOutput("acf_plot", height = "300px")),
                                       column(6, plotOutput("pacf_plot", height = "300px"))
                                     ),
                                     fluidRow(
                                       column(12, plotlyOutput("stl_plot", height = "400px"))
                                     )
                              )
                            )
                          )
                 ),
                 
                 # Tab 3: Forecasting - unchanged from your original
                 navbarMenu("Time Series Forecasting",
                            
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
  
  # --- Tab 1: Visualization ---
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
  
  # --- Tab 2: Decomposition ---
  decomp_data <- reactive({
    weather_tsbl %>%
      filter(Station == input$decomp_station) %>%
      select(Date, value = all_of(input$decomp_var)) %>%
      as_tsibble(index = Date)
  })
  
  output$acf_plot <- renderPlot({
    decomp_data() %>%
      ACF(value, lag_max = input$acf_lag) %>%
      autoplot() +
      ggtitle("ACF Plot") +
      theme_minimal()
  })
  
  output$pacf_plot <- renderPlot({
    decomp_data() %>%
      PACF(value, lag_max = input$acf_lag) %>%
      autoplot() +
      ggtitle("PACF Plot") +
      theme_minimal()
  })
  
  output$stl_plot <- renderPlotly({
    stl_decomp <- decomp_data() %>%
      model(STL(value)) %>%
      components()
    
    p <- autoplot(stl_decomp) +
      ggtitle("STL Decomposition") +
      theme_minimal()
    ggplotly(p)
  })
  
  # --- Tab 3: Forecasting ---
  # Leave this as-is: your full forecast logic from your original code should follow here
  # (unchanged, already implemented and working)
  
}

# Run the app
shinyApp(ui = ui, server = server)
