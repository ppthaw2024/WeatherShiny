# Load packages
pacman::p_load(shiny, tidyverse, lubridate, tsibble, tsibbledata)

# Load data
weather_data <- read_csv("data/weather_data_cleaned.csv")

# Convert to tsibble
weather_tsbl <- as_tsibble(weather_data, key = Station, index = Date)

# Available variables for selection
variables_select <- c(
  "Daily Rainfall Total (mm)" = "Daily Rainfall Total (mm)",
  "Mean Temperature (°C)" = "Mean Temperature (°C)",
  "Minimum Temperature (°C)" = "Minimum Temperature (°C)",
  "Maximum Temperature (°C)" = "Maximum Temperature (°C)"
)

# UI
ui <- fluidPage(
  titlePanel("Time Series Visualization of Singapore Weather"),
  sidebarLayout(
    sidebarPanel(
      selectInput("station", "Select Station:",
                  choices = unique(weather_data$Station),
                  selected = unique(weather_data$Station)[1]),
      selectInput("variable", "Select Variable:", choices = variables_select),
      dateRangeInput("daterange", "Select Date Range:",
                     start = min(weather_data$Date),
                     end = max(weather_data$Date),
                     min = min(weather_data$Date),
                     max = max(weather_data$Date)),
      radioButtons("resolution", "Time Resolution:",
                   choices = c("Daily" = "day", "Weekly" = "week"),
                   selected = "day")
    ),
    mainPanel(
      plotOutput("ts_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  selected_data <- reactive({
    data <- weather_tsbl %>%
      filter(Station == input$station,
             Date >= input$daterange[1],
             Date <= input$daterange[2])
    
    if (input$resolution == "week") {
      data %>%
        index_by(Week = ~ floor_date(., "week")) %>%
        summarise(Value = mean(.data[[input$variable]], na.rm = TRUE))
    } else {
      data %>%
        mutate(Value = .data[[input$variable]])
    }
  })
  
  output$ts_plot <- renderPlot({
    ggplot(selected_data(), aes(x = Date, y = Value)) +
      geom_line(color = "steelblue") +
      labs(title = paste(input$resolution |> str_to_title(), input$variable, "at", input$station),
           x = "Date", y = input$variable) +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
