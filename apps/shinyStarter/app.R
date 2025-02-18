library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(leaflet)

# Define UI
ui <- fluidPage(
  titlePanel("Election Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      selectInput("precinct", "Select Precinct:", choices = NULL, selected = NULL),
      dateRangeInput("date_range", "Select Date Range:", start = Sys.Date()-30, end = Sys.Date())
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", tableOutput("summary_table")),
        tabPanel("Plot", plotOutput("summary_plot")),
        tabPanel("Map", leafletOutput("map"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    data <- read_csv(input$file$datapath)
    updateSelectInput(session, "precinct", choices = unique(data$Precinct))
    return(data)
  })

  filtered_data <- reactive({
    req(dataset())
    dataset() %>%
      filter(Precinct == input$precinct, Date >= input$date_range[1], Date <= input$date_range[2])
  })

  output$summary_table <- renderTable({
    req(filtered_data())
    filtered_data() %>% summarise(
      Total_Voters = sum(Voters, na.rm = TRUE),
      Avg_Workers = mean(Poll_Workers, na.rm = TRUE)
    )
  })

  output$summary_plot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Date, y = Voters)) +
      geom_line() +
      labs(title = "Voter Turnout Over Time", x = "Date", y = "Number of Voters")
  })

  output$map <- renderLeaflet({
    req(filtered_data())
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = -77.0369, lat = 38.9072, popup = input$precinct) # Example coordinates
  })
}

# Run the application
shinyApp(ui = ui, server = server)
