# Load the required libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(viridis)

# Placeholder dataset for demonstration
data <- data.frame(
  City = c("Delhi", "Hyderabad", "Amritsar", "Patna", "Visakhapatnam", "Kolkata", "Chandigarh", "Amaravati", "Gurugram"),
  AQI = c(677, 510, 478, 449, 387, 347, 335, 312, 208),
  PM10 = c(652.21, 95.54, 486.99, 158.08, 196.15, 256.22, 224.22, 187.83, 111.56),
  SO2 = c(15.68, 9.43, 10.48, 10.66, 19.32, 13.07, 9.24, 15.29, 11.87),
  NO2 = c(81.67, 16.83, 12.54, 42.92, 41.63, 77.25, 13.60, 106.10, 44.32),
  CO = c(2.37, 1.17, 0.58, 2.75, 0.73, 1.79, 0.82, 1.43, 0.85),
  O3 = c(86.62, 38.86, 36.15, 33.28, 43.18, 38.31, 16.20, 26.24, 89.44),
  Lat = c(28.704060, 17.385044, 31.633980, 25.594095, 17.686815, 22.57265, 30.53899, 20.93333, 28.457523),
  Long = c(77.102493, 78.486671, 74.872261, 85.137566, 83.218483, 88.36389, 75.95503, 77.75, 77.026344)
)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Interactive Data Explorer"),
  dashboardSidebar(
    selectInput("parameter", "Select Parameter:",
                choices = c("PM10", "SO2", "NO2", "CO", "O3")),
    checkboxInput("showMap", "Show Map", value = FALSE),  # Add a checkbox for showing/hiding the map
    br()
  ),
  dashboardBody(
    fluidRow(
      conditionalPanel(
        condition = "input.showMap == false",  # Show this section if the checkbox is not checked
        div(
          class = "solid-header",
          box(
            title = "Parameter Description",
            width = 12,
            uiOutput("parameter_description")
          )
        ),
        div(
          class = "solid-header",
          box(
            title = "3D-Like Plot",
            width = 6,
            plotOutput("data_plot")
          )
        ),
        div(
          class = "solid-header",
          box(
            title = "Table",
            width = 6,
            tableOutput("data_table")
          )
        )
      ),
      conditionalPanel(
        condition = "input.showMap == true",  # Show this section if the checkbox is checked
        box(
          title = "Map",
          width = 12,
          leafletOutput("data_map")
        )
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Define descriptions for each parameter
  parameter_descriptions <- list(
    "PM10" = "Particulate matter with a diameter of 10 micrometers or smaller. PM10 levels can impact respiratory health.",
    "SO2" = "Sulfur dioxide concentration in the air. SO2 is a pollutant that can cause respiratory issues.",
    "NO2" = "Nitrogen dioxide concentration in the air. NO2 is a contributor to air pollution and can lead to health problems.",
    "CO" = "Carbon monoxide concentration in the air. High levels of CO can affect oxygen delivery to the body's organs and tissues.",
    "O3" = "Ozone concentration in the air. Ground-level ozone can be harmful to health and the environment."
  )
  
  # Render the parameter description based on the user's selection
  output$parameter_description <- renderUI({
    description <- parameter_descriptions[[input$parameter]]
    wellPanel(
      h4("Parameter Description"),
      p(description)
    )
  })
  
  # Create a reactive dataset based on the selected parameter
  selected_data <- reactive({
    data[, c("City", input$parameter, "AQI", "Lat", "Long")]
  })
  
  # Render the table based on the selected parameter
  output$data_table <- renderTable({
    selected_data()
  })
  
  # Render the 3D-like plot based on the selected parameter
  output$data_plot <- renderPlot({
    # Replace this with your 3D-like plot code based on the selected parameter and dataset
    # For 3D-like plots, you may consider using packages like 'plotly' or 'rgl'.
    # Here's a placeholder plot using ggplot2 for demonstration (you will need to customize this):
    ggplot(selected_data(), aes(x = City, y = !!sym(input$parameter), fill = AQI)) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      ylim(0, max(selected_data()[[input$parameter]], na.rm = TRUE) * 1.2) +
      labs(
        title = paste("3D-Like Plot for", input$parameter),
        y = input$parameter,
        x = "City"
      )
  })
  
  # Render the Leaflet map based on the selected parameter and the "Show Map" checkbox
  output$data_map <- renderLeaflet({
    if (input$showMap) {
      aqi_data <- selected_data()
      
      # Normalize AQI values to the range [0, 1]
      min_aqi <- min(aqi_data[[input$parameter]])
      max_aqi <- max(aqi_data[[input$parameter]])
      aqi_data$normalized_aqi <- (aqi_data[[input$parameter]] - min_aqi) / (max_aqi - min_aqi)
      
      # Map the normalized AQI values to the range [1, 10]
      min_radius <- 5
      max_radius <- 30
      aqi_data$radius <- min_radius + (max_radius - min_radius) * aqi_data$normalized_aqi
      
      # Create a Viridis color palette
      color_palette <- viridis(100)
      
      # Create a leaflet map
      map <- leaflet(data = aqi_data) %>%
        addTiles() # Add map tiles
      
      # Add circles with sizes and Viridis color fill
      map <- map %>%
        addCircleMarkers(
          lng = ~Long,
          lat = ~Lat,
          radius = ~radius,
          fillColor = ~color_palette[cut(normalized_aqi, breaks = 50)],
          stroke = FALSE,
          fillOpacity = 0.8,
          popup = ~paste("City: ", City, "<br>", input$parameter, ": ", format(selected_data()[[input$parameter]], big.mark = ','))
        )
      
      # Print the map
      return(map)
    }
  })
}

# Run the Shiny application
shinyApp(ui, server)
