#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)
library(stringr)

# =========================
# LOAD DATA (FIXED SCHEMA)
# =========================

parks <- st_read(
  "https://data.austintexas.gov/resource/v8hw-gz65.geojson",
  quiet = TRUE
) %>%
  st_transform(4326) %>%
  mutate(
    
    # ✅ correct column from your dataset
    park_name = as.character(location_name),
    
    # size field already exists
    acreage = as.numeric(acres),
    
    # simulate amenities (since dataset doesn't include them)
    playground = sample(c(TRUE, FALSE), n(), replace = TRUE),
    trails = sample(c(TRUE, FALSE), n(), replace = TRUE),
    restrooms = sample(c(TRUE, FALSE), n(), replace = TRUE),
    
    # stable scoring system (no scale() issues)
    score =
      (acreage / max(acreage, na.rm = TRUE)) * 3 +
      ifelse(playground, 1, 0) +
      ifelse(trails, 1, 0) +
      ifelse(restrooms, 1, 0)
  )

# =========================
# UI
# =========================

ui <- fluidPage(
  
  titlePanel("Austin Parks Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      textInput("search", "Search Park Name:", ""),
      
      sliderInput(
        "acreage",
        "Park Size (Acres):",
        min = 0,
        max = ceiling(max(parks$acreage, na.rm = TRUE)),
        value = c(0, 2000)
      ),
      
      sliderInput(
        "score",
        "Park Score:",
        min = floor(min(parks$score, na.rm = TRUE)),
        max = ceiling(max(parks$score, na.rm = TRUE)),
        value = c(0, 5)
      ),
      
      checkboxGroupInput(
        "amenities",
        "Amenities:",
        choices = c(
          "Playground" = "playground",
          "Trails" = "trails",
          "Restrooms" = "restrooms"
        ),
        selected = c("playground", "trails")
      ),
      
      hr(),
      
      h4("Summary"),
      textOutput("count"),
      textOutput("avg")
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Map", leafletOutput("map", height = 600)),
        
        tabPanel("Distribution", plotOutput("plot"))
      )
    )
  )
)

# =========================
# SERVER
# =========================

server <- function(input, output) {
  
  # ---- FILTERED DATA ----
  filtered <- reactive({
    
    req(input$acreage, input$score)
    
    df <- parks
    
    if (!is.null(input$acreage)) {
      df <- df %>%
        filter(acreage >= input$acreage[1],
               acreage <= input$acreage[2])
    }
    
    if (!is.null(input$score)) {
      df <- df %>%
        filter(score >= input$score[1],
               score <= input$score[2])
    }
    
    if (input$search != "") {
      df <- df %>%
        filter(str_detect(tolower(park_name), tolower(input$search)))
    }
    
    for (a in input$amenities) {
      df <- df %>% filter(.data[[a]] == TRUE)
    }
    
    df
  })
  
  # =========================
  # MAP
  # =========================
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -97.7431, lat = 30.2672, zoom = 11)
  })
  
  observe({
    
    pal <- colorNumeric("YlGn", filtered()$score)
    
    leafletProxy("map", data = filtered()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(score),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        popup = ~paste0(
          "<b>", park_name, "</b><br>",
          "Acres: ", round(acreage, 1), "<br>",
          "Score: ", round(score, 2)
        )
      )
  })
  
  # =========================
  # SUMMARY
  # =========================
  
  output$count <- renderText({
    paste("Parks shown:", nrow(filtered()))
  })
  
  output$avg <- renderText({
    paste("Average score:", round(mean(filtered()$score, na.rm = TRUE), 2))
  })
  
  # =========================
  # PLOT
  # =========================
  
  output$plot <- renderPlot({
    
    ggplot(filtered(), aes(x = score)) +
      geom_histogram(fill = "darkgreen", bins = 25) +
      theme_minimal() +
      labs(
        title = "Park Score Distribution",
        x = "Score",
        y = "Count"
      )
  })
}

# =========================
# RUN APP
# =========================

shinyApp(ui, server)