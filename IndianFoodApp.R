library(shiny)
library(leaflet)
library(rsconnect)


# Load the Indian food dataset
indian_food <- read.csv("Book1.csv") # Replace "path/to/indian_food_dataset.xlsx" with the actual path to your dataset

state_coords <- data.frame(
  state = c("West Bengal", "Rajasthan", "Punjab", "Uttar Pradesh", "Gujarat", 
            "Odisha", "Maharashtra", "Uttarakhand", "Assam", "Bihar", 
            "Andhra Pradesh", "Karnataka", "Telangana", "Kerala", 
            "Tamil Nadu", "Tripura", "Manipur", "Nagaland", "NCT of Delhi", 
            "Jammu & Kashmir", "Chhattisgarh", "Haryana", "Madhya Pradesh", 
            "Goa","0"),
  latitude = c(22.9868, 27.0238, 31.1471, 27.1303, 22.2587, 20.9517, 19.7515,
               30.0668, 26.2006, 25.0961, 15.9129, 15.3173, 18.1124, 10.8505, 
               11.1271, 23.9408, 23.9408, 26.1584, 28.7041, 28.6139, 34.0479, 
               21.2787, 28.7041, 22.9734, 15.2993),
  longitude = c(87.8550, 74.2179, 75.3412, 80.8597, 71.1924, 85.0985, 75.7139,
                79.0193, 92.9376, 85.3131, 79.7400, 75.7139, 79.0193, 76.2711,
                79.0193, 91.9882, 91.9882, 94.5624, 77.1025, 77.2090, 77.2636,
                81.8661, 77.1025, 78.6569, 74.1240)
)

#state_coords=data.frame(state,latitude,longitude)
#View(state_coords)


# Change the cell name for latitude and longitude for multiple states
state_coords[state_coords$state == "Jammu & Kashmir", "latitude"] <- 34.0479
state_coords[state_coords$state == "Jammu & Kashmir", "longitude"] <- 76.7729

state_coords[state_coords$state == "Chhattisgarh", "latitude"] <- 21.2787
state_coords[state_coords$state == "Chhattisgarh", "longitude"] <- 81.8661

state_coords[state_coords$state == "Haryana", "latitude"] <- 29.0588
state_coords[state_coords$state == "Haryana", "longitude"] <- 76.0856

state_coords[state_coords$state == "Madhya Pradesh", "latitude"] <- 22.9734
state_coords[state_coords$state == "Madhya Pradesh", "longitude"] <- 78.6569

state_coords[state_coords$state == "Goa", "latitude"] <- 15.2993
state_coords[state_coords$state == "Goa", "longitude"] <- 74.1240

state_coords=state_coords[1:nrow(state_coords)-1,]



indian_food <- merge(indian_food, state_coords, by = "state", all.x = TRUE)

# Define UI for shiny app
ui <- fluidPage(
  
  tags$style(
    HTML(
      "
      body {
        background-image: url('https://c8.alamy.com/comp/PWAJ2R/hand-drawn-sketch-style-indian-food-isolated-vector-illustration-PWAJ2R.jpg');
        background-size: cover;
        background-position: top;
        background-repeat: no-repeat;
      }
       
      /* Style for all tab headers */
      .nav-tabs > li > a {
        color: black; /* Set a default color for tab text */
        background-color: #f5f5f5; /* Set a default background color for tabs */
        border-color: #ddd; /* Set border color */
        border-style: solid; /* Set border style */
        border-width: 1px; /* Set border width */
        padding: 8px 16px; /* Add some padding */
        font-weight: bold; /* Make tab text bold */
      }
      
      /* Style for active tab header */
      .nav-tabs > li.active > a {
        color: #fff !important; /* Override default color for active tab text */
        background-color: #4CAF50 !important; /* Set background color for active tab */
        border-color: #ddd;
      }
      
      /* Style for tab header hover */
      .nav-tabs > li > a:hover {
        background-color: #e7e7e7; /* Set background color on hover */
      }
      .myCustomTab3 {
        background-color: #F5E8DD;  /* Change this to your desired color */
        opacity: 0.92;
      }
     
      "
    )
  ),
  
  titlePanel("TASTE OF INDIA"),  
  
  
  # Define tabs
  tabsetPanel(
    
    # Recipe Search tab
    tabPanel("Recipe Search",
             sidebarLayout(
               sidebarPanel(
                 textInput("dish_name", "Search by Dish Name"),
                 selectInput("diet_type", "Select Diet Type:",
                             choices = c("All", "vegetarian", "non vegetarian")),
                 selectInput("flavor_profile", "Select Flavor Profile:",
                             choices = c("All", unique(indian_food$flavor_profile[indian_food$flavor_profile != "-1"]))),
                 selectInput("course", "Select Course:",
                             choices = c("All", unique(indian_food$course[indian_food$course != "-1"]))),
                 selectInput("region", "Select Region:",
                             choices = c("All", unique(indian_food$region)))
               ),
               mainPanel(
                 dataTableOutput("filtered_table")
               )
             )
    ),
    
    # Regional Cuisine Explorer tab
    tabPanel("Regional Cuisine Explorer",
             class = "no-background",
             fluidRow(
               leafletOutput("map", height = "1000px") # Increased height of the map
             )
    ),
    
    tabPanel("Ingredients Search",
             class = "myCustomTab3",
             sidebarLayout(
               sidebarPanel(
                 textInput("ingredient1", "Ingredient 1"),
                 textInput("ingredient2", "Ingredient 2"),
                 textInput("ingredient3", "Ingredient 3"),
                 textInput("ingredient4", "Ingredient 4"),
                 textInput("ingredient5", "Ingredient 5"),
                 actionButton("search", "Search")
               ),
               mainPanel(
                 tableOutput("results")
               )
             )
  )
 )
 
 
)
# Define the server logic for the Shiny app
server <- function(input, output) {
  
  # Filter the dataset based on user input
  filtered_data <- reactive({
    filtered <- indian_food
    
    # Filter by dish name
    if (!is.null(input$dish_name) && input$dish_name != "") {
      filtered <- filtered[grep(input$dish_name, filtered$name, ignore.case = TRUE), ]
    }
    
    # Filter by diet type
    if (input$diet_type != "All") {
      filtered <- filtered[filtered$diet %in% input$diet_type, ]
    }
    
    # Filter by flavor profile
    if (input$flavor_profile != "All") {
      filtered <- filtered[filtered$flavor_profile == input$flavor_profile, ]
    }
    
    # Filter by course
    if (input$course != "All") {
      filtered <- filtered[filtered$course == input$course, ]
    }
    
    # Filter by region
    if (input$region != "All") {
      filtered <- filtered[filtered$region == input$region, ]
    }
    
    return(filtered)
  })
  
  # Render the filtered dataset as a table
  output$filtered_table <- renderDataTable({
    filtered_data()
  })
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet(data = indian_food) %>%
      addTiles() %>%
      setView(lng = 78.9629, lat = 22.5937, zoom = 5) %>%
      addCircleMarkers(radius = 8, color = "red", stroke = FALSE, fillOpacity = 0.8,
                       label = ~name, labelOptions = labelOptions(direction = "auto"),
                       popup = ~paste("<b>State:</b>", state, "<br>",
                                      "<b>Number of Dishes:</b>", length(name), " dishes"),
                       clusterOptions = markerClusterOptions())
  })
  
  observeEvent(input$search, {
    # Get input ingredients
    ingredients <- c(input$ingredient1, input$ingredient2, input$ingredient3, input$ingredient4, input$ingredient5)
    ingredients <- tolower(ingredients)
    
    # Function to count matching ingredients for each recipe
    count_matching_ingredients <- function(recipe_ingredients) {
      sum(sapply(ingredients, function(x) grepl(x, tolower(recipe_ingredients))))
    }
    
    # Calculate number of matching ingredients for each recipe
    indian_food$matching_count <- sapply(indian_food$ingredients, count_matching_ingredients)
    
    # Sort recipes by number of matching ingredients (descending order)
    sorted_recipes <- indian_food[order(-indian_food$matching_count), ]
    
    # Filter recipes with at least one matching ingredient
    filtered_recipes <- sorted_recipes[sorted_recipes$matching_count > 0, ]
    
    # Select top 5 recipes with the maximum number of matching ingredients
    top_5_recipes <- head(filtered_recipes[order(-filtered_recipes$matching_count), ], 10)
    
    # Display top 5 recipes
    output$results <- renderTable({
      top_5_recipes[, c("name", "region", "course")]
    })
  })
}

# Run the shiny app
shinyApp(ui = ui, server = server)