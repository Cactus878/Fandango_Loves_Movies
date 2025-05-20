# Created: 14/05/2025 
# Creator: Yanni Karlaftis
# Purpose: Display the Fandango Loves Movie visual, enhance the original,
# add user interaction and improve the graphs insight.

library(shiny)
library(dplyr)
library(bslib)
library(ggplot2)
library(readxl) 
library(tidyr)

data <- read_excel("final_data.xlsx")

movie_ratings <- read_excel("movie_ratings.xlsx")

# UI 
ui <- page_sidebar(
  title = "Fandango Loves Movies",
  sidebar = sidebar(
    selectInput(
    "movie",
    label = "Choose a movie to display",
    choices = c("-Empty-", movie_ratings$Film),
    selected = NULL)
  ),
  navset_tab( 
    nav_panel("Visualisation", 
      card(
        card_header("Normalized ratings distribution of 113 films in theaters in 2015 that had 30+ reviews."),
        mainPanel(
          plotOutput("ggplot", height = "545px", width = "150%")  # full width of main panel
        )
      ) 
    ),
    nav_panel("Original Report",
      fluidPage(
        tags$p(
          tags$strong("Title: "), 
          "Be Suspicious Of Online Movie Ratings, Especially Fandango’s"
        ),
        tags$p(
          tags$strong("Author: "), 
          "Walt Hickey"
        ),
        tags$p(
          tags$strong("Date: "), 
          "Oct. 15, 2015"
        ),
        tags$p(
          tags$strong("Link: "),
          tags$a(href = "https://fivethirtyeight.com/features/fandango-movies-ratings/", 
                 "View the original article", 
                 target = "_blank")
        )
      )
    ),
    nav_panel("Raw Data",
      tableOutput("movie_table_data")
    )
  ),
  textOutput("selected_movie")
)

# Server
server <- function(input, output) {
  
  selected_movie_data <- reactive({
    movie_ratings %>%
      filter(Film == input$movie) %>%
      pivot_longer(
        cols = c(Fandango_Rating, IMDB_Rating, RT_Rating, Metacritic_Rating, Metacritic_User_Rating, RT_User_Rating),
        names_to = "Source",
        values_to = "Rating"
      ) %>% mutate(Source = case_when(
        Source == "Fandango_Rating" ~ "Fandango",
        Source == "IMDB_Rating" ~ "IMDB",
        Source == "RT_Rating" ~ "Rotten Tomatoes",
        Source == "Metacritic_Rating" ~ "Metacritic",
        Source == "Metacritic_User_Rating" ~ "Metacritic User",
        Source == "RT_User_Rating" ~ "Rotten Tomatoes User"
      ))
    })
  
  output$movie_table_data <- renderTable(movie_ratings) 
  
  output$ggplot <- renderPlot({
    ggplot(data, aes(x = Rating, y = Percent, fill = Source)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ Source) +
      labs(
        x = "Star Rating (0–5 Scale)",
        y = "Percentage of Movies"
      ) +
      scale_x_continuous(breaks = seq(0, 5, 0.5)) +
      theme(axis.text.x = element_text(angle = 90)) +
      #Solve accessibility issue
      scale_fill_manual(values = c(
        "Fandango" = "#d55e00",    
        "Rotten Tomatoes" = "#e69f00",  
        "IMDB" = "#0072b2",        
        "Metacritic" = "#f0e442" ,
        "Rotten Tomatoes User" = "#009e73",
        "Metacritic User" = "#cc79a7"
      )) +
      # Solve insight issue
      geom_vline(
        data = selected_movie_data(), 
        aes(xintercept = Rating), 
        color = "black", linetype = "dashed", size = 1
      )
  })
}

# Run app
shinyApp(ui = ui, server = server)