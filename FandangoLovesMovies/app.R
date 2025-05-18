#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(bslib)
library(ggplot2)
library(readxl) 

data <- read_excel("final_data.xlsx")
movie_ratings <- read_excel("movie_ratings.xlsx")


# UI 
ui <- page_sidebar(
  title = "Fandango Loves Movies",
  sidebar = sidebar(
    selectInput(
    "var",
    label = "Choose a movie to display",
    choices = movie_ratings$Film,
    selected = NULL),
  ),
  card(
    card_header("Normalized ratings distribution of 113 films in theaters in 2015 that had 30+ reviews."),
    mainPanel(
      plotOutput("ggplot", height = "600px", width = "100%")  # full width of main panel
    )
  )
)

# Server
server <- function(input, output) {
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
        "IMDB" = "#f0e442",        
        "Metacritic" = "#0072b2" ,
        "Rotten Tomatoes User" = "#009e73",
        "Metacritic User" = "#cc79a7"
      ))
  })
}

# Run app
shinyApp(ui = ui, server = server)