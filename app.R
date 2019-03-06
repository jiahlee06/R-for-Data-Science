library(shiny)
library(tidyverse)
library(modelr)
library(nycflights13)
library(splines)
library(lubridate)
library(gapminder)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Question 4"),
    # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "right",
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      numericInput(inputId = "num",
                  label = "What time(s) of median do you want as a cutoff?",
                  value = 1)
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      h2("Exercise 24.3.5"),
      h3("Problem 7"),
      h4("We hypothesised that people leaving on Sundays are more likely to be business travellers who need to be somewhere on Monday. Explore that hypothesis."),
      plotOutput(outputId = "distPlot"),
      h4("With the graph showing density of long distance flights only, it is clear that Sunday has highest proportion of long distance flights in evenings.")
    )
))


# Define server logic
server <- function(input, output) {
  
  output$distPlot <- renderPlot({

    flights %>%
      mutate(
        date = make_date(year, month, day),
        wday = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                 "Friday", "Saturday")[as.POSIXlt(date)$wday + 1]) %>%
      filter(
        distance > input$num * median(flights$distance),
        hour >= 5, hour <= 21
      ) %>%
      ggplot(aes(x = hour, color = wday, y = ..density..)) +
      geom_freqpoly(binwidth = 1)
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)