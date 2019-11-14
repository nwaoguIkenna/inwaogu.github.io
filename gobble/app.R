
library(shiny)
library(tidyverse)
library(dplyr)
library(plyr)
library(maps)
library(class)
library(caret)
library(gridExtra)
library(grid)
library(ggplot2)
library(GGally)
# read in data
beers = read.csv('~/datascience/DS6306/html/build/data/Beers.csv', header = TRUE) # Import Beers data
brew = read.csv('~/datascience/DS6306/html/build/data/Breweries.csv',header = TRUE) # Import Breweries data
map = read.csv('~/datascience/DS6306/html/build/data/us-zip-code-latitude-and-longitude.csv',header = TRUE,sep=";")  # Import Zipcode USA Coordinates

beers <- beers %>% filter(!is.na(ABV))
beers <- beers %>% filter(!is.na(IBU))
mergeData = inner_join(brew,beers, by = c("Brew_ID" = "Brewery_id"))
mergeData <- dplyr::rename(mergeData, brewName = Name.x)
mergeData <- dplyr::rename(mergeData, beerName = Name.y)

ui <- fluidPage(
  
  # Application title
  titlePanel("Histogram of IBU"),
  
  # Sidebar with a slider input for number of bins 

  selectInput(inputId = "dataset",
              label = "Make a Histogram:",
              choices = c("Select", "Histogram")),

    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    if (input$dataset != "Select") {
      
    
    IBU_Content   <- mergeData$IBU
    State <- mergeData$State
   
    
    # draw the histogram with the specified number of bins
    mergeData %>% ggplot(aes(fill=State, y=IBU_Content, x=State)) + geom_bar(stat="identity") + labs(title = 'Median IBU By State')}
  })
}

# Run the application 
shinyApp(ui, server)