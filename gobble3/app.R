
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
  titlePanel("ABV Plots"),
  
  # Sidebar with a slider input for number of bins 

  selectInput(inputId = "dataset",
              label = "Choose Histogram or Boxplot:",
              choices = c("Boxplot", "Histogram")),

    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    ABV_Content   <- mergeData$ABV
    State <- mergeData$State
    if (input$dataset != "Boxplot") {

    # draw the histogram with the specified number of bins
    mergeData %>% ggplot(aes(fill=State, y=ABV_Content, x=State)) + geom_bar(stat="identity") + labs(title = 'ABV By State')}
  else{mergeData %>% ggplot(aes(fill=State, y=ABV_Content, x=State)) + geom_boxplot(outlier.colour="black", outlier.shape=16,
                    outlier.size=2, notch=FALSE) + labs(title = 'ABV By State')}  
  })
}

# Run the application 
shinyApp(ui, server)