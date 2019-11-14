
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

mod.lm <- lm(mergeData$IBU~mergeData$ABV, data = mergeData)
summary(mod.lm)
mod.lm$coefficients[1]
ui <- fluidPage(
  
  # Application title
  
  # Sidebar with a slider input for number of bins 
  
  selectInput("man",
              "State:",
              c("All",
                unique(as.character(sort(mergeData$State))))),
  
  radioButtons("dataset", label = "Scatter with or without a fitted model",
               choices = c("No Model" = 1, "Model" = 2), 
               selected = 1),
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("distPlot")
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    IBU_Content   <- mergeData$IBU
    ABV_Content <- mergeData$ABV
    State <- mergeData$State
    if (input$man == 'All' & input$dataset == 1) {mergeData %>% ggplot(aes( y=IBU_Content, x=ABV_Content)) + geom_point() + labs(title = 'ABV vs IBU w/o Regression Line') }
      
      
  else if (input$man != 'All' & input$dataset == 1)   {mergeData<-mergeData%>% filter(State == input$man) 
  mergeData %>% ggplot(aes( y=IBU, x=ABV)) + geom_point() + labs(title = paste('ABV vs IBU for',as.character(input$man),'w/o Regression Line')) + xlab('ABV_Content') + ylab('IBU_Content')}
        
    else if (input$man != 'All' & input$dataset == 2)   {mergeData<-mergeData%>% filter(State == input$man)
    mod.lm <- lm(mergeData$IBU~mergeData$ABV, data = mergeData)
    mergeData %>% ggplot(aes( y=IBU, x=ABV)) + geom_point() + labs(title = paste('ABV vs IBU for',as.character(input$man),'with Regression Line')) +
      geom_abline(slope=mod.lm$coefficients[2], intercept = mod.lm$coefficients[1], col='blue') + xlab('ABV_Content') + ylab('IBU_Content')}        
        
    else if (input$man == 'All' & input$dataset == 2)   {
    mergeData %>% ggplot(aes( y=IBU, x=ABV)) + geom_point() + labs(title = paste('ABV vs IBU With Regression Line')) +
      geom_abline(slope=mod.lm$coefficients[2], intercept = mod.lm$coefficients[1], col='blue') + xlab('ABV_Content') + ylab('IBU_Content')}})}
    
    

# Run the application 
shinyApp(ui, server)