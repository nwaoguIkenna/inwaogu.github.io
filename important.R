

shinyApp(fluidPage(
  titlePanel("Basic DataTable"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(2,
           selectInput("man",
                       "Manufacturer:",
                       c("All",
                         unique(as.character(mpg$manufacturer))))
    ),
    column(4,
           selectInput("trans",
                       "Transmission:",
                       c("All",
                         unique(as.character(mpg$trans))))
    ),
    column(4,
           selectInput("cyl",
                       "Cylinders:",
                       c("All",
                         unique(as.character(mpg$cyl))))
    )
  ),
  # Create a new row for the table.
  dataTableOutput("table")),
  
  
  function(input, output) {
    
    # Filter data based on selections
    output$table <- renderDataTable(datatable({
      data <- mpg
      if (input$man != "All") {
        data <- data[data$manufacturer == input$man,]
      }
      if (input$cyl != "All") {
        data <- data[data$cyl == input$cyl,]
      }
      if (input$trans != "All") {
        data <- data[data$trans == input$trans,]
      }
      data
    }))
    
  }
)
