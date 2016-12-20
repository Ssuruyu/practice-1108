
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("source.r")

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

  output$showTable <-renderTable({
    showTableId = which(getData$tag == input$mapcheck )
    
    print(getData[showTableId,])
  })
  
  output$showMap <-renderLeaflet({
    showTableId = which(getData$tag == input$mapcheck)
    
    lng = getData$Response_X[showTableId]
    lng = getData[showTableId, 2]
    
    lat = getData$Response_Y[showTableId]
    lat = getData[showTableId, 3]
    
    
    markers <- leaflet() %>%
               addTiles() %>%
               setView(getData$Response_X[1], getData$Response_Y[1], zoom = 13)
    markers <- addMarkers(markers, lng, lat)
    
    markers
  })
  
})
