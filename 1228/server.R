
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("readdata.R")

function(input, output) {
  output$myMap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
     })
  
observe({
  leafletProxy("map", data = houseprice) %>%
    cleanShapes() %>%
    addCircles(~longitude, ~latitude, radius=radius, layerId= ~)
})
  output$testSelect <- renderText({
    test = paste0("getvalue <- houseprice$", input$features)
    print(test)
  })  

  
  output$hisCentile <- renderPlot({
    id = which(colnames(houseprice) == input$features)
    getvalue <- houseprice[,id]
    hist(getvalue)
    
  #test = paste0("getvalue <- houseprice$", input$features)  
  #eval
  #eval(parse(test = "getvalue <- houseprice$price"))
  })
}
