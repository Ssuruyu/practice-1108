library(shiny)
library(ggmap)
library(mapproj)
library(ggplot2)
library(magrittr)
library(leaflet)
source('read7-11maps.R')

shinyServer(function(input, output, session) {

  ShowId <- eventReactive(input$Area, {
    which(stores$dno == as.numeric(input$Area))
  })
  
  observeEvent(input$Area, {
    Id = ShowId()
    if( length(ShowId) > 0 )
    {
      lng.path = stores$lan[Id]
      lat.path = stores$lat[Id]
      leafletProxy('mymap',session) %>% clearMarkers() %>% addMarkers(lng=lng.path,lat=lat.path)
    }
  })
  
  output$mymap <- renderLeaflet({
    markers711 <- leaflet() %>% 
      addTiles() %>%
      setView(121.5467, 25.05248, zoom = 13)
    markers711
  })
  
  terms <- reactive({
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })

  selectedData <- reactive({
    dat[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
})