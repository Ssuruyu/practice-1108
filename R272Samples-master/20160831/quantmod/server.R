
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("getData.R")

shinyServer(function(input, output) {

  output$prices <- renderPlot({
    chartSeries(kdcross[,4:9], theme="white", type='candles')
    addTA(kdcross[,1:2], col=c(1,2))
  })

  output$returns <- renderPlot({
    plot(subreturn[,1], col="blue", ylim=c(min(subreturn[,1:2]), max(subreturn[,1:2])))
    lines(subreturn[,1])
    par(new=TRUE)
    plot(subreturn[,2], col="red", ylim=c(min(subreturn[,1:2]), max(subreturn[,1:2])))
    lines(subreturn[,2])
  })
  
})
