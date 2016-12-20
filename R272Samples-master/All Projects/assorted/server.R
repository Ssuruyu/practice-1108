
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('Indicators.R')

shinyServer(function(input, output, session) {
  
  output$moviedata <- renderDataTable({
    allmovies = read.csv('alldata.csv')
  })
  
  output$IndicatorsOut <- renderPlot(height = 1000, {
    id = as.numeric(input$selectIndicators)
    if( id == 1 )
    {
      chartSeries(one,theme="white",up.col='red',dn.col='green', 
                  TA=c(addVo(),addBBands(), addADX(), addATR(), addCCI(), addCMF(n=20),
                       addCMO(n=14), addDEMA(),addDPO(), addEMA(),addEnvelope(), addEVWMA(),
                       addExpiry(), addMACD(), addMomentum(), addROC(), addRSI(), addSAR(col="maroon"), addWPR()))
      addTA(merge(MA_5, MA_10, MA_20, MA_60), on=1, col=c("deepskyblue", "orange", "indianred3", "limegreen"))
      addTA(merge(DIF,DEM,OSC), col=c("red","blue","limegreen"), on=3, type = c("l","l","h"),lwd = c(1,1,1.5), legend = "MACD")
      addTA(100*stoch(HLC(one),nFastK=9,nFastD=3,nSlowD=3),col=2:4)
      addTA((one[Long_Position,3])*0.95,on=1,type="p",col="blue",pch=24,lwd=2)
      addTA((one[Short_Position,2])*1.05,on=1,type="p",col="red",pch=25,lwd=2)
    }

    if( id == 2 )
    {
      charts.PerformanceSummary(ret)
    }
  })

  output$table1 <- renderDataTable({
    id = as.numeric(input$risk)
    if( id == 2 )
        return(table.Drawdowns(ret,top=5))
    if( id == 1 )
      cbind(row.names(table.DownsideRisk(ret)), table.DownsideRisk(ret))
  })
  
})

