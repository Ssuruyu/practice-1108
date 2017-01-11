library(shiny)

# Define server logic required to draw a histogram

Sys.setlocale(category = "LC_ALL", locale = "zh_TW.UTF-8")
library(readxl)
suppressMessages(library(dplyr))
df <- read_excel("TaiDa_DailyPowerUsage.xlsx")

shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    NTU_Daily_Electricity_Consumption  <- df$`Total Electricity Usage`  # Old Faithful Geyser data
    bins <- seq(min(NTU_Daily_Electricity_Consumption), max(NTU_Daily_Electricity_Consumption), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(NTU_Daily_Electricity_Consumption, breaks = bins, col = 'darkgray', border = 'white')
  })
})