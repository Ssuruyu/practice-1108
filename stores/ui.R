
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)
shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      checkboxGroupInput("mapcheck",
                         "select a store name",
                         choices = list("星巴克" = "star",
                                        "康是美"="cosmed",
                                        "郵局"="post",
                                        "咖啡廳"="cafe8mrt",
                                        "捷運站"="mrt"))
                                        
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      tableOutput("showTable"),
      leafletOutput("showMap")
    )
  )
))
