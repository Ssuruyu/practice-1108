
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source("getData.R")

shinyUI(mainPanel(
      plotOutput("prices"),
      plotOutput("returns")
    )
)