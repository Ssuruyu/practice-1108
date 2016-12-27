
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

vars <- colnames(houseprice)

navbarPage("House Price USA", id="hprice",
        tabPanel("show price map",
           leafletOutput("myMap", width = "100%", height = "1092"),
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                         width = 330, height = "auto",
                         h2("ZIP explorer"),
                         
                        selectInput("feature", "Features", vars[3:7])
                         )
         ),
        tabPanel("show XXX")
           )