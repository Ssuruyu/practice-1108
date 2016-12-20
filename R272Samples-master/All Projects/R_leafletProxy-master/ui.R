library(shiny)
library(leaflet)

shinyUI(navbarPage("Kirk Lee's Project",
                   tabPanel("Maps",
                    sidebarLayout(
                      sidebarPanel(
                        checkboxGroupInput("Area", label = h2("administrative district"), 
                        choices = list("Starbucks"="star","COSMED"="cosmed","MRT"="mrt"),
                                                   selected = c(1))),
                              mainPanel(
                                leafletOutput("mymap", height="600px"))
                            ))))