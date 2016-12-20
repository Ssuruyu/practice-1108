library(shiny)
library(ggmap)
library(mapproj)
library(ggplot2)
library(magrittr)
library(leaflet)
source('Word_Cloud.R')

shinyUI(navbarPage("Tom Wu's Project",
        tabPanel("7-11 Maps",
          sidebarLayout(
            sidebarPanel(
              checkboxGroupInput("Area", 
                label = h2("administrative district"), 
                choices = list("1.Songshan"=1,"2.Xinyi"=2,"3.Da-an"=3,
                               "4.Zhongshan"=4,"5.Zhongzheng "=5,"6.Datong"=6,
                               "7.Wanhua"=7,"8.Wenshan"=8,"9.Nangang"=9,
                               "10.Neihu"=10,"11.Shilin"=11,"12.Beitou"=12),
                selected = c(1))),
            mainPanel(
              leafletOutput("mymap", height="600px"))
        )),
        tabPanel("Word Cloud",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("selection", "Please Select:",
                                 choices = books),
                     actionButton("update", "Change"), hr(),
                     sliderInput("freq",
                                 "Minimum Frequency:",
                                 min = 1,  max = 50, value = 15),
                     sliderInput("max",
                                 "Maximum Number of Words:",
                                 min = 1,  max = 300,  value = 100)
                   ),
                   mainPanel(
                     plotOutput("plot"))
                   )),
        tabPanel("K-Means",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput('xcol', 'X Variable', names(dat)),
                     selectInput('ycol', 'Y Variable', names(dat),
                                 selected = names(dat)[[2]]),
                     numericInput('clusters', 'Cluster count', 3,
                                  min = 1, max = 9)
                   ),
                   mainPanel(
                     plotOutput('plot1'))
                 ))
))