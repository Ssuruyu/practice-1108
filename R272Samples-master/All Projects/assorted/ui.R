
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('Indicators.R')

shinyUI(navbarPage("Assorted Projects",
                   tabPanel("Indicators",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("selectIndicators", label = h2("Select"),
                                           choices = list("Indicators" = 1, "Performance" = 2))),
                             mainPanel(
                               plotOutput("IndicatorsOut")
                               )
                           )),
                   
                   tabPanel("Summary",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("risk", label = h2("Select"),
                                            choices = list("DownsideRisk" = 1, "Drawdowns" = 2))),
                              mainPanel(
                                dataTableOutput("table1")
                              )
                            )),    
                   # tabPanel("house price by SVM and word Cloud",
                   #          sidebarLayout(
                   #            sidebarPanel(
                   #              selectInput("SVMPrems", label = h2("garmma set"),
                   #                          choices = list("0.1"=0.1, "0.5"=0.5, "0.9"=0.9))),
                   #            mainPanel(
                   #              plotOutput("svmResultHOUSE"),
                   #              imageOutput("wordCloud")
                   #            )
                   #          )),
                   # tabPanel("Regression and Test",
                   #          sidebarLayout(
                   #            sidebarPanel(
                   #              selectInput("selectFX", label = h2("Select FX"), 
                   #                          choices = list("EUR" = 2, "GBP" = 3,
                   #                                         "USD" = 4)),
                   #              checkboxGroupInput("Type", label=h2("Targets"),
                   #                                 choices=list("GOLD"=5,
                   #                                              "EUR"=2,
                   #                                              "GBP"=3,
                   #                                              "USD"=4),
                   #                                 selected = 2),
                   #              actionButton("SelectAll", label = "SelectAll"),
                   #              actionButton("DelAll", label = "DelAll")),
                   #            mainPanel(
                   #              plotOutput("fxToGold"),
                   #              dataTableOutput("fxTest"),
                   #              plotOutput("allPrices"),
                   #              plotOutput("regression"))
                   #          )
                   # ),
                   tabPanel("Movies Data",
                            mainPanel(
                              dataTableOutput("moviedata")
                            ))
))
