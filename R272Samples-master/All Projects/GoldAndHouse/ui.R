
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('./TestAndRegression/ReFit.R')
source('./SVM/TestSVM.R')
source('./SVM/houseSVM.R')
#source('./wordcloud/textMining.R')

shinyUI(navbarPage("GOLD and FX",
                   tabPanel("house price regression",
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput("houseType", label=h2("House Prices and Data"),
                                                   choices=list("County"=1,
                                                                "Type"=2,
                                                                "Year"=3,
                                                                "Bed"=4,
                                                                "Living"=5),
                                                   selected = 1),
                                actionButton("SelectAllhouse", label = "SelectAll"),
                                actionButton("DelAllhouse", label = "DelAll")),
                              mainPanel(
                                dataTableOutput("houseRegression"))
                            )),
                   
                   tabPanel("house price by SVM and word Cloud",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("SVMPrems", label = h2("garmma set"),
                                            choices = list("0.1"=0.1, "0.5"=0.5, "0.9"=0.9))),
                              mainPanel(
                                plotOutput("svmResultHOUSE"),
                                imageOutput("wordCloud")
                              )
                            )),
                   tabPanel("Regression and Test",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("selectFX", label = h2("Select FX"), 
                                            choices = list("EUR" = 2, "GBP" = 3,
                                                           "USD" = 4)),
                                checkboxGroupInput("Type", label=h2("Targets"),
                                                   choices=list("GOLD"=5,
                                                                "EUR"=2,
                                                                "GBP"=3,
                                                                "USD"=4),
                                                   selected = 2),
                                actionButton("SelectAll", label = "SelectAll"),
                                actionButton("DelAll", label = "DelAll")),
                              mainPanel(
                                plotOutput("fxToGold"),
                                dataTableOutput("fxTest"),
                                plotOutput("allPrices"),
                                plotOutput("regression"))
                            )
                   ),
                   tabPanel("Gold Price by SVM",
                            mainPanel(
                              #plotOutput("svmResultHOUSE")
                              dataTableOutput("svmResult")
                              #plotOutput("wordCloud")
                            ))
))
