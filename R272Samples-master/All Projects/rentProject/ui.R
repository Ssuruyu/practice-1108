library(shiny)
library('e1071')
source('main_prepareData.R')
shinyUI(navbarPage("rent price",
                   tabPanel("regression",
                            sidebarLayout(
                              sidebarPanel(
                                checkboxGroupInput("Retailers", label=h2("parameters"),
                                                   choices=list("district"=2,
                                                                "Starbucks"=7,
                                                                "Cosmed"=8,
                                                                "PostOffice"=9,
                                                                "MRT"=10),
                                                   selected = c(2,7:10)),
                                actionButton("SelectAll", label = "SelectAll"),
                                actionButton("DelAll", label = "DelAll")),
                              mainPanel(
                                plotOutput("regression"),
                                dataTableOutput("RentRegression")  
                              )
                              
                            )
                   )
                   ,tabPanel("rent price predict by SVM",
                            sidebarLayout(
                              sidebarPanel(
                                sliderInput("degree", "degree：",
                                            min = 0,
                                            max = 10,
                                            value = 1,step=1),
                                sliderInput("gamma",
                                            "gamma：",
                                            min = 0.01,
                                            max = 1,
                                            value = 0.1,step=0.01),
                                sliderInput("cost",
                                            "cost:",
                                            min = 1,
                                            max = 10,
                                            value = 1,step=1)),
                              mainPanel(
                                plotOutput("svmResult")
                              )
                            )
)))
