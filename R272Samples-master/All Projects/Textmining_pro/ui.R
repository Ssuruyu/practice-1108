
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
Sys.setlocale(category = "LC_ALL", locale = "cht")
shinyUI(navbarPage( "MoneyDJ Forum Analysis",
                   tabPanel("Text Mining outcome",
  img(src="R_Picture.png", height = 150, width = 250),
  sidebarLayout(
    sidebarPanel(
      selectInput("month", "Please Select Month :", 
                  choices = c("April","May","June","July","August")),
      hr(),
      sliderInput("freq","Please choose top words that you want to know :",
                  min = 30,  max = 100,  value = 50, step=5),
      hr(),

      sliderInput("value","Please select top words that you want to know :",
                  min = 5,  max = 25,  value = 15, step=1),
      selectInput("words","Please select the words :"
                  ,choices = c("蘋果","股市") ),
      hr(),
      sliderInput("correlation","Please select the term's correlation:",
                  min = 0.35,  max = 0.6,  value = 0.35, step=0.05),
      numericInput("kmeansvalue","Please select the cluster :",
                  value=3,min=1,max=7,step=1)
      #actionButton("update","change")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("wordcloud",plotOutput("wordcloud",width ="100%", height = "400px")),
        tabPanel("Highfreqency_terms",plotOutput("highfreqencyterms",width = "100%", height = "400px")),
        tabPanel("TermsCorrelation",plotOutput("TermsCorrelation",width = "100%", height = "400px")),
        tabPanel("Kmeanscluster",plotOutput("Kmeansout",width = "100%", height = "400px"))
    ))
  )),
  tabPanel( "Hot Article Author Analysis",
            sidebarLayout(
              sidebarPanel(
                # selectInput("Month", "Please Select Month :", 
                #             choices = c("April","May","June","July","August")),
                # actionButton("update", "Change"),
                sliderInput("hot_article","Please choose top article that you want to know :",
                            min = 5,  max = 15,  value = 10 , step=1),
                h4("Do you want to know personal writting style with each other?"),
                selectInput("author", "Please select the author :",
                            choices = c("IDIOT101","Alice","lulucasa","朱古力男爵","大昌以琳")),
                br()
              ),
              mainPanel(tabsetPanel(
                tabPanel("Hot author & average viewcount",plotOutput("Hotauthor")),
                tabPanel("Hot athour & article",plotOutput("Hotarticle")),
                tabPanel("Top five athour writting style ",plotOutput("writtingstyle"))
              ))
  )
  ),
  tabPanel("High Frequency Writing Author",
           sidebarLayout(
             sidebarPanel(
               sliderInput("High_Frequency","Please choose top few author that you want to know :",
                           min = 5,  max = 20,  value = 10, step=1),
               h4("Do you want to know personal writting style with each other?"),
               selectInput("frequent_author", "Please select the author :",
                           choices = c("sdanny","大昌秋如","大昌期權天后洪紫瑜","大昌劉澤慧","胡雅惠",
                                       "廖維凌","大昌樹林~游燕玲","鴨寶&押寶","jessica6136","大昌以琳")),
               br()
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("High freqency author",plotOutput("high_freqency_author")),
                 tabPanel("Authoraver_ageviewcount",plotOutput("mean_high_freqency")),
                 tabPanel("Top ten athour writting style ",plotOutput("frequentstyle"))
             )
           ))
)
))
