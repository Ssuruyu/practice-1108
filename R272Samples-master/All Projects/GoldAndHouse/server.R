
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(jpeg)

source('./TestAndRegression/ReFit.R')
source('./SVM/TestSVM.R')
source('./SVM/houseSVM.R')
#source('./wordcloud/textMining.R')

shinyServer(function(input, output, session) {
  
  selAllhouse <- observeEvent(input$SelectAllhouse, {
    updateCheckboxGroupInput(session, "houseType", selected = as.character(c(1:5)))
  })
  
  delAllhouse <- observeEvent(input$DelAllhouse, {
    updateCheckboxGroupInput(session, "houseType", selected = c(""))
  })
  
  selAll <- observeEvent(input$SelectAll, {
    updateCheckboxGroupInput(session,"Type", selected=as.character(c(2:5)))
  })
  
  delALL <- observeEvent(input$DelAll, {
    updateCheckboxGroupInput(session,"Type", selected=c(""))
  })  
  
  output$allPrices <- renderPlot({
    
    typeName = c("date", "EUR", "GBP", "USD", "GOLD")
    
    getType = as.numeric(input$Type)
    
    if(length(getType) >=1)
    {
      subPrice = data.frame(price$date, price[,getType])
      names(subPrice) = c("date", typeName[getType])
      
      mdf <- melt(subPrice, id.vars="date", value.name="Price", variable.name="FX")
      
      ggplot(data=mdf, aes(x=date, y=Price, group=FX, colour=FX)) +
        geom_line() +
        geom_point( size=1, shape=1, fill="white" )
    }
  })
  
  output$regression <- renderPlot({
    #regression y = b1 x1 + b2 x2 + b3 x3
    train = 1:100
    predict = 101:153
    oneV = rep(1, length(train))
    Xgold = as.matrix( cbind(oneV, price[train,2:4]) )
    Ygold = as.matrix( price[train, 5] )
    Beta = solve(t(Xgold) %*% Xgold) %*% t(Xgold) %*% Ygold
    oneV = rep(1, length(predict))
    Xpred = as.matrix( cbind(oneV, price[predict, 2:4]) )
    
    
    plot(predict, as.numeric(Xpred%*%Beta), type="l", col="red", ylim = c(100,120))
    lines(predict, as.numeric(price$GOLD[predict]), col="blue")
  })
  
  output$fxToGold <- renderPlot({
    fxselect = as.numeric(input$selectFX)
    subpriceFX <- data.frame(price$GOLD, price[,fxselect])
    names(subpriceFX) = c("GOLD", "FX")
    lmresult <- with(subpriceFX, lm(GOLD ~ FX))
    plot(GOLD ~ FX, data=subpriceFX, main="",
         xlab="FX",
         ylab="GOLD")
    abline(lmresult, lwd=2)
  })
  
  output$fxTest <- renderDataTable({
    typeName = c("date", "EUR", "GBP", "USD", "GOLD")
    fxselect = as.numeric(input$selectFX)
    subpriceFX <- data.frame(price$GOLD, price[,fxselect])
    names(subpriceFX) = c("GOLD", "FX")
    testResult = summary(lm(GOLD ~ ., data = subpriceFX ))
    showName = rbind("Intercept", typeName[fxselect])
    data.frame(showName, testResult$coefficients)
  })
  
  output$svmResult <- renderDataTable({
    outTable = data.frame(svm.test)
    names(outTable) = c("pred", "org", "freq")
    outTable
  })
  
  output$svmResultHOUSE <- renderPlot({
    getGarmma = as.numeric(input$SVMPrems)
    #getGarmma = 0.9
    svm.model = svm( label ~ ., TrainData, kernal='radial', type = 'eps-regression', cost = 1, gamma = getGarmma, degree = 1, epsilon = 0.001)
    svm.pred = predict(svm.model, TestData)
    
    plot(TestData$label, col="red")
    par(new=TRUE)
    plot(svm.pred, col="blue")
    #RMSE = mean( abs(TestData$label - svm.pred) / TestData$label )
  })
  
  output$houseRegression <- renderDataTable({
    nameList = c("County", "Type", "Year", "Bed", "Living")
    getHouseType = as.numeric(input$houseType)
    print(getHouseType)
    X[,3] = floor(X[,3] / 10)
    subX = X[,getHouseType]
    Y = log(Y)
    subData = data.frame(Y, subX)
    names(subData) = c("label", nameList[getHouseType])
    testResult = summary(lm(label ~ ., data = subData ))
    print(testResult$coefficients)
  })
  
  output$wordCloud <- renderImage({
    list(
        src = "./wordcloud/Rplot.jpg",
        filetype = "image/jpeg"
      )}, deleteFile  = FALSE)
  
  #output$wordCloud <- renderPlot({
    #wordcloud(countResult, countFreq, min.freq = 1, random.order = F, ordered.colors = T, 
    #          colors = rainbow(length(countResult)))
  #})
})
