library(shiny)
library('e1071')
source('main_prepareData.R')
shinyServer(function(input, output, session) {
  selAll <- observeEvent(input$SelectAll, {
    updateCheckboxGroupInput(session,"Retailers", selected=as.character(c(4,9:12)))
  })
  delALL <- observeEvent(input$DelAll, {
    updateCheckboxGroupInput(session,"Retailers", selected=c(""))
  })  
  output$regression <- renderPlot({
    Retailerselect_n <- as.numeric(input$Retailers)
    ind <- sample(2, nrow(AllData), replace=TRUE, prob=c(0.9,0.1))
    ind
    trainData <- AllData[ind==1,]
    testData <- AllData[ind==2,]
    oneV = rep(1, nrow(trainData))
    X = as.matrix( cbind(oneV, AllData[ind==1,Retailerselect_n]) )
    Y = as.matrix( AllData[ind==1, 11] )
    Beta = solve(t(X) %*% X) %*% t(X) %*% Y
    oneV = rep(1, nrow(testData))
    Xpred = as.matrix( cbind(oneV, AllData[ind==2,Retailerselect_n]) )
    plot(1:nrow(testData), Xpred%*%Beta, type="p", col="red",ylab="rent price / pre lvel ground",xlab="test data",sub="red：predict；blue：original")
    lines(1:nrow(testData), AllData[ind==2,11],type="p", col="blue")
  })
  
  output$RentRegression <- renderDataTable({
    Retailerselect_n <- as.numeric(input$Retailers)
    subX = AllData[,Retailerselect_n]
    Y = AllData[,11]
    subData = data.frame(Y, subX)
    names(subData) = c("Rent", names(AllData[,Retailerselect_n]))
    testResult = summary(lm(Rent ~ ., data = subData ))
    print(testResult$coefficients)
  })
  output$svmResult <- renderPlot({
    degree_user = as.numeric(input$degree)
    gamma_user = as.numeric(input$gamma)
    cost_user = as.numeric(input$cost)
    
    svm.model = svm( FarePerSquare ~ ., AllData[ind==1,], kernal='radial', type = 'eps-regression', cost = cost_user, gamma = gamma_user, degree = degree_user, epsilon = 0.001)
    svm.pred = predict(svm.model, AllData[ind==2,])

    plot(AllData[ind==2,11] , col="blue",ylab="rent price / pre lvel ground",xlab="test data",sub="red：predict；blue：original")
    par(new=TRUE)
    plot(svm.pred, col="red",ylab="rent price / pre lvel ground",xlab="test data",sub="red：predict；blue：original")
  })
  
})
