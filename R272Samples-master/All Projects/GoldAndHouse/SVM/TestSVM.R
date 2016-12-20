library('e1071')
totalnp = length(price$date)
np = totalnp * 0.85
return = price$GOLD[2:totalnp] - price$GOLD[1:(totalnp-1)]
Y = return / abs(return)
X = price[1:(totalnp-1),2:5]

index = 1:np
TrainY = data.frame(Y[index])
names(TrainY) = c("label")
TrainX = X[index,]
TrainData = cbind(TrainY, TrainX)

TestData = X[-index,]

svm.model = svm( label ~ ., TrainData, kernal='radial', type = 'C-classification', cost = 10, gamma = 0.5)
svm.pred = predict(svm.model, TestData)

svm.test = table(svm.pred, Y[-index])
correct = sum(diag(svm.test) / sum(svm.test)) * 100