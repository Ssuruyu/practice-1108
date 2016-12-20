library('e1071')
library('plot3D')
house = read.csv('./SVM/house.csv')

Y = house$price
X = cbind(house$county, house$type, house$year, house$bed, house$living)
n = length(Y)
trainDataID = c(1:(n*0.5))
TrainData = data.frame(Y[trainDataID],X[trainDataID,])
names(TrainData) = c('label', 'county', 'type', 'year', 'bed', 'living')
TestData = data.frame(Y[-trainDataID],X[-trainDataID,])
names(TestData) = c('label', 'county', 'type', 'year', 'bed', 'living')


#tune.svm(label ~ ., data = TrainData, degree = 1:3, gamma = c(0.1,0.9,0.01), cost = 1:10)

