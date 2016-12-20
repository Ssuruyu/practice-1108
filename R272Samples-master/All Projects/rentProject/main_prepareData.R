AllData <- read.csv('rent_dist.csv')
ind <- sample(2, nrow(AllData), replace=TRUE, prob=c(0.9,0.1))
ind
trainData <- AllData[ind==1,]
testData <- AllData[ind==2,]
oneV = rep(1, nrow(trainData))
X = as.matrix( cbind(oneV, AllData[ind==1,c(9)]) )
Y = as.matrix( AllData[ind==1, 11] )
Beta = solve(t(X) %*% X) %*% t(X) %*% Y
oneV = rep(1, nrow(testData))
Xpred = as.matrix( cbind(oneV, AllData[ind==2,c(9)]) )