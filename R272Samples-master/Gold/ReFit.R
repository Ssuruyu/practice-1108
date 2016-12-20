library(reshape2)
library(ggplot2)
library(xts)
library(zoo)
library(TTR)
library(quantmod)
library(fArma)

getSymbols("GLD", from='2016-01-04', to='2016-08-10')
getFX("EUR/TWD", from='2016-01-04', to='2016-08-10')
getFX("GBP/TWD", from='2016-01-04', to='2016-08-10')
getFX("USD/TWD", from='2016-01-04', to='2016-08-10')

fxid = index(EURTWD)
goldid = index(GLD)
undelid = c()
for(i in 1:length(fxid))
{
  for(j in 1:length(goldid))
  {
    if( fxid[i] == goldid[j] )
    {
      print(i)
      print(fxid[i])
      print(goldid[j])
      undelid = rbind(undelid, i)    
      break
    }
  }
}

price=data.frame(goldid[undelid], EURTWD[undelid,], GBPTWD[undelid,], USDTWD[undelid,], GLD[,4])
price[,2:5] = log(price[,2:5])

names(price) = c("date", "EUR", "GBP", "USD", "GOLD")
mdf <- melt(price, id.vars="date", value.name="Price", variable.name="FX")
at = seq(1, length(price[,1]), by=10)

ggplot(data=mdf, aes(x=date, y=Price, group=FX, colour=FX)) +
  geom_line() +
  geom_point( size=1, shape=1, fill="white" ) +
  scale_x_discrete(at, mdf$date[at]) +
  xlab("Date")

EURDiff = diff( price$EUR )
EURDiff = as.ts( tail( EURDiff ) )
fit = armaFit( formula=~arma(2,2), data=EURDiff)
fit@fit$aic
as.numeric( predict( fit, n.ahead=1, doplot=F )$pred )

# regression y = b1 x1 + b2 x2 + b3 x3
train = 1:100
predict = 101:153
oneV = rep(1, length(train))
X = as.matrix( cbind(oneV, price[train,2:4]) )
Y = as.matrix( price[train, 5] )
Beta = solve(t(X) %*% X) %*% t(X) %*% Y
oneV = rep(1, length(predict))
Xpred = as.matrix( cbind(oneV, price[predict, 2:4]) )
plot(predict, Xpred%*%Beta)
lines(predict, price[predict,5])
