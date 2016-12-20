library(xts)
library(zoo)
library(quantmod)
library(TTR)
library(reshape2)
library(ggplot2)

getSymbols("IBM", source="google")
ibm <- merge(Cl(IBM), MACD(Cl(IBM), 8, 17, 9, "EMA", FALSE))
ibm$macdOsc <- ibm$macd - ibm$signal
tail(ibm)

getSymbols("2301.TW", src="yahoo", from="2016-01-01", to="2016-08-01")
Stock = `2301.TW`
names(Stock) <- c("open", "high", "low", "close", "volume", "adjusted")

# KD-line
stochOSC <- stoch(Stock[,c("high","low","close")], nFastK=9)
kd = cbind(stochOSC[-(1:12),1], stochOSC[-(1:12),3])

n = length(kd[,1])
kbigger = ifelse(kd[,1]>=kd[,2], 1, 0)
kdpoint = data.frame(as.numeric(kbigger[1:(n-1),1]) - as.numeric(kbigger[2:n,1]))
names(kdpoint) <- c("cross")
kdcross = data.frame(kd[-1,], kdpoint)
kdcross = data.frame(kdcross, Stock[-(1:13),])
m = length(kdcross[,1])

trade = data.frame() # buy price, sell price
return = 0
i = 1
while( i <= m  )
{
  if( kdcross[i,3] == -1 && i+1 <= m )
  {
    buy = kdcross[i+1, 4]
    i = i + 1
    while( i <= m )
    {
      if( kdcross[i,3] == 1 )
      {
        sell = kdcross[i+1, 4]
        break
      }
      i = i + 1
    }
    tradeTemp = cbind(buy, sell)
    trade = rbind(trade, tradeTemp)
    return = return - buy + sell
  }
  i = i + 1
}

subreturn = cbind(trade, trade[,2] - trade[,1])