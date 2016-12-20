library(reshape2)
library(ggplot2)
library(xts)
library(zoo)
library(TTR)
library(quantmod)
library(fArma)

# getSymbols("GLD", from='2015-01-04', to='2015-12-10')
# getFX("EUR/TWD", from='2015-01-04', to='2015-12-10')
# getFX("GBP/TWD", from='2015-01-04', to='2015-12-10')
# getFX("USD/TWD", from='2015-01-04', to='2015-12-10')
# 
# write.csv(EURTWD,"./TestAndRegression/EURTWD.csv")
# write.csv(GBPTWD,"./TestAndRegression/GBPTWD.csv")
# write.csv(USDTWD,"./TestAndRegression/USDTWD.csv")
# write.csv(GLD,"./TestAndRegression/GLD.csv")

EURTWD = read.csv("./TestAndRegression/EURTWD.csv")
GBPTWD = read.csv("./TestAndRegression/GBPTWD.csv")
USDTWD = read.csv("./TestAndRegression/USDTWD.csv")
GLD = read.csv("./TestAndRegression/GLD.csv")

fxid = index(EURTWD)
goldid = index(GLD)
undelid = c()
for(i in 1:length(fxid))
{
  for(j in 1:length(goldid))
  {
    if( fxid[i] == goldid[j] )
    {
      #print(i)
      #print(fxid[i])
      #print(goldid[j])
      undelid = rbind(undelid, i)    
      break
    }
  }
}

price=data.frame(goldid, EURTWD[undelid,2], GBPTWD[undelid,2], USDTWD[undelid,2], GLD[,4])
names(price) = c("date", "EUR", "GBP", "USD", "GOLD")