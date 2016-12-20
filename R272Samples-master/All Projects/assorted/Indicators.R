library("quantmod")
library("xts")
library("zoo")
library("TTR")
library("PerformanceAnalytics")

# getSymbols("AAPL",from ="2016-01-05",to = "2016-08-25")
# one = get("AAPL")
# save(one,file="data.Rda")

load("data.Rda")
# 均線指標 #
MA_5<-runMean(one[,4],n=5)                                        
MA_10<-runMean(one[,4],n=10)
MA_20<-runMean(one[,4],n=20)
MA_60<-runMean(one[,4],n=60)

# MACD指標 #
close<-Cl(one)                                                     
DIF<-EMA(close,n=12)-EMA(close,n=26)
DEM<-EMA(DIF,n=9)
OSC<-DIF-DEM

# KD指標 #
KD<-100*stoch(HLC(one),nFastK=9,nFastD=3,nSlowD=3)                 

Long_Position<-which(diff(sign(MA_5-MA_20)) == 2)                     
Short_Position<-which(diff(sign(MA_5-MA_20)) == -2)

# 回測圖 #
sig<-ifelse(MA_5-MA_20,1,0)                                             
sig<-Lag(sig)
roc<-ROC(type='discrete',close)
ret<-roc*sig

# 均線指標 x3 #
MA_5<-runMean(one[,4],n=5)                                         
MA_10<-runMean(one[,4],n=10)
MA_20<-runMean(one[,4],n=20)

Long_Position<-which(diff(sign(MA_10-MA_20)) == 2)              
Short_Position<-which(diff(sign(MA_10-MA_20)) == -2)
sig<-ifelse(MA_10-MA_20,1,0) 

# Long_Position<-which(diff(sign(MA_10[,1]-MA_10[,2])) == 2)                    
# Short_Position<-which(diff(sign(MA_10[ ,1]-MA_10[ ,2])) == -2)
# sig<-ifelse(MA_10[ ,1]-MA_10[ ,2],1,0) 

Long_Position<-which(diff(sign(MA_5-MA_20)) == 2)                     
Short_Position<-which(diff(sign(MA_5-MA_20)) == -2)
sig<-ifelse(MA_5-MA_20,1,0)

# MACD指標 x3 #                                                                  
Long_Position<-which(diff(sign(DIF-DEM)) == 2)                     
Short_Position<-which(diff(sign(DIF-DEM)) == -2)
sig<-ifelse(DIF>DEM,1,0) 

# Long_Position<-which(diff(sign(DIF[,1]-DIF[,2])) == 2)                    
# Short_Position<-which(diff(sign(DIF[,1]-DIF[,2])) == -2)
# sig<-ifelse(DIF[,1]-DIF[,2],1,0) 
# 
# Long_Position<-which(diff(sign(OSC[,1]-OSC[,2])) == 2)                   
# Short_Position<-which(diff(sign(OSC[,1]-OSC[,2])) == -2)
# sig<-ifelse(OSC[,1]-OSC[,2],1,0)

# ADX指標 x1 #
ADX<-ADX(one)                                                      
# Long_Position<-which(diff(sign(((ADX$DIp-ADX$DIn)/(ADX$ADX[,1]-ADX$ADX[,2])))) == 2)
# Short_Position<-which(diff(sign(((ADX$DIp-ADX$DIn)/(ADX$ADX[,1]-ADX$ADX[,2])))) == -2)
# sig<-ifelse(((ADX$DIp-ADX$DIn)/(ADX$ADX[,1]-ADX$ADX[,2])),1,0)

# ATR指標 #
ATR<-ATR(one)                                                      

# BBands指標 x1 #
BBands<-BBands(Cl(one))                                            
Long_Position<-which(diff(sign(Cl(one)-BBands$up)) == 2)                
Short_Position<-which(diff(sign(Cl(one)-BBands$up)) == -2)
sig<-ifelse((Cl(one)-BBands$up),1,0)  

# CCI指標 x1 #
CCI<-CCI(Cl(one))                                                  
Long_Position<-which(diff(sign(CCI$cci)) == 2)                  
Short_Position<-which(diff(sign(CCI$cci)) == -2)
sig<-ifelse(CCI$cci,1,0) 

# CMF指標 #
CMF<-CMF(one,Vo(one))                                              
# CMO指標 #
CMO<-CMO(Cl(one))                                                  
# DEMA指標 #
DEMA<-DEMA(Cl(one))                                                
# DPO指標 #
DPO<-DPO(Cl(one))                                                 

# EMA指標 #
EMA<-EMA(Cl(one))                                                  

# ?Envelope指標 #
                                                  
# EVWMA指標 #
EVWMA<-EVWMA(Cl(one),Vo(one))                                       

# ?Expiry指標 #
                                                    
# ?MACD指標 #
                                                    
# momentum指標 #
momentum<-momentum(Cl(one))                                         
# ROC指標 #
ROC<-ROC(Cl(one))                                                   

# RSI指標 #
RSI<-RSI(Cl(one))                                                   
# SAR指標 x1 #
SAR<-SAR(one)                                                       
# Long_Position<-which(diff(sign(SAR[,1]-SAR[,2])) == 2)                 
# Short_Position<-which(diff(sign(SAR[,1]-SAR[,2])) == -2)
# sig<-ifelse(SAR[,1]-SAR[,2],1,0)
# ?WPR指標 #

# KD指標 x2 #
KD<-100*stoch(HLC(one),nFastK=9,nFastD=3,nSlowD=3)                 
Long_Position<-which(diff(sign(KD$fastD-80)) == 2)                 
Short_Position<-which(diff(sign(KD$fastD-80)) == -2)
sig<-ifelse(KD$fastD-80,1,0) 

Long_Position<-which(diff(sign(KD$fastD-KD$slowD)) == 2)                 
Short_Position<-which(diff(sign(KD$fastD-KD$slowD)) == -2)
sig<-ifelse(KD$fastD-KD$slowD,1,0) 