rm(list=ls(all=TRUE))
library(XML)
library(bitops)
library(RCurl)
library(httr)

ferURL = 'http://www.cbc.gov.tw/lp.asp?CtNode=644&CtUnit=307&BaseDSD=32&mp=1&nowPage=1&pagesize=30'

alldata = data.frame()

urlExists = url.exists(ferURL)
  
if(urlExists)
{
  html = getURL(ferURL, ssl.verifypeer = FALSE)
  xml = htmlParse(html, encoding ='utf-8')
  datetemp = xpathSApply(xml, "//td[1]", xmlValue)
  date = datetemp[-1]
  FXRtemp = xpathSApply(xml, "//td[2]", xmlValue)
  FXR = FXRtemp[-1]
  tempdata = data.frame(date, FXR)
}
alldata = rbind(alldata, tempdata)

write.csv(alldata,"FXR.csv")