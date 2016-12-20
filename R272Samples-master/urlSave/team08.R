rm(list=ls(all=TRUE))
library(XML)
library(bitops)
library(RCurl)
library(httr)

orgURL1 = 'http://www.gck99.com.tw/gold1.php?yy=2015&mm='

startPage = 1
endPage = 12
alldata = data.frame()
temp = startPage:endPage
strid = sprintf("%02d", temp)

for( i in startPage:endPage)
{
  goldURL1 <- paste(orgURL1, strid[i], sep='')
  urlExists = url.exists(goldURL1)
  
  if(urlExists)
  {
    html = getURL(goldURL1, ssl.verifypeer = FALSE)
    xml = htmlParse(html, encoding ='utf-8')
    date = xpathSApply(xml, "//tr[@class='main_1']//td[1]", xmlValue)
    price = xpathSApply(xml, "//tr[@class='main_1']//td[2]", xmlValue)
    tempdata = data.frame(date, price)
  }
  alldata = rbind(alldata, tempdata)
}

write.csv(alldata,"alldata.csv")