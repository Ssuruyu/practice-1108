rm(list=ls(all=TRUE))
library(XML)
library(bitops)
library(RCurl)
library(httr)

alldata = data.frame()

startYear = 2015
endYear = 2016
tempY = startYear:endYear

orgURL1 = 'http://www.gck99.com.tw/gold1.php?yy='
orgURL2 = '&mm='

for (y in tempY) 
{
  orgURL <- paste(orgURL1, y, orgURL2, sep = "")

  #orgURL = 'http://www.gck99.com.tw/gold1.php?yy=2015&mm='
  
  startPage = 1
  endPage = 12
  
  temp = startPage:endPage
  strid = sprintf("%02d", temp)
  
  for( i in startPage:endPage)
  {
      goldURL <- paste(orgURL, strid[i], sep='')
      urlExists = url.exists(goldURL)
      
      if(urlExists)
      {
        html = getURL(goldURL, ssl.verifypeer = FALSE)
        xml = htmlParse(html, encoding ='utf-8')
        datetemp = xpathSApply(xml, "//tr[@class='main_1']//td[1]", xmlValue)
        date = substr(datetemp,1,10)
        price = xpathSApply(xml, "//tr[@class='main_1']//td[2]", xmlValue)
        tempdata = data.frame(date, price)
      }
      alldata = rbind(alldata, tempdata)
  }
}

write.csv(alldata,"GOLD.csv")