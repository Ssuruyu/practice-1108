rm(list=ls(all=TRUE))
library(XML)
library(bitops)
library(RCurl)
library(httr)

orgURL1 = 'https://www.settour.com.tw/ec/prodline/hdp/searchList.html?pager.currentPage='
orgURL2 = '&stype=&sp=&sc=&erpOrdInfo=&cond=%E5%8F%B0%E5%8C%97%E5%B8%82%2C%26%2321488%3B%26%2321271%3B&portTp=B2C&pst=&ped=&destinationShow=%E5%8F%B0%E5%8C%97%E5%B8%82&destination=TPE&ps1=&ps2=&pky=%E5%8F%B0%E5%8C%97'
#orgURL = 'https://www.ptt.cc/bbs/StupidClown/index.html'

startPage = 1
endPage = 1
alldata = data.frame()
for( i in startPage:endPage)
{
  pttURL <- paste(orgURL1, i, orgURL2, sep='')
  urlExists = url.exists(pttURL)
  
  if(urlExists)
  {
    html = getURL(pttURL, ssl.verifypeer = FALSE)
    xml = htmlParse(html, encoding ='utf-8')
    
    write(html, "test.html")
    
    hotelname = xpathSApply(xml, "//em", xmlValue)
    urltemp = xpathSApply(xml, "//ul[@class='probox hdp-page']//li/a", xmlGetAttr, 'onclick')
    urlid = seq(1,length(urltemp), by=2)
    url = urltemp[urlid]
    gpsid = seq(2,length(urltemp), by=2)
    gps = urltemp[gpsid]
    price = xpathSApply(xml, "//span[@class='txt-high']/strong", xmlValue)
    price = gsub("\r\n\t\t\t", "", price)
    price = gsub("\t", "", price)
    price = gsub(" ", "", price)
    tempdata = data.frame(hotelname[-1], url, gps, price)
  }
  alldata = rbind(alldata, tempdata)
}

write.csv(alldata,"alldata.csv")