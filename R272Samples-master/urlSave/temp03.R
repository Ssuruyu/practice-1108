rm(list=ls(all=TRUE))
library(XML)
library(bitops)
library(RCurl)
library(httr)

orgURL = 'https://buy.yungching.com.tw/region/%E5%8F%B0%E5%8C%97%E5%B8%82-%E5%A4%A7%E5%AE%89%E5%8D%80_c/?pg='

startPage = 1
endPage = 2
alldata = data.frame()
for( i in startPage:endPage)
{
  HOUSEPRICEURL <- paste(orgURL, i, sep='')
  urlExists = url.exists(orgURL)
  
  if(urlExists)
  {
    html = getURL(HOUSEPRICEURL, ssl.verifypeer = FALSE)
    xml = htmlParse(html, encoding ='utf-8')
    address = xpathSApply(xml, "//li[@class=\"m-list-item\"]/a", xmlGetAttr, "title")
    url = xpathSApply(xml, "//li[@class=\"m-list-item\"]/a", xmlGetAttr, "href")
    housetypeTemp = xpathSApply(xml, "//ul[@class=\"item-info-detail\"]/li", xmlValue)
    totalprice = xpathSApply(xml, "//span[@class=\"price-num\"]", xmlValue)
    housetypeTemp = gsub("\r\n", "", housetypeTemp)
    housetypeTemp = gsub(" ", "", housetypeTemp)
    houseid = seq(1,length(housetypeTemp),by=9)
    housetype = housetypeTemp[houseid]
    tempdata = data.frame(address,url,housetype,totalprice)
    alldata = rbind(alldata, tempdata)
  }
}

write.csv(alldata,"alldata.csv")