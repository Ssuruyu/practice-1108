rm(list=ls(all=TRUE))
library(XML)
library(bitops)
library(RCurl)
library(NLP)
library(httr)


#Sys.setlocale("LC_ALL", "cht")


alldata = read.csv('alldata.csv')
orgURL = 'http://www.boxofficemojo.com'
alldataResult = data.frame()
errorid = c()
for( i in 1:length(alldata$link))
{
  tempdata= data.frame
  pttURL <- paste(orgURL, alldata$link[i], sep='')
  urlExists = url.exists(pttURL)
  
  print(i)
  
  if(urlExists)
  {
    html = getURL(pttURL, ssl.verifypeer = FALSE, encoding='UTF-8')
    xml = htmlParse(html, encoding='UTF-8')
    
    #saveXML(xml, "test.html")
    
    tryCatch({
      budg = xpathSApply(xml, "//div[@id='body']/table", xmlValue)
      start = gregexpr("Budget", budg)
      testbudg = substr(budg, start[[1]][1], start[[1]][1]+30)
      testbudg = gsub('\n','',testbudg)
      testbudg = gsub('\t','',testbudg)
      testbudg = gsub(' ','',testbudg)
      testbudg = gsub('[A-Za-z]','',testbudg)
      budg = gsub(':','',testbudg)
      tempdata = data.frame(budg)})
  }
  alldataResult = rbind(alldataResult, tempdata)
}

alldata = data.frame(alldata, alldataResult)

write.csv(alldataResult,"alldata2.csv")