rm(list=ls(all=TRUE))
#test
# install.packages("XML")
# install.packages("bitops")
# install.packages("RCurl")
# install.packages("httr")
install.packages("readr")
library(XML)
library(bitops)
library(RCurl)
library(httr)
library(readr)

orgURL = "https://www.sciencedaily.com/news/science_society/stem_education/"

if(url.exists(orgURL))
{
  alldata = data.frame()
  html = mystring <- read_file("team06Page.txt")
  #html = getURL(orgURL, ssl.verifypeer = FALSE)
  xml = htmlParse(html, encoding ='UTF-8')
  title <- xpathSApply(xml,"//*[@id=\"featured_shorts\"]//li",xmlValue)
  link <- xpathSApply(xml,"//*[@id=\"featured_shorts\"]//li/a//@href")
  # link = orgURL + link
  alldata <- data.frame(title, link)
  write.csv(alldata,"a_ha.csv")
}