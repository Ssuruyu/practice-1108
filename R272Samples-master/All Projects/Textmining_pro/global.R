library(shiny)
library(Rcpp)
library(jiebaRD)
library(jiebaR)       
library(NLP)
library(tm)           
library(slam)        
library(RColorBrewer)
library(wordcloud)    
library(plyr)
library(dplyr)
library(ggplot2)
library(graphics)
library(plotly)
library(cluster)
library(fpc)
library(SnowballC)

#getnoun <- function(x){
#   index=substr(x,regexpr(pattern="[[:punct:]]",x),nchar(x))
#   noun_position <- which(index %in% c("(n)","(nr)","(nr1)","(nr2)","(nrj)","(nrf)","(ns)"
#                                       ,"(nsf)","(nt)","(nz)","(nl)","(ng)")==TRUE)
#   nountemp <- x[noun_position]
#   noun <- substr(nountemp,1,regexpr(pattern="[[:punct:]]",nountemp)-1)
#   return(noun)
# }         # get noun (data format => list)

getnoun_list <- function(x){                   
  for(i in 1:length(x)){
    index=substr(x[[i]],regexpr(pattern="[[:punct:]]",x[[i]]),nchar(x[[i]]))
    noun_position <- which(index %in% c("(n)","(nr)","(nr1)","(nr2)","(nrj)","(nrf)","(ns)"
                                        ,"(nsf)","(nt)","(nz)","(nl)","(ng)")==TRUE)
    nountemp <- x[[i]][noun_position]
    noun <- substr(nountemp,1,regexpr(pattern="[[:punct:]]",nountemp)-1)
    x[[i]] <- noun
  }
  return(x)
}    # get noun (data format => matrix) 

# getmatrix1 <- function(orgPath){
#   
#   text = Corpus(DirSource(orgPath), list(language = NA))
#   text <- tm_map(text, function(word){ gsub("[A-Za-z0-9]", "", word) })
#   text <- tm_map(text, removePunctuation) 
#   
#   cutter = worker("tag",stop_word=".\\www\\stopword.txt",bylines = T)
#   mat <- matrix( unlist(text), nrow=length(text) )                          # All article transform to matrix
#   
#   article_words = lapply(mat, function(x){
#     tmp = cutter <= x
#     tmp = paste(tmp[[1]], '(', names(tmp[[1]]),')', sep = '')
#     return(tmp)
#   } )
#   
#   article_words <- unlist(article_words)  # need to tranform unlist data format
#   article_words <- getnoun(article_words)
#   article_words<- as.data.frame(article_words)
#   
#   delidx = which( nchar(as.vector(article_words[,1])) < 2 )   #delete world's length less than 2 
#   countText = article_words[-delidx,]
#   
#   return(countText)
# }

getmatrix2 <- function(x){
  text = Corpus(DirSource(x), list(language = NA))
  text <- tm_map(text, function(word){ gsub("[A-Za-z0-9]", "", word) })
  text <- tm_map(text, removePunctuation) 
  
  cutter = worker("tag",stop_word=".\\www\\stopword.txt",bylines = T)
  mat <- matrix( unlist(text), nrow=length(text) )  
  
  article_words = lapply(mat, function(x){
    tmp = cutter <= x
    tmp = paste(tmp[[1]], '(', names(tmp[[1]]),')', sep = '')
    return(tmp)
  } )
  
  article_words <- getnoun_list(article_words)
  article_words <- Corpus(VectorSource(article_words)) 
  
  tdm <- TermDocumentMatrix( article_words, control=list(wordLengths=c(2,Inf)))
  return(tdm)
}

getresult1 <-function(x){                         # find top author and their article's average viewcount
  
  hot_topic <- sort(all_data$viewcount,decreasing = T)[1:x]
  hot_news <- all_data[which(all_data$viewcount %in% hot_topic),]
  hot_news <- hot_news[order(hot_news$viewcount,decreasing = T),]   # top 5 hot news 
  hotarticle_author <- plyr::count(hot_news, names(all_data)[2])
  
  for(i in 1:nrow(hotarticle_author)){
    hot_article <- all_data[which(all_data$author %in% hotarticle_author[i,1]),]
    temp <- hot_article[order(hot_article[,6],decreasing = T)[1],]
    filename <- paste(".\\hot_author\\temp",
                      i,".csv",sep="")
    write.csv(temp,file=filename)
  }
  
  num_file <- length(list.files(path=".\\hot_author",pattern="*.csv"))
  
  for(i in 1:num_file){
    temporg <- paste(".\\hot_author\\temp",i,".csv",sep='')
    if(i==1){
      data <- read.csv(file=temporg,encoding = 'utf-8')
      alldata <- data
    }
    if(i>=2){
      data <- read.csv(file=temporg,encoding = 'utf-8')
      alldata <- rbind(alldata,data)
    }
    write.csv(alldata,file=".\\content\\combine.csv")
  }
  
  tempsort <- read.csv(file=".\\content\\combine.csv")
  tempsort <- tempsort[,-1]
  hotarticle_author <- tempsort[order(tempsort[,6],decreasing=T),]
  
  mean_info <- c()
  for(i in 1:length(list.files(path=".\\hot_author",pattern="*.csv")))
  {
    temporg <- paste(".\\hot_author\\temp",i,".csv",sep='')
    data <- read.csv(file=temporg,encoding = 'utf-8')
    mean_info[i] <- mean(data$viewcount)
  }
  mean_info <- as.data.frame(mean_info)
  result1 <- cbind(hotarticle_author,mean_info)[,c(3,7)]
  names(result1) <- c("author","average_viewcount")
  
  files_name <- list.files(path=".\\hot_author",pattern="*.csv")   ### delete file
  
  for(i in 1:length(files_name)){
    x <- paste(".\\hot_author\\temp",i,".csv",sep="")
    unlink(x) 
  }
  return(result1)
}

gethotarticle_author <-function(x){
  
  hot_topic <- sort(all_data$viewcount,decreasing = T)[1:x]
  hot_news <- all_data[which(all_data$viewcount %in% hot_topic),]
  hot_news <- hot_news[order(hot_news$viewcount,decreasing = T),]   # top 5 hot news 
  hotarticle_author <- plyr::count(hot_news, names(all_data)[2])
  
  for(i in 1:nrow(hotarticle_author)){
    hot_article <- all_data[which(all_data$author %in% hotarticle_author[i,1]),]
    temp <- hot_article[order(hot_article[,6],decreasing = T)[1],]
    filename <- paste(".\\hot_author\\temp",i,".csv",sep="")
    write.csv(temp,file=filename)
  }
  
  num_file <- length(list.files(path=".\\hot_author",pattern="*.csv"))
  
  for(i in 1:num_file){
    temporg <- paste(".\\hot_author\\temp",i,".csv",sep='')
    if(i==1){
      data <- read.csv(file=temporg,encoding = 'utf-8')
      alldata <- data
    }
    if(i>=2){
      data <- read.csv(file=temporg,encoding = 'utf-8')
      alldata <- rbind(alldata,data)
    }
    write.csv(alldata,file=".\\content\\combine.csv")
  }
  
  tempsort <- read.csv(file=".\\content\\combine.csv")
  tempsort <- tempsort[,-1]
  hotarticle_author <- tempsort[order(tempsort[,6],decreasing=T),]
  
  files_name <- list.files(path=".\\hot_author",pattern="*.csv")   ### delete file
  
  for(i in 1:length(files_name)){
    x <- paste(".\\hot_author\\temp",i,".csv",sep="")
    unlink(x) 
    return(hotarticle_author)
    
  }
  
}

getmean_high_freqency  <-function(highfreq_author){
  
  for(i in 1:nrow(highfreq_author)){
    x <- all_data[which(all_data$author %in% news_authoor[i,1]),]
    filename <- paste(".\\high_freqency\\freq_article",i,".csv",sep="") 
    write.csv(x,file=filename)
  }
  mean_info2 <- c()
  for(i in 1:length(list.files(path=".\\high_freqency",pattern="*.csv")))
  {
    temporg <- paste(".\\high_freqency\\freq_article",i,".csv",sep='')
    data <- read.csv(file=temporg,encoding = 'utf-8')
    mean_info2[i] <- mean(data$viewcount)
  }
  mean_info2 <- round(mean_info2,digits = 0)
  mean_info2 <- as.data.frame(mean_info2)
  result1 <- cbind(highfreq_author,mean_info2)[,c(1,3)]
  names(result1) <- c("author","average_viewcount")
  
  files_name <- list.files(path=".\\high_freqency",pattern="*.csv")   ### delete file
  
  for(i in 1:length(files_name)){
    x <- paste(".\\high_freqency\\freq_article",i,".csv",sep="")
    unlink(x) 
  }
  return(result1)
}

get_hotauthorstyle <- function(x){
  text = Corpus(DirSource(x,encoding = "UTF-8"), list(language = NA))
  text <- tm_map(text, function(word){ gsub("[A-Za-z0-9]", "", word) })  #delete all article english & number
  text <- tm_map(text, removePunctuation) 
  
  getnoun_list <- function(x){                   
    for(i in 1:length(x)){
      index=substr(x[[i]],regexpr(pattern="[[:punct:]]",x[[i]]),nchar(x[[i]]))
      noun_position <- which(index %in% c("(n)","(nr)","(nr1)","(nr2)","(nrj)","(nrf)","(ns)"
                                          ,"(nsf)","(nt)","(nz)","(nl)","(ng)")==TRUE)
      nountemp <- x[[i]][noun_position]
      noun <- substr(nountemp,1,regexpr(pattern="[[:punct:]]",nountemp)-1)
      x[[i]] <- noun
    }
    return(x)
  }

  cutter = worker("tag",stop_word=".\\www\\stopword.txt",bylines = T)
  mat <- matrix( unlist(text), nrow=length(text) )                          # All article transform to matrix
  
  article_words = lapply(mat, function(x){
    tmp = cutter <= x
    tmp = paste(tmp[[1]], '(', names(tmp[[1]]),')', sep = '')
    return(tmp)
  } )
  
  article_words <- getnoun_list(article_words)
  article_words <- Corpus(VectorSource(article_words)) 
  tdm <- TermDocumentMatrix( article_words , control=list(wordLengths=c(2,Inf)))
  
  termFrequency <- rowSums(as.matrix(tdm))
  sortResult <- sort(termFrequency,decreasing = T)[1:5]
  sortResult_name <- as.data.frame(names(sortResult))
  
  sortResult_count <- c()
  for (i in 1:nrow(sortResult_name)){
    sortResult_count[i] <- sortResult[[i]]
  }
  sortResult_count <- as.data.frame(sortResult_count)
  sortResult <- cbind(sortResult_name,sortResult_count )
  names(sortResult) <- c("words","times")
  return(sortResult)
}
get_freqentauthor  <- function(x){
  text = Corpus(DirSource(x,encoding = "UTF-8"), list(language = NA))
  text <- tm_map(text, function(word){ gsub("[A-Za-z0-9]", "", word) })  #delete all article english & number
  text <- tm_map(text, removePunctuation) 
  removedanny <- function(x) gsub(pattern="小丹尼",replacement="",x)
  text <- tm_map(text,removedanny)
  removeword1 <- function(x) gsub(pattern="大家",replacement="",x)
  text <- tm_map(text,removeword1)
  
  getnoun_list <- function(x){                   
    for(i in 1:length(x)){
      index=substr(x[[i]],regexpr(pattern="[[:punct:]]",x[[i]]),nchar(x[[i]]))
      noun_position <- which(index %in% c("(n)","(nr)","(nr1)","(nr2)","(nrj)","(nrf)","(ns)"
                                          ,"(nsf)","(nt)","(nz)","(nl)","(ng)")==TRUE)
      nountemp <- x[[i]][noun_position]
      noun <- substr(nountemp,1,regexpr(pattern="[[:punct:]]",nountemp)-1)
      x[[i]] <- noun
    }
    return(x)
  }
  
  cutter = worker("tag",stop_word=".\\www\\stopword.txt",bylines = T)
  mat <- matrix( unlist(text), nrow=length(text) )                          # All article transform to matrix
  
  article_words = lapply(mat, function(x){
    tmp = cutter <= x
    tmp = paste(tmp[[1]], '(', names(tmp[[1]]),')', sep = '')
    return(tmp)
  } )
  
  article_words <- getnoun_list(article_words)
  article_words <- Corpus(VectorSource(article_words)) 
  tdm <- TermDocumentMatrix( article_words , control=list(wordLengths=c(2,Inf)))
  
  termFrequency <- rowSums(as.matrix(tdm))
  sortResult <- sort(termFrequency,decreasing = T)[1:10]
  sortResult_name <- as.data.frame(names(sortResult))
  
  sortResult_count <- c()
  for (i in 1:nrow(sortResult_name)){
    sortResult_count[i] <- sortResult[[i]]
  }
  sortResult_count <- as.data.frame(sortResult_count)
  sortResult <- cbind(sortResult_name,sortResult_count )
  names(sortResult) <- c("words","times")
  return(sortResult)
}

all_data <- read.csv(file="alldata.csv",header=T)         # read data on global environment
all_data <- all_data[,c(2:5,8,9)]
news_authoor <- plyr::count(all_data, names(all_data)[2])
news_authoor <- news_authoor[order(news_authoor[,2],decreasing=T),]
