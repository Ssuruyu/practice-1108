library(jiebaRD)
library(jiebaR)       
library(NLP)
library(tm)           
library(slam)         
library(RColorBrewer)
library(wordcloud)    
library(topicmodels)  
library(plyr)
library(dplyr)
library(ggplot2)
library(graphics)
library(plotly)
library(cluster)
library(fpc)
library(SnowballC)


orgPath =".\\month_content\\2016_07"

text = Corpus(DirSource(orgPath), list(language = NA))
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
tdm <- TermDocumentMatrix( article_words , control=list(wordLengths=c(2,Inf)))   # Bulid term document matrix
tdm <- TermDocumentMatrix( article_words , control=list(wordLengths=c(2,4)))
#inspect(tdm)

m <- as.matrix(tdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
words <- names(wordFreq)
# word cloud
set.seed(375) # to make it reproducible

wordcloud(words, freq=wordFreq, min.freq=10, random.order=F, ordered.colors = F,
          rot.per=0.2, scale=c(5,.3),colors=rainbow(length(words)))

#colors=brewer.pal(6, "Dark2")
############## find top words  ############################
termFrequency <- rowSums(as.matrix(tdm))
sortResult <- sort(termFrequency,decreasing = T)[1:20]
sortResult_name <- as.data.frame(names(sortResult))

sortResult_count <- c()
for (i in 1:nrow(sortResult_name)){
  sortResult_count[i] <- sortResult[[i]]
}
sortResult_count <- as.data.frame(sortResult_count)
sortResult <- cbind(sortResult_name,sortResult_count )
names(sortResult) <- c("words","times")

ggplot(data=sortResult ,aes(x=words,y=times,fill=words))+
  geom_bar(stat="identity", width = 0.7,colour="black")+
  theme(axis.text.x =element_text(size=8),axis.title=element_text(size=1),
        axis.text.y =element_text(size=8,face="bold",colour="black"))+
  ggtitle("Terms Freqency")+theme(plot.title = element_text(lineheight=.1, face="bold"))+
  geom_text(aes(label=sortResult$times,size=4,fontface ="bold"), nudge_y =-(sortResult$times/2),colour = "black")+ 
  coord_flip()
########### compute correlation###############
re<- findAssocs(tdm,"股市", 0.35)  #find terms associated with "apple" with correlation no less than 0.25
re<- unlist(re)
wor_re <- names(re)[1:length(re)]
wor_re <- as.data.frame(wor_re)
corvalue<- c()
for( i in 1:length(re)){
  va <- re[[i]]
  corvalue[i] <- va
}
realation_data <- data.frame()
corvalue <- as.data.frame(corvalue)
realation_data <- cbind(wor_re,corvalue)
names(realation_data) <- c("relation","correlation_value")
################ visualization correlation ###############################
ggplot(data=realation_data ,aes(x=1:nrow(realation_data),y=correlation_value,fill=relation))+
  geom_bar(stat="identity", width = 0.8,colour="black")+
  theme(axis.text.x =element_text(size=9),axis.title=element_text(size=9),
        axis.text.y =element_text(size=9,face="bold",colour="black"))+
  ggtitle("Word's relationship&correlation")+theme(plot.title = element_text(lineheight=.1, face="bold"))+
  scale_y_continuous(limits=c(0,1))+
  geom_label(aes(label=relation,fontface ="bold"),
             colour="black",hjust=0,vjust=0.5,size=3)+
  geom_text(aes(label=correlation_value,fontface ="bold",angle=0),
            colour="black",hjust=4,vjust=0,size=4)+
  coord_flip()

myTdm2 <- removeSparseTerms(tdm, sparse=0.8)
m2 <- as.matrix(myTdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward.D2")
plot(fit)
# cut tree into 3 clusters
rect.hclust(fit, k=3)
(groups <- cutree(fit, k=3))

# transpose the matrix to cluster documents (tweets)
m3 <- t(m2)
# set a fixed random seed
set.seed(122)
# k-means clustering of tweets
k <-5
kmeansResult <- kmeans(m3, k)
# More complex
round(kmeansResult$centers, digits=3)

for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep=""))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:3], "\n")
  # print the tweets of every cluster
  # print(rdmTweets[which(kmeansResult$cluster==i)])
}

d <- dist(t(myTdm2), method="euclidian")   
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 


