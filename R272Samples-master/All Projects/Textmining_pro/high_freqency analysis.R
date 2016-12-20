all_data <- read.csv(file="alldata.csv",header=T)
all_data <- all_data[,c(2,3,7,9)]
news_authoor <- plyr::count(all_data, names(all_data)[2])           #count the author's article and author
news_authoor <- news_authoor[order(news_authoor[,2],decreasing=T),]
topauthor <- news_authoor[1:10,]                             #Top 10 writting the most article authors
author_article <-all_data[which(all_data[,2] %in% topauthor[,1]),]          # Top 10 author's article
author_article <- author_article[order(author_article[,4],decreasing=T),]   # sort the viewcount 

# topone_author <- author_article[which(author_article[,2] %in% topauthor[1,1]),][,3]
# topone_author<- as.data.frame(topone_author)

## If you want to run again ,you need to delete folders and folder's files

for(i in 1:nrow(topauthor)){
  topone_author <- author_article[which(author_article[,2] %in% topauthor[i,1]),][,3]
  topone_author <- as.data.frame(topone_author)
  foldername <- paste(".//www//temp2//","author",i,sep="")
  dir.create(foldername)
  for(j in 1:nrow(topone_author)){
    articletemp <- topone_author[j,]
    articletemp <- as.data.frame(articletemp)
    filename <- paste(foldername,"//author_article",i,"_",j,".txt",sep="")
    write.table(articletemp,file=filename,sep="",fileEncoding = "utf-8")
  }
}
orgPath=".\\www\\temp2\\author1"
text = Corpus(DirSource(orgPath,encoding = "UTF-8"), list(language = NA))
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

ggplot(data=sortResult ,aes(x=words,y=times,fill=words))+
  geom_bar(stat="identity", width = 0.7,colour="black")+
  theme(axis.text.x =element_text(size=8),axis.title=element_text(size=1),
        axis.text.y =element_text(size=8,face="bold",colour="black"))+
  ggtitle("Hot article use the most frequently words")+theme(plot.title = element_text(lineheight=.1, face="bold"))+
  geom_text(aes(label=sortResult$times,size=4,fontface ="bold"), nudge_y =-(sortResult$times/2),colour = "black")+ 
  coord_flip()

####### delete folders and folder's files 
name <- list.dirs(path=".\\www\\temp2", full.names = FALSE, recursive = FALSE)  # know folder names
for(i in 1:length(name)){
  deletefile <- paste(".\\www\\temp2\\",name[i],sep="")                  
  unlink(deletefile,recursive = T)             # delete folder and folder's files
}