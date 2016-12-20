all_data <- read.csv(file="alldata.csv",header=T)
all_data <- all_data[,c(2,3,7,9)]
hot_topic <- sort(all_data$viewcount,decreasing = T)[1:40]
hot_news <- all_data[which(all_data$viewcount %in% hot_topic),]
hot_news <- hot_news[order(hot_news$viewcount,decreasing = T),]   
hotarticle_author <- plyr::count(hot_news, names(all_data)[2])

author <- hot_news[which(hot_news$author %in% hotarticle_author[2,1]),]

## If you want to run again ,you need to delete folders and folder's files

for(i in 1:nrow(hotarticle_author)){
  author <- hot_news[which(hot_news$author %in% hotarticle_author[i,1]),][,3]
  author <- as.data.frame(author)
  foldername <- paste(".//www//temp1//","author",i,sep="")
  dir.create(foldername)
  for(j in 1:nrow(author)){
    articletemp <- author[j,]
    articletemp <- as.data.frame(articletemp)
    filename <- paste(foldername,"//author_article",i,"_",j,".txt",sep="")
    write.table(articletemp,file=filename,sep="",fileEncoding = "utf-8")
  }
}


# for( i in 1:nrow(hotarticle_author)){
#   author <- hot_news[which(hot_news$author %in% hotarticle_author[i,1]),][,3]
#   foldername <- paste(".//www//temp1//","author",i,sep="")
#   dir.create(foldername)
#   filename <- paste(foldername,"//author_article",i,".txt",sep="")
#   write.table(author,file=filename,sep=",")
# }

orgPath=".\\month_content\\temp2\\author3"
orgPath=".\\www\\temp1\\author1"
text = Corpus(DirSource(orgPath,encoding = "UTF-8"), list(language = NA))
text <- tm_map(text, function(word){ gsub("[A-Za-z0-9]", "", word) })  #delete all article english & number
text <- tm_map(text, removePunctuation) 
inspect(text[1])

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

ggplot(data=sortResult ,aes(x=words,y=times,fill=words))+
  geom_bar(stat="identity", width = 0.7,colour="black")+
  theme(axis.text.x =element_text(size=8),axis.title=element_text(size=1),
        axis.text.y =element_text(size=8,face="bold",colour="black"))+
  ggtitle("Hot article use the most frequently words")+theme(plot.title = element_text(lineheight=.1, face="bold"))+
  geom_text(aes(label=sortResult$times,size=4,fontface ="bold"), nudge_y =-(sortResult$times/2),colour = "black")+ 
  coord_flip()

####### delete folders and folder's files 
name <- list.dirs(path=".\\www\\temp1", full.names = FALSE, recursive = FALSE)  # know folder names
for(i in 1:length(name)){
  deletefile <- paste(".\\www\\temp1\\",name[i],sep="")                  
  unlink(deletefile,recursive = T)             # delete folder and folder's files
}


# unlink("C:\\Users\\user\\Desktop\\csv",recursive = T) 
# list.dirs(path="C:\\Users\\user\\Desktop\\csv", full.names = TRUE, recursive = TRUE)
# list.dirs(path="C:\\Users\\user\\Desktop\\csv", full.names = FALSE, recursive = FALSE)
