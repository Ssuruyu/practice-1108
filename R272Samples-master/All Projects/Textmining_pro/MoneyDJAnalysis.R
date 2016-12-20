all_data <- read.csv(file="alldata.csv",header=T)
all_data <- all_data[,c(2:5,8,9)]

hot_topic <- sort(all_data$viewcount,decreasing = T)[1:30]
hot_news <- all_data[which(all_data$viewcount %in% hot_topic),]
hot_news <- hot_news[order(hot_news$viewcount,decreasing = T),]   # sort author by viewcount   
hotarticle_author <- plyr::count(hot_news, names(all_data)[2])    # get author names and author's freqency 

for(i in 1:nrow(hotarticle_author)){                              # get hot author's articles from all_data
  hot_article <- all_data[which(all_data$author %in% hotarticle_author[i,1]),]
  temp <- hot_article[order(hot_article[,6],decreasing = T)[1],]
  filename <- paste(".\\hot_author\\temp",
                    i,".csv",sep="")
  write.csv(temp,file=filename)
}
########## combine data ##########################################
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
##### get dataframe
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
### find top author and their article's average viewcount
ggplot(data=result1 ,aes(x=author,y=average_viewcount,fill=author))+
  geom_bar(stat="identity", width = 1,colour="black")+
  theme(axis.text.x =element_text(size=9),axis.title=element_text(size=9),
        axis.text.y =element_text(size=9,face="bold",colour="black"))+
  scale_y_continuous(limits=c(0,500000))+
  ggtitle("Hot article author & average viewcount")+theme(plot.title = element_text(lineheight=.1, face="bold"))+
  geom_label(aes(label=average_viewcount,size=3,fontface ="bold"),fill="white",
             colour = "black",hjust=0.5,vjust=0,size=5)+
  coord_flip()

ggplot(data=hotarticle_author ,aes(x=author,y=viewcount,fill=author))+
  geom_bar(stat="identity", width = 1,colour="black")+
  theme(axis.text.x =element_text(size=9),axis.title=element_text(size=9),
        axis.text.y =element_text(size=9,face="bold",colour="black"))+
  ggtitle("Hot article & author")+theme(plot.title = element_text(lineheight=.1, face="bold"))+
  scale_y_continuous(limits=c(0,800000))+
  geom_label(aes(label=title,size=3,fontface ="bold"),colour ="white",hjust=0,vjust=1,size=4)+
  geom_text(aes(label=viewcount,fontface ="italic"),colour ="black",hjust=1.1,vjust=-1,size=5)+
  coord_flip()

# ggplot(data=hot_news ,aes(c,y=viewcount,fill=author))+
#   geom_bar(stat="identity", width = 1,colour="black")+
#   theme(axis.text.x =element_text(size=9),axis.title=element_text(size=9),
#         axis.text.y =element_text(size=9,face="bold",colour="black"))+
#   ggtitle("Hot article & author")+theme(plot.title = element_text(lineheight=.1, face="bold"))+
#   scale_y_continuous(limits=c(0,800000))+
#   #geom_label(aes(label=title,size=3,fontface ="bold"),colour ="white",hjust=0,vjust=1,size=4)+
#   geom_text(aes(label=viewcount,fontface ="italic"),colour ="black",hjust=1.1,vjust=-1,size=5)+
#   coord_flip()

################################################

news_authoor <- plyr::count(all_data, names(all_data)[2])
news_authoor <- news_authoor[order(news_authoor[,2],decreasing=T),]
highfreq_author <- news_authoor[1:17,]

# fn <-list.files(path=".\\high_freqency",pattern="*.csv")
# unlink(fn[1:length(fn)])
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

ggplot(data=highfreq_author ,aes(x=author,y=freq,fill=author))+
  geom_bar(stat="identity", width =0.4,colour="black")+
  theme(axis.text.x =element_text(size=9),axis.title=element_text(size=9),
        axis.text.y =element_text(size=8,face="bold",colour="black"))+
  geom_label(aes(label=freq,size=3,fontface ="bold"),fill="white",
             colour ="black",hjust=1.2,vjust=0.5,size=4)+
  ggtitle("high freqency author")+theme(plot.title = element_text(lineheight=.1, face="bold"))+
  coord_flip()

####### plot high freqency author ###############
ggplot(data=result1,aes(x=author,y=average_viewcount,fill=author))+
  geom_bar(stat="identity", width =0.4,colour="black")+
  theme(axis.text.x =element_text(size=9),axis.title=element_text(size=9),
        axis.text.y =element_text(size=8,face="bold",colour="black"))+
  ggtitle("high freqency author $ averageviewcount")+
  theme(plot.title = element_text(lineheight=.1, face="bold"))+
  geom_label(aes(label=average_viewcount,size=3,fontface ="bold"),fill="white",
             colour ="black",hjust=1.2,vjust=0.5,size=4)+
  coord_flip()
data <- read.csv(file=".\\content\\alldata.csv",header=T)
