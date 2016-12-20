
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
#source('textmining_shiny.R')
#source('MoneyDJAnalysis.R')

shinyServer(function(input, output, session) {

  output$wordcloud <- renderPlot({
    getmonth = input$month
    if(getmonth=="April"){
      getmonth =".\\month_content\\2016_04"
    }
    if(getmonth=="May"){
      getmonth =".\\month_content\\2016_05"
    }
    if(getmonth=="June"){
      getmonth =".\\month_content\\2016_06"
    }
    if(getmonth=="July"){
      getmonth =".\\month_content\\2016_07"
    }
    if(getmonth=="August"){
      getmonth =".\\month_content\\2016_08"
    }
    tdm  <- getmatrix2(getmonth)
    m <- as.matrix(tdm)
    # calculate the frequency of words and sort it descendingly by frequency
    wordFreq <- sort(rowSums(m), decreasing=TRUE)
    words <- names(wordFreq)
    termfreq <- input$freq
    # word cloud
    wordcloud(words, freq=wordFreq, min.freq=termfreq, random.order=F, ordered.colors = F,
              rot.per=0.2, scale=c(5,.3),colors=rainbow(length(words)))
  })
  output$highfreqencyterms <- renderPlot({
    
    getmonth = input$month
    
    if(getmonth=="April"){
      getmonth =".\\month_content\\2016_04"
    }
    if(getmonth=="May"){
      getmonth  =".\\month_content\\2016_05"
    }
    if(getmonth=="June"){
      getmonth  =".\\month_content\\2016_06"
    }
    if(getmonth=="July"){
      getmonth  =".\\month_content\\2016_07"
    }
    if(getmonth=="August"){
      getmonth  =".\\month_content\\2016_08"
    }
    
    tdm <- getmatrix2(getmonth)
    
    termFrequency <- rowSums(as.matrix(tdm))
    topnumber <- input$value
    topnumber <- as.numeric(topnumber)
    sortResult <- sort(termFrequency,decreasing = T)[1:topnumber]
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
      geom_text(aes(label=times,size=4,fontface ="bold"), nudge_y =-(sortResult$times/2),colour = "black")+ 
      coord_flip()
  })
  output$TermsCorrelation <- renderPlot({
    getmonth = input$month
    
    if(getmonth=="April"){
      getmonth  =".\\month_content\\2016_04"
    }
    if(getmonth=="May"){
      getmonth  =".\\month_content\\2016_05"
    }
    if(getmonth=="June"){
      getmonth  =".\\month_content\\2016_06"
    }
    if(getmonth=="July"){
      getmonth =".\\month_content\\2016_07"
    }
    if(getmonth=="August"){
      getmonth  =".\\month_content\\2016_08"
    }
    tdm <- getmatrix2(getmonth)
    
    cwords  <- input$words
    correlation <- as.numeric(input$correlation)
    
    re<- findAssocs(tdm,cwords, correlation)  
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
  })
  output$Kmeansout <- renderPlot({
    getmonth = input$month
    if(getmonth=="April"){
      getmonth  =".\\month_content\\2016_04"
    }
    if(getmonth=="May"){
      getmonth  =".\\month_content\\2016_05"
    }
    if(getmonth=="June"){
      getmonth =".\\month_content\\2016_06"
    }
    if(getmonth=="July"){
      getmonth =".\\month_content\\2016_07"
    }
    if(getmonth=="August"){
      getmonth =".\\month_content\\2016_08"
    }

    tdm <- getmatrix2(getmonth)

    myTdm2 <- removeSparseTerms(tdm, sparse=0.8)
  
    d <- dist(t(myTdm2), method="euclidian") 
    kmeansvalue <- input$kmeansvalue
    kmeansvalue <- as.numeric(kmeansvalue)
    kfit <- kmeans(d, kmeansvalue)   
    clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 
    
  })
  output$Hotauthor<- renderPlot({
    all_data <- read.csv(file="alldata.csv",header=T)
    all_data <- all_data[,c(2:5,8,9)]
    topauthor<- input$hot_article
    topauthor <- as.numeric(topauthor)
    result1 <- getresult1(topauthor) 
    
    ggplot(data=result1 ,aes(x=author,y=average_viewcount,fill=author))+
      geom_bar(stat="identity", width = 1,colour="black")+
      theme(axis.text.x =element_text(size=9),axis.title=element_text(size=9),
            axis.text.y =element_text(size=9,face="bold",colour="black"))+
      scale_y_continuous(limits=c(0,500000))+
      ggtitle("Hot article author & average viewcount")+
      theme(plot.title = element_text(lineheight=.1, face="bold"))+
      geom_label(aes(label=average_viewcount,size=3,fontface ="bold"),fill="white",
                 colour = "black",hjust=0.5,vjust=0,size=5)+
      coord_flip()
    
  })
  output$Hotarticle<- renderPlot({
    data <- read.csv(file="alldata.csv",header=T)
    all_data <- data[,c(2:5,8,9)]
    hotauthor<- input$hot_article
    hotauthor <- as.numeric(hotauthor)
    
    hotarticle_author <- gethotarticle_author( hotauthor)
    
    ggplot(data=hotarticle_author ,aes(x=author,y=viewcount,fill=author))+
      geom_bar(stat="identity", width = 1,colour="black")+
      theme(axis.text.x =element_text(size=9),axis.title=element_text(size=9),
            axis.text.y =element_text(size=9,face="bold",colour="black"))+
      ggtitle("Hot article & author")+theme(plot.title = element_text(lineheight=.1, face="bold"))+
      scale_y_continuous(limits=c(0,800000))+
      geom_label(aes(label=title,size=3,fontface ="bold"),colour ="white",hjust=0,vjust=1,size=4)+
      geom_text(aes(label=viewcount,fontface ="italic"),colour ="black",hjust=1.1,vjust=-1,size=5)+
      coord_flip()
    
  })
  output$writtingstyle<- renderPlot({
    getauthor <- input$author
    if(getauthor=="IDIOT101"){
      getauthor=".\\www\\temp1\\author2"
    }
    if(getauthor=="Alice"){
      getauthor=".\\www\\temp1\\author1"
    }
    if(getauthor=="lulucasa"){
      getauthor=".\\www\\temp1\\author3"
    }
    if(getauthor=="朱古力男爵"){
      getauthor=".\\www\\temp1\\author7"
    }
    if(getauthor=="大昌以琳"){
      getauthor=".\\www\\temp1\\author4"
    }
    sortResult <- get_hotauthorstyle(getauthor)
    ggplot(data=sortResult ,aes(x=words,y=times,fill=words))+
      geom_bar(stat="identity", width = 0.7,colour="black")+
      theme(axis.text.x =element_text(size=8),axis.title=element_text(size=1),
            axis.text.y =element_text(size=8,face="bold",colour="black"))+
      ggtitle("Hot article use the most frequently words")+
      theme(plot.title = element_text(lineheight=.1, face="bold"))+
      geom_text(aes(label=sortResult$times,size=4,fontface ="bold"), 
                nudge_y =-(sortResult$times/2),colour = "black")+ 
      coord_flip()
  })
  output$high_freqency_author <- renderPlot({
    
    HF <- input$High_Frequency
    HF <- as.numeric(HF)
    highfreq_author <- news_authoor[1:HF,]
    ggplot(data=highfreq_author ,aes(x=author,y=freq,fill=author))+
      geom_bar(stat="identity", width =0.4,colour="black")+
      theme(axis.text.x =element_text(size=9),axis.title=element_text(size=9),
            axis.text.y =element_text(size=8,face="bold",colour="black"))+
      geom_label(aes(label=freq,size=3,fontface ="bold"),fill="white",
                 colour ="black",hjust=1.2,vjust=0.5,size=4)+
      ggtitle("high freqency author")+theme(plot.title = element_text(lineheight=.1, face="bold"))+
      coord_flip()
  })
  output$mean_high_freqency <- renderPlot({

    HF2  <- input$High_Frequency
    HF2  <- as.numeric(HF2)
    highfreq_author <- news_authoor[1:HF2,]
    
    result1  <- getmean_high_freqency(highfreq_author)
    
    ggplot(data=result1,aes(x=author,y=average_viewcount,fill=author))+
      geom_bar(stat="identity", width =0.4,colour="black")+
      theme(axis.text.x =element_text(size=9),axis.title=element_text(size=9),
            axis.text.y =element_text(size=8,face="bold",colour="black"))+
      ggtitle("high freqency author $ averageviewcount")+
      theme(plot.title = element_text(lineheight=.1, face="bold"))+
      geom_label(aes(label=average_viewcount,size=3,fontface ="bold"),fill="white",
                 colour ="black",hjust=1.2,vjust=0.5,size=4)+
      coord_flip()
  })
  output$frequentstyle <-renderPlot({
    getauthor <- input$frequent_author
    
    if(getauthor=="sdanny"){
      getauthor=".\\www\\temp2\\author1"
    }
    if(getauthor=="大昌秋如"){
      getauthor=".\\www\\temp2\\author2"
    }
    if(getauthor=="大昌期權天后洪紫瑜"){
      getauthor=".\\www\\temp2\\author3"
    }
    if(getauthor=="大昌劉澤慧"){
      getauthor=".\\www\\temp2\\author4"
    }
    if(getauthor=="胡雅惠"){
      getauthor=".\\www\\temp2\\author5"
    }
    if(getauthor=="廖維凌"){
      getauthor=".\\www\\temp2\\author6"
    }
    if(getauthor=="大昌樹林~游燕玲"){
      getauthor=".\\www\\temp2\\author7"
    }
    if(getauthor=="鴨寶&押寶"){
      getauthor=".\\www\\temp2\\author8"
    }
    if(getauthor=="jessica6136"){
      getauthor=".\\www\\temp2\\author9"
    }
    if(getauthor=="大昌以琳"){
      getauthor=".\\www\\temp2\\author10"
    }
    sortResult <- get_freqentauthor(getauthor)
    ggplot(data=sortResult ,aes(x=words,y=times,fill=words))+
      geom_bar(stat="identity", width = 0.7,colour="black")+
      theme(axis.text.x =element_text(size=8),axis.title=element_text(size=1),
            axis.text.y =element_text(size=8,face="bold",colour="black"))+
      ggtitle("Hot article use the most frequently words")+
      theme(plot.title = element_text(lineheight=.1, face="bold"))+
      geom_text(aes(label=sortResult$times,size=4,fontface ="bold"), 
                nudge_y =-(sortResult$times/2),colour = "black")+ 
      coord_flip()
  })
})
