library(tm)
library(wordcloud)
library(memoise)

# read K-means Data
dat <- read.csv(file="iris9999.csv",head=TRUE,sep=",")

# Start Word Cloud
books <- list("A Mid Summer Night's Dream" = "summer",
              "Customer Complaints" = "merchant",
              "Customer Service" = "romeo")

getTermMatrix <- memoise(function(book) {
  if (!(book %in% books))
    stop("Unknown book")
  
  text <- readLines(sprintf("./%s.txt.gz", book),
                    encoding="UTF-8")
  
  myCorpus = Corpus(VectorSource(text))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)

})