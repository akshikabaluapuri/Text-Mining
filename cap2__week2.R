setwd("~/R/coursera_data_science/capstone")
library(dplyr)
library(tidytext)
library(tm)
library(quanteda)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(stringi)
library(RWeka)
library(quanteda)
library(ggplot2)


if(!file.exists("./project")){
  dir.create("./project")}
Url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if(!file.exists("./project/Coursera-SwiftKey.zip")){
  download.file(Url, destfile = "./project/Coursera-SwiftKey.zip",mode= "wb")}
if(!file.exists("./project/final")){
  unzip(zipfile = "./project/Coursera-SwiftKey.zip",exdir = "./project")}
path <- file.path("./project/final", "en_US")
files <- list.files(path, recursive = TRUE)

#week 2
con1 <- file("./project/final/en_US/en_US.twitter.txt","r")
linestwitter <- readLines(con1, encoding = "UTF-8")

con2 <- file("./project/final/en_US/en_US.blogs.txt","r")
linesblogs <- readLines(con2, encoding = "UTF-8")

con3 <- file("./project/final/en_US/en_US.news.txt","r")
linesnews <- readLines(con3, encoding = "UTF-8")

Twitter <- stri_stats_general(linestwitter)
Blogs<- stri_stats_general(linesblogs)
News <- stri_stats_general(linesnews)
data_structure <- data.frame(Twitter,Blogs,News)
data_structure 

sam_twitter <- linestwitter[sample(length(linestwitter)*0.01)] 
sam_blogs <- linesblogs[sample(length(linesblogs)*0.01)] 
sam_news <- linesnews[sample(length(linesnews)*0.01)] 


remove_funny <- function(x){ 
  sampleX <- x
  for (i in 1:length(sampleX)) {
    original <- sampleX[i]
    clean <- iconv(original, "UTF-8","ASCII", sub =" ")
    sampleX[i]<- clean}
  x <- sampleX}

sam_twitter <- remove_funny(sam_twitter)
sam_blogs <- remove_funny(sam_blogs) 
sam_news <- remove_funny(sam_news)

mastervector <- c(sam_twitter,sam_blogs,sam_news)
corp <- VCorpus(VectorSource(mastervector))

addspace_corp <- content_transformer(function(x, pattern) {
  return(gsub(pattern, " ", x))
})

corp <- tm_map(corp, addspace_corp, "-")
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, c("syllogism", "tautology")) 
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, removeWords,c("the","The", "will","also","that","and","for","in","is","it","not","to"))
corp <- tm_map(corp,content_transformer(tolower))
corp <- tm_map(corp, stripWhitespace)


cleantext<-data.frame(text=unlist(sapply(corp, `[`, "content")), stringsAsFactors=F)

uni_tokenizer <- NGramTokenizer(cleantext, Weka_control(min=1 ,max = 1))
bi_tokenizer <- NGramTokenizer(cleantext, Weka_control(min=2 ,max = 2))
tri_tokenizer <- NGramTokenizer(cleantext, Weka_control(min=3 ,max = 3))
quart_tokenizer <- NGramTokenizer(cleantext, Weka_control(min=4 ,max = 4))


uni <- data.frame(table(uni_tokenizer))
bi <- data.frame(table(bi_tokenizer))
tri <- data.frame(table(tri_tokenizer))
quart<- data.frame(table(quart_tokenizer))

unisorted <- uni[order(uni$Freq,decreasing = TRUE),]
bisorted <- bi[order(bi$Freq,decreasing = TRUE),]
trisorted <- tri[order(tri$Freq,decreasing = TRUE),]
quartsorted <- quart[order(quart$Freq,decreasing = TRUE),]

one20 <- unisorted[1:20,]
colnames(one20) <- c("Word","Frequency")
two20 <- bisorted[1:20,]
colnames(two20) <- c("Word","Frequency")
three20 <- trisorted[1:20,]
colnames(three20) <- c("Word","Frequency")
four20 <- quartsorted[1:20,]
colnames(four20) <- c("Word","Frequency")



ggplot(one20, aes(x=Word,y=Frequency) ) + geom_bar(stat="Identity", fill="blue") +geom_text(aes(label=Frequency),
                vjust=-0.2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(two20, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", 
fill="green") +geom_text(aes(label=Frequency), vjust=-0.2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(three20, aes(x=Word,y=Frequency) ) + geom_bar(stat="Identity", 
  fill="red") +geom_text(aes(label=Frequency), vjust=-0.2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(four20, aes(x=Word,y=Frequency) ) + geom_bar(stat="Identity", 
   fill="red") +geom_text(aes(label=Frequency), vjust=-0.2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


saveRDS(one20, " ngram1.rds")
saveRDS(two20, " ngram2.rds")
saveRDS(three20, " ngram3.rds")
saveRDS(four20, " ngram4.rds")



