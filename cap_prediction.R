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
library(tidyr)
library(plyr)


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

sam_twitter <- linestwitter[sample(length(linestwitter)*0.1)] 
sam_blogs <- linesblogs[sample(length(linesblogs)*0.1)] 
sam_news <- linesnews[sample(length(linesnews)*0.1)] 


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
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp,content_transformer(tolower))
corp <- tm_map(corp, stripWhitespace)


cleantext<-data.frame(text=unlist(sapply(corp, `[`, "content")), stringsAsFactors=F)

uni_tokenizer <- NGramTokenizer(cleantext$text, Weka_control(min=1 ,max = 1))
bi_tokenizer <- NGramTokenizer(cleantext$text, Weka_control(min=2 ,max = 2))
tri_tokenizer <- NGramTokenizer(cleantext$text, Weka_control(min=3 ,max = 3))
quart_tokenizer <- NGramTokenizer(cleantext$text, Weka_control(min=4 ,max = 4))



uni <- data.frame(table(unlist(uni_tokenizer)))
uni <- rename(uni, c("Var1"= "word1", "Freq"="n"))

  
  
  
bi <- filter(cleantext) %>% unnest_tokens(bi_tokenizer,text , token = "ngrams", to_lower = TRUE, n = 2) %>%
  separate(bi_tokenizer, c("word1", "word2"), sep = " ")%>%
  count(word1, word2, sort = TRUE)

tri <- filter(cleantext) %>% unnest_tokens(tri_tokenizer,text, token = "ngrams", to_lower = TRUE, n = 3) %>%
  separate(tri_tokenizer, c("word1", "word2","word3"), sep = " ")%>%
  count(word1, word2,word3, sort = TRUE)

quart <- filter(cleantext) %>% unnest_tokens(quart_tokenizer, text, token = "ngrams", to_lower = TRUE, n = 4) %>%
  separate(quart_tokenizer, c("word1", "word2","word3", "word4"), sep = " ")%>%
  count(word1, word2,word3, word4, sort = TRUE)





#uni <- data.frame(table(unlist(uni_tokenizer)))
#bi <- data.frame(table(unlist(bi_tokenizer)))
#tri <- data.frame(table(unlist(tri_tokenizer)))
#quart<- data.frame(table(unlist(quart_tokenizer)))

unisorted <- uni[order(uni$n ,decreasing = TRUE),]
bisorted <- bi[order(bi$n,decreasing = TRUE),]
trisorted <- tri[order(tri$n,decreasing = TRUE),]
quartsorted <- quart[order(quart$n,decreasing = TRUE),]


saveRDS(unisorted, " ngrams1.rds")
saveRDS(bisorted, " ngrams2.rds")
saveRDS(trisorted, " ngrams3.rds")
saveRDS(quartsorted, " ngrams4.rds")

