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
library(stylo)
library(stringr)



ngram4 <- readRDS(" ngrams4.rds")
ngram3 <- readRDS(" ngrams3.rds")
ngram2 <- readRDS(" ngrams2.rds")
ngram1 <- readRDS(" ngrams1.rds")




  
  NextWord <- function(input){
  wordInput <- cleanInput(input)
  wordCount <- length(wordInput)
  prediction <- c()
    
    if(wordCount > 3)
    { 
      wordInput <- wordInput[(wordCount-2): wordCount]
      prediction <- fourgram(wordInput[1],wordInput[2],wordInput[3])
      }
  
  if(wordCount == 3)
  { prediction <- fourgram(wordInput[1],wordInput[2],wordInput[3])}
  
  if(wordCount == 2)
  { prediction <- threegram(wordInput[1],wordInput[2])}
  
  if(wordCount == 1)
  { prediction <- twogram(wordInput[1])}
    
  if(wordCount == 0)
  { prediction <-"Enter Something in TEXT INPUT "}
  
  if(length(prediction) == 0)
  { prediction <-"Oops! Sorry something went wrong in your TEXT INPUT"}
  
  if(length(prediction) < 10)
  { prediction }
  else
  { prediction[1:10] }
    
  }
    
    
    cleanInput <- function(text){ 
      
    textInput <- tolower(text)
    textInput <- removePunctuation(textInput)
    textInput<- stripWhitespace(textInput)
    textInput<- removeNumbers(textInput)
    textInput<- str_replace_all(textInput,"[^[:alnum:]]"," ")  
    textInput <- txt.to.words.ext(textInput, language = "english.all", preserve.case = TRUE)
    return(textInput)
   
    } 
    
fourgram <- function(inputWord1,inputWord2,inputWord3) {
  predictWord <- filter(ngram4,(word1 == inputWord1 & word2 == inputWord2 & word3 == inputWord3))$word4
  if(length(predictWord)== 0){
    
    predictWord <- filter(ngram4,( word2 == inputWord2 & word3 == inputWord3))$word4
    if(length(predictWord)== 0){
      
      predictWord <- filter(ngram4,( word1 == inputWord2 & word2 == inputWord3 ))$word3
      if(length(predictWord)== 0){
       
        predictWord <- threegram(inputWord2,inputWord3)
          
      }
    }
  }
  predictWord
}  


threegram <- function(inputWord1,inputWord2) {
  predictWord <- filter(ngram3,(word1 == inputWord1 & word2 ==inputWord2))$word3
  if(length(predictWord)== 0){
    
    predictWord <- filter(ngram3,( word2 == inputWord2 ))$word3
    if(length(predictWord)== 0){
      
      predictWord <- filter(ngram3,( word1 == inputWord2))$word2
      if(length(predictWord)== 0){
        
        predictWord <- twogram(inputWord2)
      }
    }
  }
  predictWord
}   


twogram <- function(inputWord1) {
  predictWord <- filter (ngram2,( word1 == inputWord1))$word2 
  
  predictWord
}   





