setwd("~/R/coursera_data_science/capstone")
library(tm)
library(stringr)
library(ngram)
library(ggplot2)
library(RWeka)


#week 2
u <- readRDS(" ngram1.rds")
b <- readRDS(" ngram2.rds")
t <- readRDS(" ngram3.rds")


#wc_file <- input$wc
#if(!is.null(wc_file)){
 ## wc_text <- readLines(wc_file$datapath)
#}
#else
#{
  #wc_text <-  c(sampletwitter,sampleblogs,samplenews)
 # wc_text <- "A word cloud is an image of words that together resemles a cloud shape.The size of a
 # word shows how important it is. eg. How often it appear in the text - is frequency."
#}