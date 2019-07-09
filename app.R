
#setwd("~/R/coursera_data_science/capstone")
library(dplyr)
library(ggplot2)
library(quanteda)
library(RColorBrewer)
library(RWeka)
library(SnowballC)
library(stringi)
library(stylo)
library(stringr)
library(shiny)
library(tm)
library(tau)
library(tidytext)
library(wordcloud)
library(evaluate)
library(openssl)



ui <- fluidPage(
   
 
   titlePanel("Text Mining"),
   navbarPage("Predict Next Word",
   tabPanel("Application",
   
   sidebarLayout(
   sidebarPanel(
   
        textInput("text",label = h1("Text Input", style = "color:brown"),value =  " "),
       
        
        fluidRow(column(10, verbatimTextOutput("value"))),
        
        
        
        fileInput("wc",h3("Select textfile for WordCloud"), multiple = F, accept = "text/plain"),
       
        
        sliderInput("freq",
                    "Minimun Frequency in cloud:",
                    min = 1,
                    max = 50,
                    value = 20),
        
        sliderInput("max",
                    "Number of words in cloud:",
                    min = 1,
                    max = 100,
                    value = 20),
        actionButton("update","Create Word cloud")
       
      
         
     
      ),
      
      # Show a plot of the generated distribution
      mainPanel(tabsetPanel(type = "tab",
        tabPanel("Next predicted word",
                  mainPanel(
                    h1(htmlOutput("predictions1"),align = "center",style = "color:blue"))),
                    
                                       
        tabPanel("wordcloud", plotOutput("wcplot")),
         tabPanel("word frequency",plotOutput("distPlot1"),plotOutput("distPlot2"),plotOutput("distPlot3"))
     
       
        )
      )
   )
)
))


#Server

source( "global.R" )

source("prediction.R")

server <- function(input, output,session) {
  

  output$value <-renderText({input$text})
  output$predictions1 <- renderUI({ 
    inputText <- input$text
    inputText <- cleanInput(inputText)
    prediction <- NextWord(inputText)
    
    
    str1 <- " predicted Word(s):"
    str2 <- " "
    
    for (i in 1:length(prediction)) { 
      if(!is.na(prediction[i])){
        if(prediction[i]== "Enter Something"){
          str2 <- paste(str2,"<span style = color:green>", h4(prediction[i] , align ="left"),"</span>" )
          str1 <- " "
        }
        else if(prediction[i]=="Oops! Sorry something went wrong in your TEXT INPUT")
        {
          str2 <- paste(str2,"<span style = color:red>", h4(prediction[i] , align ="left"),"</span>" )
          str1 <- " "
        }
        
        else{
          prediction[i] <- paste(i,". ",prediction[i])
          str2 <- paste(str2, h4(prediction[i], align = "left"), "</span>")
          
        }
      }
      
    }
    
    str1 <- h4(str1, align = "left")
    HTML(paste(str1, str2))
    
    
  })
  
  
 wc_data <- reactive({
    
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        wc_file <- input$wc
        if(!is.null(wc_file)){
          wc_text <- readLines(wc_file$datapath)
        }
        else
        {
         
          wc_text <- "A word cloud is an image of words that together resemles a cloud shape.The size of a
                      word shows how important it is. eg. How often it appear in the text - is frequency."
        }
        wc_corpus <- Corpus(VectorSource(wc_text))
        wc_corpus <- tm_map(wc_corpus, removePunctuation)
        wc_corpus <- tm_map(wc_corpus, removeNumbers)
        wc_corpus <- tm_map(wc_corpus, removeWords, c("syllogism", "tautology")) 
        wc_corpus <- tm_map(wc_corpus, removeWords, stopwords("english"))
        wc_corpus <- tm_map(wc_corpus,content_transformer(tolower))
        wc_corpus <- tm_map(wc_corpus, stripWhitespace)
        wc_corpus <- tm_map(wc_corpus, stemDocument)
        
      
      })
    })
     
   })
   wordcloud_rep <- repeatable(wordcloud)
   
   output$wcplot <- renderPlot({
     withProgress({
       setProgress(message = "creating wordcloud...")
       wc_corpus <- wc_data()
       
       wordcloud_rep(wc_corpus,scale = c(4,0,5), min.freq = input$freq,max.words = input$max,
                     colors = brewer.pal(8, "Dark2"), random.order = FALSE, rot.per = .30)
     })
   })



        
         
 source( "global.R" )  
output$distPlot1 <- renderPlot({
  
  withProgress({
    setProgress(message = "Creating Graph...")
  
  
  u <- readRDS(" ngram1.rds")
  b <- readRDS(" ngram2.rds")
  t <- readRDS(" ngram3.rds")


 ggplot(u, aes(x= Word,y=Frequency)) + geom_bar(stat="Identity", fill=" light grey") +geom_text(aes(label=Frequency),
                        vjust=-0.2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 
  })  
})

output$distPlot2 <- renderPlot({
  
  withProgress({
    setProgress(message = "Creating Graph...")
  ggplot(b, aes(x=Word,y=Frequency)) + geom_bar(stat="Identity", fill="dark grey")+
  geom_text(aes(label=Frequency), vjust=-0.2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
})
})

output$distPlot3 <- renderPlot({
  
  withProgress({
    setProgress(message = "Creating Graph...")
 
  ggplot(t, aes(x=Word,y=Frequency) ) + geom_bar(stat="Identity", fill="black") +
    geom_text(aes(label=Frequency), vjust=-0.2) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
})
})








}
# Run the application 
shinyApp(ui = ui, server = server)





