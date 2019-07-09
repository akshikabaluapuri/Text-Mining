# Text-Mining
Capstone project

In cap_prediction.R the data is downloaded and created corpus of the file ,then cleaned by using tm package.
After that I created dataframe  and also applied sapply on data .
Then tokenize data and created ngrams from it.
Because the data is large it was unable to create matrix data ,thats why I created dataframe and then ngrams from it.
Saved data into .rds file for further use

Prediction.R
Here I used Katz's off equation for making prediction for next word.
I used my ngrams which I had created in my cap_prediction.R file.

Global.R and cap2_week2.R
This two files I used for my app to show some basic details of the data eg. 
1 wordcloud .
2  Histogram of ngrams showing frequencies of words
