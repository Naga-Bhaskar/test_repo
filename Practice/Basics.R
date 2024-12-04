my_input = list(c(1:5))
typeof(my_input)
my_input
?vector
is.vector(my_input)
?c
?str
?summary
?matrix
str(my_input)
summary(my_input)
my_input
?apply
apply(my_input,2,sum)
a =c(1:10)
b= c("A","B","C")
c =c(T,F,T,F)
my_input = list(first=a,second =b,third=c)
my_input$third[2]
ls()
my_char = factor(c("m","f","f","f"), levels = c("m","f"))
my_char = c("m","f","f","f")
summary(my_char)
typeof(my_char)
class(my_char)
str(my_char)
str(my_input)
?typeof
?class
df1 = data.frame(first=my_char,second=my_char )
df1
str(df1)
class(df1)
df1[which(df1$first=="f"),]
 is_even <- function(number){
   rem = number%%2
   if (rem == 0){
     return(TRUE)
   }else{
     return(FALSE)
   }
 }
 
 is_even(2309809)
getwd() 
library(XLConnect)
?XLConnect
install.packages("XLConnect")
install.packages("rjson")
library(rjson)
?readHTMLTable
install.packages("XML")
library(XML)
install.packages("RMySQL")
library(RMySQL)
install.packages("tidyr")
install.packages("dplyr")
library(tidyr)
library(dplyr)
?gather
messy = data.frame(name=c("Naga-M","Venky-M","Adarsh-M","Chinnu-M"),
                   physics=c(76,85,90,99),
                   maths=c("51","57","60","51"),
                  English=c(95,92,93,94))
messy
plot(messy)
messy = gather(messy,subject,score,physics,maths,English)
plot(messy$name,messy$score)
?separate
messy = separate(messy,name,c("Name","Gender"),sep="-")
messy = spread(messy,subject,score)

library(dplyr)
?filter
?factor
sample = data.frame(c("A","B","B","C","Akjbkn"),c(1,2,3,4,5))
summary(sample)
str(sample)
factor(sample)
#,"readr","twitterR","qdap","syuzhet","ggplot2"
install.packages("twitteR")
library(ggplot2)
speech = read_file("gandhi_speech2.txt")
View(speech)
??syuzhet
g_scores = get_nrc_sentiment(speech)
g_scores
class(g_scores)
g_polarity = g_scores[1,9:10]
g_sentiment = g_scores[1,0:8]
barplot(g_polarity)
g_polarity = data.matrix(g_polarity,rownames.force = FALSE)
class(g_polarity)
barplot(g_sentiment)
g_sentiment = data.matrix(g_sentiment,rownames.force = TRUE)
g_sentence = get_sentences(speech)
g_sentiment = get_sentiment(g_sentence)
summary(g_sentiment)
max(g_sentiment)
sentence_sentiment = data.frame(g_sentence,g_sentiment)
View(sentence_sentiment)
which.max(sentence_sentiment$g_sentiment)
sentence_sentiment[15,]
#"devtools","RColorBrewer","tm","SnowballC","wordcloud"
install.packages("wordcloud")
library(wordcloud)
wordcloud(speech,colors = c("blue","green"),max.words = 6)
?twListToDF()
