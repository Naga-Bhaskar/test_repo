x=2
y=3
z=4
rm(list=ls()) # removes all the using the command line
head(DRUG) # returns the first 6 rows
DRUG
dim(DRUG) # to find the kdimensions of the dataset
names(DRUG)
str(DRUG)
library(tibble)
?tibble
attributes(DRUG)
DRUG[1:10,1:3]
DRUG$BP[1:6]
DRUG[1:6,3]
DRUG[1:6,"BP"]
summary(DRUG) #this summarizes all the data in the datset which can be used to undertand how the dataset looks like when it is huge
as.factor(DRUG$Sex)
?stats
??stat
library(help = "stats") # to know about any package and its functions interchange the variable in quotes
install.packages("stats")
library(stats)
install.packages("pastecs")
library(pastecs)
mean(DRUG$Age)
median(DRUG$Age)
range(DRUG$Age)
sd(DRUG$Age)
quantile(DRUG$Age)
fivenum(DRUG$Age)
stat.desc(DRUG, basic = FALSE)
?quantile
?fivenum
data = cbind(DRUG$Na,DRUG$K)
data[,1]
stat.desc(data , basic = FALSE)
aggregate(DRUG$Na,by=list(DRUG$Drug,DRUG$Sex) ,FUN=mean)# very useful to do aggeragation on grouping with a function
?aggregate
hist(DRUG$Age)
plot(density(DRUG$Age))
density(DRUG$Age)
table(DRUG$Age)
pie(table(DRUG$Sex))
?pie
table(DRUG$Sex)
barplot.default(table(DRUG$Sex), main = "Barplot", xlab="Gender",ylab="Frequency",col = c("red","pink"))
?barplot
plot(DRUG$Na,DRUG$K, main = "Scatter PLot") # correlation between the variable can show us why the data pints are scattered
cor(DRUG$Na,DRUG$K) # the correlation here is not strong as the values is less.
boxplot(Na~Drug, data = DRUG , main="boxplot",xlab = "Drug used" , ylab="Sodium levels", col=c("pink","pink","pink","pink","pink"))
install.packages("reshape2")

install.packages("hflights")
library(hflights)
view(hflights)
colnames(hflights)
??tapply
attach(hflights)
tapply(X=AirTime , INDEX = UniqueCarrier , FUN = mean , na.rm = T)#applies a function to one vector and group by another vector
apply()# it applies to all the values in either the row or column
colSums(is.na(hflights))
# to clean the data set as below by grouping multiple columns under one threshold
lowfrequencyCarrier <- names(table(UniqueCarrier)[table(UniqueCarrier) < 1000])
hflights[UniqueCarrier %in% lowfrequencyCarrier,]$UniqueCarrier
#hardley wickham data scientist at R - dplyr, ggplot2, reshpe2
#select , mutate, filter, group_by, summarize, arrange
#select , select , where, group by , select , order by as in sql for above
#filter out columns - use slect
#filter/subsets out rows - use filter
#mutate- creates new variables
#arrange - sorting
#summarize - aggregation always use dplyr::
dplyr::summarise(hflights , min_dist = min(Distance), max_dist = max(Distance), numberofrows = n(), distinctcarriers = n_distinct(UniqueCarrier) )
??n_distinct

install.packages("caret")

install.packages("lattice")
library(lattice)
library(ggplot2)
library(caret)
findCorrelation()

library(ISLR)
data("Carseats")
install.packages("e1071")
library(e1071)
set.seed(32)# ??
indx = sample(2, nrow(), replace = T)
# work on the navibayes in r 
#mean(pred_clas != tragetvariiable in the actual data#)
install.packages("randomForest")    
library(randomForest)
# use the out of the bag samples
??randomForest
library(caret)
#??confusionMatrix(prediction which for random forest is rf$prediction , target variable)
 install.packages("ROCR")
 library(ROCR)
#prediction()
 #performance()
 ??prediction()
 ??performance
 
