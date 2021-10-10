library(ISLR)
data("Carseats")
dim(Carseats)
colnames(Carseats) # same the one below
names(Carseats)
str(Carseats)
min(Carseats$Sales)
max(Carseats$Sales)
fivenum(Carseats$Sales)
quantile(Carseats$Sales)
# atatch the dataset to the file so that you need not specify 
#the datframe again and again in the code you write
attach(Carseats)
mean(Sales)
sd(Sales)
head(Sales, n=10)# same as below
Sales[1:10]# or as below too
Carseats[1:10, "Sales"] #or as below
Carseats[1:10,1]
detach(Carseats )# to detach the object that we attached
??detach
attach(Carseats )
Carseats[1:10,1:5]
Carseats[Sales < 4]#undefined columns selected errora dn corrected bbelow
Carseats[Sales < 4,]
Carseats[ShelveLoc == "Good",]
Carseats[1:5,c("Sales","ShelveLoc")]# to bring out more columns we need to use cbind
Carseats[1:5,"Sales"]# else as shown here
??filter
??select
??arrange
??mutate
filter(Carseats,Sales < 4)
filter(Carseats,(Sales < 4 & ShelveLoc == "Good") | Price >100)
filter(Carseats,Sales < 4 | ShelveLoc == "Good" , ShelveLoc == "Bad")
# %>% this is a chaining operator
??tidyverse
install.packages("tidyverse")
library(tidyverse)
# , is used as a and operator in the condition
Carseats %>% filter((Sales < 4 & ShelveLoc == "Good") | Price >100)
x1 <- 1:5
x2 <- 10:14
(x2-x1)^2 %>% sum() %>% sqrt()
sqrt(sum((x2-x1)^2))
Carseats %>% between(1:5) %>% select(Sales, ShelveLoc, contains("Pop"))

Carseats %>% filter(row(1:5))
#sort or order works the smae way
x <- c(3,1,4,465,678,9786,46)
x[order(x , decreasing = TRUE)]
x[sort(x , decreasing = TRUE)]
# arrange is a ordering in dplyr
Carseats %>% arrange(desc(Price))

#mutate
Carseats$ratio <- Price/CompPrice
Carseats %>% mutate(ratio1 = Price/CompPrice)
summary(Price)
Carseats %>% group_by(ShelveLoc) %>% summarise(mean(Sales,na.rm = TRUE))# you can find the relationship between the target variable and other variables
??summarise
??summarize
install.packages("ggplot2")
library(ggplot2)
??ggplot
#ggplot(data,mapping = aes(x, y axis), geometry)
ggplot( data = Carseats , mapping = aes(x=Price , y=Sales))+ geom_point(size=1)+geom_line(color = "Blue")+
  labs(x="Price Points",y="Sale points")+
  ggtitle("Random Plot")
ggplot(Carseats , aes(Sales))+
  geom_histogram(fill="Red", color = "black", binwidth = 0.5) +
  labs(y="Frequency") +ggtitle("Histogram for Sales")+ylim(c(0:30))

#sapply , lapply , apply
sapply(Carseats, is.factor)
Fvar <- Carseats %>% sapply(is.factor)
Carseats[,Fvar]
Nvar <- Carseats %>% sapply(is.numeric)                                             
apply(Carseats[,Nvar],2 , mean)
myName = function(x){
  return ("Naga Bhakar")
}
myName("who are you")

