install.packages("knitr")
library(knitr)
library(dplyr)
summary(gun_deaths)
head(gun_deaths)
names(gun_deaths)
#  Q1 (a)Generate a data frame that summarizes the number of gun deaths per month. Print the data
# frame as a formatted kable() table.

aggregate(gun_deaths$month ,by = list(Month =gun_deaths$month), FUN=sum)
kable(aggregate(gun_deaths$month ,by = list(gun_deaths$month), FUN=sum))

# Q1 (b)Generate a bar chart with labels on the x-axis. That is, each month should be labeled “Jan”,
#“Feb”, “Mar” and etc
sum(is.na(gun_deaths$month))
?month.abb
gun_deaths$month=month.abb[gun_deaths$month]
barplot(table(gun_deaths$month), main="Gun Deaths in America" ,xlab="Months" , ylab= "Frequency", col= c("red"))

# Q1 (c)Generate a bar chart that identifies the number of gun deaths associated with each type of intent
#cause of death. The bars should be sorted from highest to lowest values.
barplot(sort(table(gun_deaths$intent),decreasing = TRUE),main="Gun Deaths Vs Intent" , xlab="Intent" , ylab="Fequency", col =c("#104E8B") )

#Q1 (d) Generate a boxplot visualizing the age of gun death victims, by sex. Print the average age of
# female gun death victims.
sum(complete.cases(gun_deaths$age))
sum(is.na(gun_deaths$age))
gun_deaths$age[is.na(gun_deaths$age)] = mean(gun_deaths$age,na.rm = TRUE)
output <- aggregate(gun_deaths$age,list(Sex=gun_deaths$sex), FUN=mean , cols = c("Sex","Average Age"))
rename(output,AverageAge =x)
input = data.frame(table(gun_deaths$sex,gun_deaths$age))
boxplot(input$Var2~input$Var1, main=" Gun Death Victims vs Sex ", xlab = "Sex" , ylab = "Age", col = c("purple"))

#Q1 (e)How many white males with at least a high school education were killed by guns in 2012?
sum(is.na(gun_deaths$education))
sum(complete.cases(gun_deaths$education))
summary(gun_deaths$year)
gun_deaths_data=as.data.frame(gun_deaths)
gun_deaths_in_2012=subset.data.frame(gun_deaths_data,gun_deaths_data$year== 2012 & gun_deaths_data$race == "White" & "Less than HS"  %in%  gun_deaths_in_2012$education )
#21793
#Q1 (f)(f) Which season of the year has the most gun deaths? Summer 
as.factor(gun_deaths$month)
gun_deaths$month[gun_deaths$month %in% c(1,2,3)] = "Winter"
gun_deaths$month[gun_deaths$month %in% c(4,5,6)] = "Spring"
gun_deaths$month[gun_deaths$month %in% c(7,8,9)] = "Summer"
gun_deaths$month[gun_deaths$month %in% c(10,11,12)] = "Fall"
sort(table(gun_deaths$month),decreasing = TRUE)
#Summer Spring   Fall Winter 
#26280  25801  25062  23655 

#Q1 (g) Are whites who are killed by guns more likely to die because of suicide or homicide? How does
#this compare to blacks and Hispanics?
table(gun_deaths$race=="White" & gun_deaths$intent=="Suicide")
table(gun_deaths$race=="White" & gun_deaths$intent=="Homicide")
boxplot(gun_deaths$race~gun_deaths$intent)
as.factor(gun_deaths$intent) %>%
sapply(gun_deaths$intent, unclass)
gun_deaths$intent<-sapply(gun_deaths$intent, class)
aggregate(gun_deaths$intent, by = list(gun_deaths$race),FUN=sum)

#Q1 (h) Are police-involved gun deaths significantly different from other gun deaths? Assess the relationship between police involvement and other variables.
table(gun_deaths$police)
police_gunned=subset(gun_deaths, gun_deaths$police ==1)
public_gunned=subset(gun_deaths, gun_deaths$police ==0)
summary(subset(gun_deaths, gun_deaths$police ==0))
??subset
table(police_gunned$place)
table(public_gunned$place)
as.factor(police_gunned$place)
summary(police_gunned$place)
library(stats)
stat.desc(police_gunned$age)
library(ggplot2)
pairs(police_gunned)



# mostof the police gunned locations are not available 1383/1402 total gunned deaths by police
# the detahs are almost equally spread across the 3 years and it is same with the months too
#allof then are hiomicide which is obvious 
# most of them are males(1339) than females(63)
#Native American/Native Alaskan are the least number to tbe shot at 25
#Most of the place where deaths happened are not available and no detahs in Industrial/construction,Residential institution ,School/instiution 
??summarise
library(dplyr)
police_gunned %>% group_by(police)

# Home                 NA    Other specified  Other unspecified             Street 
# 3               1383                  2                  8                  5 
# Trade/service area 
# 1 
# > public_gunned=subset(gun_deaths, gun_deaths$police ==0)
# > table(public_gunned$place)
# 
# Farm                    Home Industrial/construction                      NA 
# 470                   60483                     248                       1 
# Other specified       Other unspecified Residential institution       School/instiution 
# 13749                    8859                     203                     671 
# Sports                  Street      Trade/service area 
# 128                   11146                    3438 






# Q2(a) Rename the 1st column to “id”; the 3rd to the 5th columns respectively to “WeightLoss month1”, “WeightLoss month2”, and “WeightLoss month3”; and the 6th to the 8th
# columns to “SelfEsteem month1”, “SelfEsteem month2”, “SelfEsteem month3”, respectively
attach(weightLoss)
library(dplyr)
names (weightLoss)
oldnames = c("wl1","wl2","wl3","se1","se2","se3")
newnames = c("WeightLoss month1", "WeightLoss month2",  "WeightLoss month3" ,"SelfEsteem month1","SelfEsteem month2", "SelfEsteem month3" )
weightLossQ2 <- weightLoss %>% dplyr::rename(id = X) %>% rename_at(oldnames, ~ newnames)
# Q2(b)Use the melt() function from the “Reshape2” package to reshape the dataset based on the
# WeightLoss month variables into a single column. Use “id” and “group” as the “id variables”.
# Let’s call this reshaped data frame “wl.data”.
??melt
library(reshape2)
wl.data <- weightLossQ2 %>% melt(id.vars=c("id", "group") ,measure.vars= c("WeightLoss month1", "WeightLoss month2",  "WeightLoss month3"), variable.name = c("wl.data"))
#c) Rename the 3rd and 4th columns of wl.data to “WeightLoss_Month” and “WeightLos”.
wl.data <- weightLossQ2b %>% dplyr::rename(WeightLoss_Month  = names(weightLossQ2b)[3] ) %>% dplyr::rename(WeightLoss = names(weightLossQ2b)[4])
# (d) This time use the melt() function to reshape the data based on “SelfEsteem month” variables.
# Call this dataset “we.data”. Rename the 3rd and 4th columns of we.data to “SelfEsteem Month”
# and “SelfEsteem Score”.
we.data <- weightLossQ2 %>% melt(id.vars=c("id", "group") ,measure.vars= c("SelfEsteem month1","SelfEsteem month2", "SelfEsteem month3"), variable.name = c("we.data")) %>%
  dplyr::rename(SelfEsteem_Month  = we.data ) %>% dplyr::rename(SelfEsteem_Score = value)

# (e) Combine the two datasets “wl.data” and “we.data” and call the new dataset as “data.long”.
# Make sure each column is repeated once.
data.long=merge(x=wl.data , y=we.data, by = "group")
??merge

# (f) Use the Weight Loss (pounds) as a categorical data and get the weight loss frequencies by
# groups.
table(data.long$WeightLoss)


  nrow(weightLoss)
  ??nrow
  theme_set(theme_classic())
ggplot(data = data.long )+aes(x= WeightLoss) +
  geom_histogram(fill="Red", binwidth = 0.5 ) +
  labs(y="Count") + labs(x="weight in pounds") +ggtitle("Weight Loss by Group within 3 months")+theme_grey()+
  facet_wrap(~group)+geom_bar(aes(color=group), width = 0.5, orientation = "x")

??facet_wrap
??geom_bar
library(dplyr)
# (h) Use the “weightLoss.data” and the ggplot() function to reproduce the exact same scatter plot
# below
wightLossMonth1 =data.long %>% dplyr::filter(WeightLoss_Month == "WeightLoss month1" , SelfEsteem_Month == "SelfEsteem month1") 
 ggplot(data = wightLossMonth1)+ 
  aes(x= WeightLoss , y= SelfEsteem_Score) + geom_point(size=1 , aes(color=group) ) + facet_wrap(~group)+theme_grey()+
  ggtitle("Weight Loss vs Self-Esteem - Month1")
??geom_point
 
 
 #Question 3:
 colnames(testX) <- c("Radius_mean",
                       "Texture_mean",
                       "Perimeter_mean",
                       "Area_mean",
                       "Smoothness_mean",
                       "Compactness_mean",
                       "Concavity_mean",
                       "NOCPOC_mean",
                       "Symmetry_mean",
                       "FD_mean",
                       "Radius_sd",
                       "Texture_sd",
                       "Perimeter_sd",
                       "Area_sd",
                       "Smoothness_sd",
                       "Compactness_sd",
                       "Concavity_sd",
                       "NOCPOC_sd",
                       "Symmetry_sd",
                       "FD_sd",
                       "Radius_Lgst",
                       "Texture_Lgst",
                       "Perimeter_Lgst",
                       "Area_Lgst",
                       "Smoothness_Lgst",
                       "Compactness_Lgst",
                       "Concavity_Lgst",
                       "NOCPOC_Lgst",
                       "Symmetry_Lgst",
                       "FD_Lgst"
 )
 colnames(trainY) <- c("Label")
 library(dplyr)
 install.packages("tree")
 library(tree)
 
 #rm(trainX)
 trainX$id <- trainX %>% mutate(id =n())
 trainX <-  trainX %>% mutate(-id)
 testY <- testY %>% mutate(id = 1:56)
 trainY <- trainY %>% select(-id)
 testX <- testX %>% mutate(id = 1:56)
 trainY<- trainY%>% mutate(id = 1:454)  
 trainX <- trainX%>% mutate(id = 1:454)  
 
 #rm(FinalDataFrametest )
 FinalDataFrameTrain <- merge(trainX, trainY , by = "id") %>% select(-id)# rbind can be used instead of it
 FinalDataFrameTest <- merge(testX, testY , by = "id") %>% select(-id)# rbind can be used instead of it
 FinalDataFrameTrain %>% as.factor(Label)
 FinaldataFrame <- as.factor(FinaldataFrame$Label)# wrong one
 as.factor(FinalDataFrameTrain$Label)

colnames(trainX)
??mutate
library(rpart)
library(rpart.plot)

tree_full <- rpart(Label ~ Radius_mean+
                Texture_mean+
                Perimeter_mean+
                Area_mean+
                Smoothness_mean+
                Compactness_mean+
                Concavity_mean+
                NOCPOC_mean+
                Symmetry_mean+
                FD_mean+
                  Radius_sd+
                  Texture_sd+
                  Perimeter_sd+
                  Area_sd+
                  Smoothness_sd+
                  Compactness_sd+
                  Concavity_sd+
                  NOCPOC_sd+
                  Symmetry_sd+
                  FD_sd+
                  Radius_Lgst+
                  Texture_Lgst+
                  Perimeter_Lgst+
                  Area_Lgst+
                  Smoothness_Lgst+
                  Compactness_Lgst+
                  Concavity_Lgst+
                  NOCPOC_Lgst+
                  Symmetry_Lgst+
                  FD_Lgst
                  
                , data = FinalDataFrameTrain, method = "class")
rpart.plot(tree_full)
summary(tree_full)
summary(predict(tree_full, FinalDataFrameTrain, type = "class"))
table(trainY$Label)
summary(predict(tree_full, FinalDataFrameTest, type = "class"))
table(testY$Label)
??rpart
tree_mean <- rpart(Label ~ Radius_mean+
                     Texture_mean+
                     Perimeter_mean+
                     Area_mean+
                     Smoothness_mean+
                     Compactness_mean+
                     Concavity_mean+
                     NOCPOC_mean+
                     Symmetry_mean+
                     FD_mean
                     
                   , data = FinaldataFrame , method = "class")


tree_Lgst <- rpart(Label ~ Radius_Lgst+
                   Texture_Lgst+
                   Perimeter_Lgst+
                   Area_Lgst+
                   Smoothness_Lgst+
                   Compactness_Lgst+
                   Concavity_Lgst+
                   NOCPOC_Lgst+
                   Symmetry_Lgst+
                   FD_Lgst
                 , data = FinaldataFrame  )
prune_tree <- prune(tree_full,cp=tree_full$cptable[which.min(tree_full$cptable[,"xerror"]),"CP"])
rpart.plot(prune_tree)
summary(prune_tree)
rpart.plot(tree_full)
rpart.plot(prune_tree)
summary(tree_full)
rpart.rules(tree_full)
rpart.rules(prune_tree)
table(predict(tree_full, FinaldataFrame))
table(predict(tree_full, FinaldataFrametest))
table(predict(prune_tree, FinaldataFrame))
table(predict(tree_full, FinaldataFrametest,type="class"))
summary(predict(tree_full, FinaldataFrame,type="class"))
table(trainY)
??rpart
# (a) Clean your data (if required) before running any model. Create a C&R decision tree to its full
# depth. How many leaves are in tree? How do you get this information?
#there are 5 leaf nodes in the tree and the leaf  nodes are the nodes
#that are lowest hanging nodes of a decision tree
#(c) Give two strong rules that describe who is likely to have cancer. Please justify your choices
# 0.86 when Perimeter_Lgst >= 106 & Texture_mean <  17 & NOCPOC_mean >= 0.062                      
# 0.95 when Perimeter_Lgst >= 106 & Texture_mean >= 17       
#B answer
# Variable importance
# Perimeter_Lgst      Area_Lgst    Radius_Lgst Perimeter_mean 
# 17             16             15             14 
# Radius_mean      Area_mean   Texture_mean    NOCPOC_Lgst 
# 14             14              2              2 
# Texture_Lgst Concavity_mean  Symmetry_Lgst    NOCPOC_mean 
# 1              1              1              1 
fivenum((predict(tree_full, trainX)))
printcp(tree_Lgst)#	display cp table
plotcp(tree_Lgst)	#plot cross-validation results
rsq.rpart(tree_Lgst )	#plot approximate R-squared and relative error for different splits (2 plots). labels are only appropriate for the "anova" method.
print(fit)	#print results
summary(tree_Lgst)	#detailed results including surrogate splits
plot(tree_Lgst)	#plot decision tree
text(tree_Lgst)#	label the decision tree plot

# 0.857 when Perimeter_Lgst >= 106 & NOCPOC_Lgst >= 0.15 & Texture_mean <  17                       
# 0.993 when Perimeter_Lgst >= 106                       & Texture_mean >= 17 & NOCPOC_mean >= 0.049    
