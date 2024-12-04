library(dplyr)
library(rpart)
library(rpart.plot)
library(stats)
# (a) Clean your data (if required) before running any model. Create a C&R decision tree to its full
# depth. How many leaves are in tree? How do you get this information?
  
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

testY <- testY %>% mutate(id = 1:56)
testX <- testX %>% mutate(id = 1:56)
trainY<- trainY%>% mutate(id = 1:454)  
trainX <- trainX%>% mutate(id = 1:454)  

FinalDataFrameTrain <- merge(trainX, trainY , by = "id") %>% select(-id)
FinalDataFrameTest <- merge(testX, testY , by = "id") %>% select(-id)

as.factor(FinalDataFrameTrain$Label)
as.Factor(FinalDataFrameTest$Label)

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

#(b) What are the major predictors of Diagnosis? Please justify your reasoning.

summarise(tree_full)

#(c) Give two strong rules that describe who is likely to have cancer. Please justify your choices
rpart.rules(tree_full)

# (d) What is the accuracy of your decision tree model on the training data? What is the accuracy
# of this model on the test data?
summary(predict(tree_full, trainX, type = "class"))
table(trainY$Label)
summary(predict(tree_full, FinalDataFrameTest, type = "class"))
??table(testY$Label)

#(e) Is it possible to improve the performance of your model?
# (f) Construct the best possible decision tree to predict the Y labels. Explain how you construct
# such tree
prune_tree <- prune(tree_full,cp=tree_full$cptable[which.min(tree_full$cptable[,"xerror"]),"CP"])
rpart.plot(prune_tree)
summary(prune_tree)
summary(predict(prune_tree, FinalDataFrameTrain, type = "class"))
table(trainY$Label)
summary(predict(prune_tree, FinalDataFrameTest, type = "class"))
table(testY$Label)
# g) Plot your final decision tree model.
rpart.plot(prune_tree)


###########################END OF QUESTION3#############################


# The below are the additional tree i constructed to figure out
#if we can create a more sophisticated decision tree usng segregated data
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

tree_sd <- rpart(Label ~ Radius_sd+
                     Texture_sd+
                     Perimeter_sd+
                     Area_sd+
                     Smoothness_sd+
                     Compactness_sd+
                     Concavity_sd+
                     NOCPOC_sd+
                     Symmetry_sd+
                     FD_sd
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

rpart.plot(tree_full)
rpart.plot(prune_tree)
summary(tree_full)

rpart.rules(prune_tree)
table(predict(tree_full, FinaldataFrame))
table(predict(tree_full, FinaldataFrametest))
table(predict(prune_tree, FinaldataFrame))
table(predict(tree_full, FinaldataFrametest,type="class"))
summary(predict(tree_full, FinaldataFrame,type="class"))
table(trainY)

fivenum((predict(tree_full, trainX)))
printcp(tree_Lgst)
plotcp(tree_Lgst)
rsq.rpart(tree_Lgst )
print(tree_sd)
summary(tree_Lgst)
plot(tree_Lgst)
text(tree_Lgst)

??cptable
tree_full$cptable[,"xerror"]
which.min(tree_full$cptable[,"xerror"])
