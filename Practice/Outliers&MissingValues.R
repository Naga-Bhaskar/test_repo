
# Handling outliers

# finding outliers visiaully using graphs -- We can use cutoff threshold to remove outliers
# The most popular method: IQR method. Any value outside of [Q1 - 1.5 IQR, Q3 + 1.5 IQR] is
# an outlier -- boxplot uses this method
# z-score: compute the z-score of a variable. If z-score exceeds 3, it is considered
# an outlier
# MAD method: Dachuan is going to explain this in full details


# finding outliers visiaully using graphs
hist(hmeq$CLAGE)
Newdata = subset(hmeq, CLAGE < 500)
hist(Newdata$CLAGE)

# IQR method - Inter Quantle method
outliers= boxplot(hmeq$CLAGE)$out
outliers
CLAGEnoOut = ifelse(hmeq$CLAGE %in% outliers, NA, hmeq$CLAGE) # ifelse condition
boxplot(CLAGEnoOut)





# Handling missing values

is.na(hmeq$NINQ)
sum(is.na(hmeq$NINQ))
sum(complete.cases(hmeq$NINQ))
sum(!complete.cases(hmeq$NINQ))
which(!complete.cases(hmeq$NINQ))
summary(hmeq)

#Method 1 replacing missing values with mean
# na.rm = T removes na values before the computation


hmeq$NINQ[is.na(hmeq$NINQ)] = mean(hmeq$NINQ, na.rm = TRUE)# na.rm would remove those missing values while calcualting the mean
sum(is.na(hmeq$NINQ))

#Method2 omitting the mising values in the dataset
CLAG_omit = na.omit(hmeq$CLAGE)
sum(is.na(CLAG_omit))

# replacing missing values with mice (Multivariate Imputation by Chained Equations) function

install.packages("mice")
library(mice)

NData = mice(hmeq, m = 5)
# m indicates the number of imputated dataset
NData$imp$CLAGE
complete(NData,2)
