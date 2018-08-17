rm(list = ls(all = TRUE))
setwd("F:/P&D_Model Building/20170827_Multiple_Linear_Regression_Lab01/multipleRegression")
library(dplyr)
library(corrplot)
library(MASS)

cus_data = read.csv("data.csv")
names(cus_data)
dim(cus_data)
head(cus_data)
tail(cus_data)
str(cus_data)

#converting into factors
cat_data = dplyr::select(cus_data, City)
cat_data = lapply(cat_data, FUN = as.factor)
cus_data$CustomerID = NULL
cus_data = dplyr::select(cus_data, -c(City))
str(cus_data)

#combining the data
cus_data = cbind(cus_data, cat_data)
str(cus_data)

#target variable
library(dplyr)
target = dplyr::select(cus_data, TotalRevenueGenerated)
str(target)

#removing the target to normalise the other numeric variables in the data
cus_data$TotalRevenueGenerated = NULL
str(cus_data)
#numeric variables
num_data = dplyr::select(cus_data, 2:9)
names(num_data)

num_data = scale(num_data)
#replace the standardized num_data with the original ones in the data
cus_data = dplyr::select(cus_data, -c(2:9))
str(cus_data)
cus_data = cbind(cus_data, num_data)
str(cus_data)

#combining the target variable
cus_data = cbind(cus_data, target)
#correlation plot
library(corrplot)
names(cus_data)

#form a correlation matrix of all the numeric varibales with the target variable
cormatrix = cor(cus_data[,c("MinAgeOfChild","MaxAgeOfChild","Tenure",
            "FrquncyOfPurchase","NoOfUnitsPurchased",
"FrequencyOFPlay","NoOfGamesPlayed","NoOfGamesBought"
,"TotalRevenueGenerated")], use = "complete.obs")


corrplot(cormatrix, method = "number")


set.seed(100)
# We can control the randomness of the sampling for future 
#reproducibility by using the "set.seed()" function

#splitting into train and test
train_rows = sample(x = 1:nrow(cus_data), 
                    0.75*nrow(cus_data), replace = FALSE)
train_cus = cus_data[train_rows,]
test_cus = cus_data[-train_rows,]
dim(train_cus)
dim(test_cus)
str(train_cus)
str(test_cus)

#checking null values
sum(is.na(cus_data))
#model building
modle1 = lm(TotalRevenueGenerated~., data = train_cus)
plot(modle1)

summary(modle1)

pred_test = predict(modle1, test_cus)


#applying stepAIC
stepmodel = stepAIC(modle1)
pred_test_step = predict(stepmodel, test_cus)

#evalution
install.packages('DMwR')
library(DMwR)
regr.eval(trues = test_cus[,"TotalRevenueGenerated"], preds = pred_test)
regr.eval(trues = test_cus[,"TotalRevenueGenerated"], preds = pred_test_step)