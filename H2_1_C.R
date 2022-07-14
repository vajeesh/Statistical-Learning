library(ISLR)
library(MASS)
library(e1071)
library(vctrs)
library(tree)
library(tidyverse)
library(readxl)
library(tibble)
library(dplyr)
library(VIM)
library(caret)
rm(list = ls())
data <-read_excel("C:/Users/Maha Vajeeshwaran/Desktop/STATISTICAL LEARNING/ASSIGNMENT 2/Data_Cortex_Nuclear.xls")
view(data)

## Replacing the null values
data <- data %>%
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))
sum(is.na(data)) == 0
summary(data)

# Removing the index
data <- data[,c(2:82)]
names(data)
# attach(data)
# data$Genotype <- as.factor(Genotype)
# data$Treatment <- as.factor(Treatment)
# data$Behavior <- as.factor(Behavior)
# data$class <- as.factor(class)
# detach(data)
#------------

# Training and Testing Dataframe 

# Multi-class Dataset
df1 <- data[,c(1:77,81)]
# Binary-class Dataset
df2 <- data[,c(1:78)]
set.seed(1000)
df1$class <- as.factor(df1$class)
df2$Genotype <- as.factor(df2$Genotype)
# Sampling the dataset
train <- sample(1:nrow(data),756) # 70% for train

# Multiclass Train and Test Dataset
df1_train <- df1[train,]
df1_test <- df1[-train,]
View(df1_train)

# Binaryclass Train and Test Dataset
df2_train <- df2[train,]
df2_test <- df2[-train,]

# c) .
# Random Forest Multi Class
library(randomForest)
ran.multi <- randomForest(class~., data=df1_train, mtry=9,importance =TRUE)
Yhat.ran.multi <- predict(ran.multi ,newdata = df1_test)
ran.test.multi <- as.factor(df1_test$class)
table(predict=Yhat.ran.multi, truth=ran.test.multi)
confusionMatrix(Yhat.ran.multi,ran.test.multi)

varImpPlot(ran.multi)
# while seeing the plot we can say that SOD1_N is the most important variable when compared to all others

# Random Forest Binary Class

ran.bi <- randomForest(Genotype~., data=df2_train, mtry=9, 
                       importance =TRUE)
Yhat.ran.bi <- predict(ran.bi ,newdata = df2_test)
table(predict=Yhat.ran.bi, truth=df2_test$Genotype)

confusionMatrix(Yhat.ran.bi,df2_test$Genotype)
#accuracy =0.99

varImpPlot(ran.bi)
# while seeing the plot we can say that APP_N is the most important variable when compared to all others


# Bagging Multi Class

# bagging: multi class
library(caret)       #for general model fitting
library(rpart)       #for fitting decision trees
library(ipred)       #for fitting bagged decision trees
bag <- bagging(class ~ ., data = df1_train, coob=TRUE)
summary(bag)
print(bag)

# Bagging classification trees with 25 bootstrap replications 
# 
# Call: bagging.data.frame(formula = class ~ ., data = df1_train, coob = TRUE)
# 
# Out-of-bag estimate of misclassification error:  0.0833 

Yhat_bag <- predict(bag ,df1_test,type="class")
df1_test$class <- as.factor(df1_test$class)
confusionMatrix(Yhat_bag, df1_test$class)
#Accuracy : 0.9012

table(Yhat_bag,df1_test$class)

S_Comb <- list(list(model=slda, predict=function(object, newdata)
  + predict(object, newdata)$x))

Dbl_bag <- bagging(class ~ ., data = df1_train, comb=S_Comb)
summary(Dbl_bag)

Yhat_bag <- predict(Dbl_bag ,df1_test,type="class")
confusionMatrix(Yhat_bag, df1_test$class)

table(Yhat_bag,df1_test$class)
#Accuracy : 0.9907
# bagging: binary-class

bag <- bagging(Genotype ~ ., data = df2_train, coob=TRUE)
summary(bag)
print(bag)


Yhat_bag <- predict(bag ,df2_test,type="class")
confusionMatrix(Yhat_bag, df2_test$Genotype)

table(Yhat_bag,df2_test$Genotype)
#Accuracy = 0.95

S_Comb <- list(list(model=slda, predict=function(object, newdata)+ predict(object, newdata)$x))

Dbl_bag <- bagging(Genotype ~ ., data = df2_train, comb=S_Comb)
summary(Dbl_bag)

Yhat_bag <- predict(Dbl_bag ,df2_test,type="class")
confusionMatrix(Yhat_bag, df2_test$Genotype)
# Accuracy = 0.963
table(Yhat_bag, df2_test$Genotype)

##############Boosting

#####################
boost.binary <- df2
boost.binary$Genotype <- ifelse(boost.binary$Genotype == "Control",0,1)
View(boost.binary)
bb_train <- boost.binary[train,]
bb_test <- boost.binary[-train,]
#####################Boost Binary
library(gbm)
bbinary <- gbm(Genotype~., data=bb_train, distribution = "bernoulli",
               n.trees =5000, interaction.depth =4)
summary(bbinary)
Yhat.bbinary <- predict(bbinary, newdata=bb_test,n.trees=5000)
Yhat <- (Yhat.bbinary - min(Yhat.bbinary)) / (max(Yhat.bbinary) - min(Yhat.bbinary))
data_pred <- ifelse(Yhat <= 0.5,0,1)
ran.test.binary <- bb_test$Genotype
table(predict=data_pred,truth=ran.test.binary)
#164+155 = 319/324 = 0.9845
#Accuracy = 0.9845

########## Boosting Multiclass
bmulti <- gbm(class~., data=df1_train, distribution = "multinomial",
              n.trees =5000, verbose = F, shrinkage = 0.01, interaction.depth =4)
summary(bmulti)
Yhat.bmulti <- predict(bmulti, newdata=df1_test,n.trees=5000,type="response")
pred=as.matrix(Yhat.bmulti[,,1])
P_pred <- apply(pred, 1, which.max)
ran.test.multi <- df1_test$class
table(P_pred, ran.test.multi)
#53+39+36+36+40+31+42+40 = 317/324 = 0.978
# Accuracy = 0.978
