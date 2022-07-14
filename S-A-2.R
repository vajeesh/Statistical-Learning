library(writexl)
library(readxl)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(MASS)
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

#import the edited excel file in R as dataframe 
US_data = read_excel("C:/Users/Maha Vajeeshwaran/Desktop/STATISTICAL LEARNING/ASSIGNMENT 1/ANES2016.xlsx") 
#C:\Users\Maha Vajeeshwaran\Desktop\STATISTICAL LEARNING\ASSIGNMENT 1

US_data <- na.omit(US_data)  # removes na values from the data

#a)  delete  -8 and -9 which doesn't have much information

US_data <- US_data[US_data$Media >= 0,]
US_data <- US_data[US_data$FamSize >= 0,]
US_data <- US_data[US_data$Hillary >= 0,]
US_data <- US_data[US_data$Trump >= 0,]
US_data <- US_data[US_data$Age >= 0,]
US_data <- US_data[US_data$Education >= 0,]
US_data <- US_data[US_data$Employment >= 0,]
US_data <- US_data[US_data$Birthplace >= 0,]
US_data <- US_data[US_data$GBirth >= 0,]
US_data <- US_data[US_data$Dependent >= 0,]
US_data <- US_data[US_data$Housing >= 0,]
US_data <- US_data[US_data$Income >= 0,]
US_data <- US_data[US_data$Partner >= -1,]
US_data <- US_data[US_data$SpouseEdu >= -1,]

nrow(US_data)

# Performed cross-validation 
set.seed(7)
train <- sample(nrow(US_data), size = 0.8*nrow(US_data))
US_data_train <- US_data[train,]
US_data_test <- US_data[-train,]
Y.test <- US_data_test$PartyID
nrow(US_data_test)
nrow(US_data_train)
names(US_data)

library(nnet)
multinom.fit <- multinom(as.factor(PartyID)~., data=US_data_train)
pred.fit<-predict(multinom.fit, newdata=US_data_test)
accuracy <- mean(pred.fit == US_data_test$PartyID)
accuracy
summary(multinom.fit)

## best subset selection to select best variable
#install.packages("leaps")
library(leaps)
regfit.full <- regsubsets(PartyID~.,data=US_data_train, nvmax = 27)
reg.summary <- summary(regfit.full)
plot(reg.summary$cp, xlab="No of Variables", ylab ="Cp")
reg.cp.min <- which.min(reg.summary$cp)
points(reg.cp.min, reg.summary$cp[reg.cp.min], pch = 20, col="red")
reg.summary$which[reg.cp.min,]

#Checking the model after subset selection
set.seed(222)
multinom.fit <- multinom(as.factor(PartyID)~Media+Hillary+
                           Trump+Age+Partner+SpouseEdu+Birthplace+
                           GBirth, data=US_data_train, trace = FALSE)
pred.fit<-predict(multinom.fit, newdata=US_data_test)
accuracy <- mean(pred.fit == US_data_test$PartyID)
accuracy
summary(multinom.fit)
###
#LDA Model after feature selection 
set.seed(222)
US_data_train$PartyID <- as.factor(US_data_train$PartyID)
US_data_test$PartyID <- as.factor(US_data_test$PartyID)
lda.fit <-lda(PartyID~Media+Hillary+
                Trump+Age+Partner+SpouseEdu+Birthplace+
                GBirth, data=US_data_train)
print(lda.fit)

lda.pred = predict(lda.fit, newdata=US_data_test, type="response")
lda.class = lda.pred$class
prop.table(xtabs(~PartyID+lda.class,data=US_data_test),1)
table(lda.class, US_data_test$PartyID)
mean(lda.class == US_data_test$PartyID)


## qda
set.seed(222)
US_data_train$PartyID <- as.factor(US_data_train$PartyID)
US_data_test$PartyID <- as.factor(US_data_test$PartyID)
qda.fit = qda(PartyID~Media+Hillary+
                Trump+Age+Partner+SpouseEdu+Birthplace+
                GBirth, data=US_data_train)
qda.fit

qda.pred = predict(qda.fit, newdata=US_data_test, type="response")
qda.class = qda.pred$class
table(qda.class, US_data_test$PartyID)
mean(qda.class == US_data_test$PartyID)


#KNN

set.seed(222)
US_data_train$PartyID <- as.factor(US_data_train$PartyID)
US_data_test$PartyID <- as.factor(US_data_test$PartyID)
train.PartyID <-US_data_train$PartyID

train.data <-cbind(US_data_train$Media,US_data_train$Hillary,US_data_train$Trump, US_data_train$Age,
                        US_data_train$Partner,US_data_train$SpouseEdu,US_data_train$Birthplace, US_data_train$GBirth)

test.data <- cbind(US_data_test$Media,US_data_test$Hillary,US_data_test$Trump, US_data_test$Age,
                        US_data_test$Partner,US_data_test$SpouseEdu,US_data_test$Birthplace, US_data_test$GBirth)
library(class)
pred.knn <- knn(train=train.data,test=test.data,cl=train.PartyID,k=3)
prop.table(xtabs(~US_data_test$PartyID+pred.knn),1)
mean(pred.knn==US_data_test$PartyID)

pred.knn <- knn(train=train.data,test=test.data,cl=train.PartyID,k=5)
prop.table(xtabs(~US_data_test$PartyID+pred.knn),1)
mean(pred.knn==US_data_test$PartyID)

pred.knn <- knn(train=train.data,test=test.data,cl=train.PartyID,k=10)
prop.table(xtabs(~US_data_test$PartyID+pred.knn),1)
mean(pred.knn==US_data_test$PartyID)




