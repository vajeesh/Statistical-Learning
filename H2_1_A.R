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
names(data)
## Replacing the null values
data <- data %>%
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))
sum(is.na(data)) == 0
summary(data)

# Removing index column
data <- data[,c(2:82)]
names(data)

#  Training & Testing the dataset

# (Multiclass Dataset)
data_1 <- data[,c(1:77,81)]
names(data_1)
# (Binary class Dataset)
data_2 <- data[,c(1:78)]
names(data_2)
set.seed(1000)

data_1$class <- as.factor(data_1$class)
data_2$Genotype <- as.factor(data_2$Genotype)
# Sampling the dataset
train <- sample(1:nrow(data),756)

# Multi-class Train and Test Dataset
data_1_train <- data_1[train,]
data_1_test <- data_1[-train,]

View(data_1_train)

# Binary-class Train and Test Dataset
data_2_train <- data_2[train,]
data_2_test <- data_2[-train,]

# 1. a) 
#---------- Multi class Decision Tree 
tree.data_1_train <- tree(class ~., data_1_train)
summary(tree.data_1_train)

x11()
plot(tree.data_1_train)
text(tree.data_1_train ,pretty =0)
Yhat_B <- predict(tree.data_1_train ,data_1_test,type="class")
table(Yhat_B,data_1_test$class)
confusionMatrix(Yhat_B,data_1_test$class)
#  Accuracy = 0.69

#                   Binary class: Decision Tree 
tree.data_2_train <- tree(data_2_train$Genotype ~., data_2_train)
summary(tree.data_2_train)
x11()
plot(tree.data_2_train)
text(tree.data_2_train ,pretty =0)
Yhat_B2 <- predict(tree.data_2_train ,data_2_test, type="class")
table(Yhat_B2,data_2_test$Genotype)
confusionMatrix(Yhat_B2,data_2_test$Genotype)
# Accuracy= 0.858

##----------------------------------------
#Use cross-validation in order to determine the optimal level of tree complexity.
#Pruning is performed
#            Multi-class: Decision Tree 
set.seed(1000)
cv.data_1_train <-cv.tree(tree.data_1_train, FUN=prune.misclass)
names(cv.data_1_train)
cv.data_1_train
par(mfrow =c(1,2))
plot(cv.data_1_train$size ,cv.data_1_train$dev ,type="b")
plot(cv.data_1_train$k ,cv.data_1_train$dev ,type="b")
prune.data_1_train <- prune.misclass(tree.data_1_train , best =20)

# plot the pruned tree
x11()
plot(prune.data_1_train)
text(prune.data_1_train ,pretty =0)
Prd_T_3 <- predict(prune.data_1_train ,data_1_test,type="class")
table(Prd_T_3,data_1_test$class)
confusionMatrix(Prd_T_3,data_1_test$class)
#Accuracy = 0.69

# Binary class: Decision Tree 
set.seed(1000)
cv.data_2_train <-cv.tree(tree.data_2_train ,FUN=prune.misclass)
names(cv.data_2_train)
cv.data_2_train
par(mfrow =c(1,2))
plot(cv.data_2_train$size ,cv.data_2_train$dev ,type="b")
plot(cv.data_2_train$k ,cv.data_2_train$dev ,type="b")
prune.data_2_train <- prune.misclass(tree.data_2_train , best =15)

### plot the pruned tree
x11()
plot(prune.data_2_train)
text(prune.data_2_train ,pretty =0)
Prd_T_4 <- predict(prune.data_2_train ,data_2_test,type="class")
table(Prd_T_4,data_2_test$Genotype)
confusionMatrix(Prd_T_4,data_2_test$Genotype)
# Accuracy = 0.8611

#------------------

# Multi-class:support vector classifier-Linear 

#  Linear
SVM_fit_linear <- svm(class~., data=data_1_train, kernel ="linear", cost = 0.1,
                     scale =FALSE)
Y_Pred <- predict(SVM_fit_linear ,data_1_test)
table(predict = Y_Pred , truth= data_1_test$class)
confusionMatrix(Y_Pred,data_1_test$class)
# Accuracy = 0.7716

## Linear
SVM_fit_linear <- svm(class~., data=data_1_train, kernel ="linear", cost = 0.01,
                     scale =FALSE)
Y_Pred <- predict(SVM_fit_linear ,data_1_test)
table(predict = Y_Pred , truth= data_1_test$class)
confusionMatrix(Y_Pred,data_1_test$class)
# Accuracy = 0.565

#  Fit a support vector classifier to the data with various values of cost, Report the 
#  cross-validation errors associated with different values of this parameter

set.seed(1000)
Tune_out_1 <- tune(svm, class ~., data=data_1_train, kernel = "linear",
                  ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(Tune_out_1)
bestmod1 <- Tune_out_1$best.model
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  1 
# 
# Number of Support Vectors:  343

# Best model 1 & Applying it
SVM_fit_linear <- svm(class~., data=data_1_train, kernel ="linear", cost = 1,
                     scale =FALSE)
Y_Pred <- predict(SVM_fit_linear ,data_1_test)
table(predict = Y_Pred , truth= data_1_test$class)
confusionMatrix(Y_Pred,data_1_test$class)
# Accuracy = 0.9414

#                  Multiclass: support vector classifier-Radial

SVM_RBF1 <- svm(class~., data=data_1_train, kernel ="radial", gamma = 0.1,
                   cost = 0.1)
Y_pred_r1 <- predict(SVM_RBF1 ,data_1_test)
table(predict = Y_pred_r1 , truth=data_1_test$class)
confusionMatrix(Y_pred_r1,data_1_test$class)
# Accuracy = 0.11

#

set.seed(1000)
Tune_out_1 <- tune(svm, class ~., data=data_1_train, kernel = "radial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                 gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(Tune_out_1)
bestmodr1 <- Tune_out_1$best.model

# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  5 
# 
# Number of Support Vectors:  507

SVM_RBF1 <- svm(class~., data=data_1_train, kernel ="radial", gamma = 0.001,
                   cost = 5)
Y_pred_r1 <- predict(SVM_RBF1 ,data_1_test)
table(predict = Y_pred_r1 , truth=data_1_test$class)
confusionMatrix(Y_pred_r1,data_1_test$class)
# Accuracy = 0.9537

# Multi-class:support vector classifier-Polynomial

SVM_fit_poly1 <- svm(class~., data=data_1_train, kernel="polynomial",degree = 2, gamma = 0.1,
                    cost = 0.01)

Y_Predp1 <- predict(SVM_fit_poly1 ,data_1_test)
table(predict = Y_Predp1 , truth=data_1_test$class)
confusionMatrix(Y_Predp1,data_1_test$class)
# Accuracy 0.95

#Tune
set.seed(1000)
Tune_outp1 <- tune(svm, class ~., data=data_1_train, kernel = "polynomial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                 degree=c(2,3,4)))
summary(Tune_outp1)
bestmodp1 <- Tune_outp1$best.model

# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  polynomial 
# cost:  10 
# degree:  3 
# coef.0:  0 
# 
# Number of Support Vectors:  390
 

SVM_fit_poly2 <- svm(class~., data=data_1_train, kernel="polynomial",degree = 3,
                    cost = 10)

Y_Predp2 <- predict(SVM_fit_poly2 ,data_1_test)
table(predict = Y_Predp2 , truth=data_1_test$class)
confusionMatrix(Y_Predp2,data_1_test$class)
# Accuracy = 0.987

#             Binary Class: support vector classifier- Linear

SVM_fit_linearbl1 <- svm(Genotype~., data = data_2_train, kernel ="linear", cost = 0.01,
                        scale =FALSE)
Y_Predbl1 <- predict(SVM_fit_linearbl1 ,data_2_test)
table(predict = Y_Predbl1 , truth= data_2_test$Genotype)
confusionMatrix(Y_Predbl1,data_2_test$Genotype)
# Accuracy = 0.65

##Tune
set.seed(1000)
Tune_outbl1 <- tune(svm, Genotype ~., data=data_2_train, kernel = "linear",
                    ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(Tune_outbl1)
bestmod1 <- Tune_outbl1$best.model
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  1 
# 
# Number of Support Vectors:  95

SVM_fit_linearbl1 <- svm(Genotype~., data = data_2_train, kernel ="linear", 
                        cost = 1, scale =FALSE)
Y_Predbl1 <- predict(SVM_fit_linearbl1 ,data_2_test)
table(predict = Y_Predbl1 , truth= data_2_test$Genotype)
confusionMatrix(Y_Predbl1,data_2_test$Genotype)
#Accuracy= 0.9475

####Radial
SVM_KFbr1 <- svm(Genotype~., data=data_2_train, kernel ="radial", 
                     gamma = 1, cost = 1)
Y_Pred_RK_1 <- predict(SVM_KFbr1 ,data_2_test)
table(predict = Y_Pred_RK_1 , truth= data_2_test$Genotype)
confusionMatrix(Y_Pred_RK_1,data_2_test$Genotype)
# Accuracy = 0.52

set.seed(1000)
Tune_outrbr1 <- tune(svm, Genotype ~., data=data_2_train, kernel = "radial",
                     ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                   gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(Tune_outrbr1)
bestmodrb1 <- Tune_outrbr1$best.model

# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  5 
# 
# Number of Support Vectors:  251

SVM_KFbr1 <- svm(Genotype~., data=data_2_train, kernel ="radial", 
                     gamma = 0.001, cost = 5)
Y_Pred_RK_1 <- predict(SVM_KFbr1 ,data_2_test)
table(predict = Y_Pred_RK_1 , truth= data_2_test$Genotype)
confusionMatrix(Y_Pred_RK_1,data_2_test$Genotype)
# Accuracy = 0.963

#####polynomial
SVM_polyb2 <- svm(Genotype~., data=data_2_train, kernel="polynomial",
                      degree = 2, gamma = 1, cost = 0.01)
Y_Pred_Pb2 <- predict(SVM_polyb2 ,data_2_test)
table(predict = Y_Pred_Pb2 , truth= data_2_test$Genotype)
confusionMatrix(Y_Pred_Pb2,data_2_test$Genotype)
# Accuracy = 0.9815

#

Tune_outply_1 <- tune(svm, as.factor(Genotype) ~., data=data_2_train, kernel = "polynomial",
                    ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                  degree=c(2,3,4)))
summary(Tune_outply_1)
bestmodpb1 <- Tune_outply_1$best.model

# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  polynomial 
# cost:  100 
# degree:  3 
# coef.0:  0 
# 
# Number of Support Vectors:  243

SVM_polyb2 <- svm(Genotype~., data=data_2_train, kernel="polynomial",
                      degree = 3, gamma = 1,cost = 100)
Y_Pred_Pb2 <- predict(SVM_polyb2 ,data_2_test)
table(predict = Y_Pred_Pb2 , truth= data_2_test$Genotype)
confusionMatrix(Y_Pred_Pb2,data_2_test$Genotype)
# Accuracy = 0.99

