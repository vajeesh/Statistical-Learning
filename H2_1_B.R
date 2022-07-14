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

# Removing the index
data <- data[,c(2:82)]
names(data)

##-------------
#b) Perform principal component analysis on the 77 numerical features. 
#Use an appropriate number of principal components as predictors and 
#perform the same classification task.

###PCA

Pr_out <- prcomp(data[,c(1:77)], scale = TRUE)
biplot(Pr_out , scale = 0)
attach(data)

Pr_Var <- Pr_out$sdev^2
Pr_Var
PVE <- Pr_Var/sum(Pr_Var)
PVE

plot(PVE, xlab="Principal Component", ylab="Proportion of
         Variance Explained", ylim=c(0,1) ,type="b")

plot(cumsum (PVE ), xlab="Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", ylim=c(0,1),
     type="b")
abline(h = 0.9, col = "red")

comp <- data.frame(Pr_out$x[,1:20])
View(comp)
attach(comp)

########### Training and Testing Dataframe ###########

# Multi-class Dataset
dfc <- data[81]
View(dfc)
names(dfc)

# Binary-class Dataset
DF_G <- data[78]
DF_G <- data[78]
names(DF_G)
View(DF_G)
set.seed(1000)
################ Multi-class: Decision Tree #####################
dfmd <- data.frame(comp,dfc)
train <- sample(1:nrow(dfmd),756)
DF_Trainm <- dfmd[train,]
df_testm <- dfmd[-train,]
View(df_testm)
DF_Trainm$class <- as.factor(DF_Trainm$class)
df_testm$class <- as.factor(df_testm$class)
##
tree.mc = tree(class~.,DF_Trainm)
summary(tree.mc)
x11()
plot(tree.mc)
text(tree.mc ,pretty =0)
tree.predm1=predict (tree.mc ,df_testm ,type ="class")
table(tree.predm1,df_testm$class)
confusionMatrix(tree.predm1,df_testm$class)
# Accuracy = 0.679

#####BINARY
dfbn <- data.frame(comp,DF_G)
View(dfbn)
dfbn$Genotype <- as.factor(dfbn$Genotype)
train <- sample(1:nrow(dfbn),756)
df_trainb <- dfbn[train,]
df_testb <- dfbn[-train,]

#df_testb <- as.factor(df_testb$Genotype)
#df_trainb <- as.factor(df_trainb$Genotype)
tree.bi <- tree(Genotype~., df_trainb)
#or
tree.bi =tree(as.factor(Genotype)~., df_trainb)
summary(tree.bi)
x11()
plot(tree.bi)
text(tree.bi ,pretty =0)
tree.predm1= predict(tree.bi ,df_testb ,type ="class")
table(tree.predm1,df_testb$Genotype)
confusionMatrix(tree.predm1,df_testb$Genotype)
# Accuracy = 0.907

###CV

################ Multi-class: Decision Tree #####################
set.seed(1000)
cv.train_df1 <- cv.tree(tree.mc, FUN=prune.misclass)
cv.train_df1
par(mfrow =c(1,2))
plot(cv.train_df1$size ,cv.train_df1$dev ,type="b")
plot(cv.train_df1$k ,cv.train_df1$dev ,type="b")
pruned_tree1 <- prune.misclass(tree.mc, best = 28)
plot(pruned_tree1)
text(pruned_tree1 ,pretty =0)
Pred_test3 <- predict(pruned_tree1 ,df_testm,type="class")
test_df1cv1 <- as.factor(df_testm$class)
table(Pred_test3,test_df1cv1)
confusionMatrix(Pred_test3,test_df1cv1)
# Accuracy = 0.679

################ Binary-class: Decision Tree #####################
cv.train_df2 <- cv.tree(tree.bi, FUN=prune.misclass)
cv.train_df2
plot(cv.train_df2$size ,cv.train_df2$dev ,type="b")
Pruned_tree2 <- prune.misclass(tree.bi, best = 20)
plot(Pruned_tree2)
text(Pruned_tree2 ,pretty =0)

Pred_test4 <- predict(Pruned_tree2 ,df_testb,type="class")
test_df2s2 <- as.factor(df_testb$Genotype)
table(Pred_test4,test_df2s2)
confusionMatrix(Pred_test4,test_df2s2)
# Accuracy = 0.8611

########## Multi-class:support vector classifier-Linear ################
## Linear
SVM_fit_linear1 <- svm(class~., data=DF_Trainm, kernel ="linear", 
                      cost = 0.01, scale =FALSE)
Y_predl1 <- predict(SVM_fit_linear1 ,df_testm)
table(predict = Y_predl1 , truth= df_testm$class)
confusionMatrix(Y_predl1,df_testm$class)
## Accuracy = 0.885
#######

#  Fit a support vector classifier to the data with various values of cost, Report the 
# cross-validation errors associated with different values of this parameter
set.seed(1000)
tune.out1 <- tune(svm, class ~., data=DF_Trainm, kernel = "linear",
                  ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out1)
bestmod1 <- tune.out1$best.model
#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  linear 
#cost:  1

#Number of Support Vectors:  292
#Best model 1 & Applying it
SVM_fit_linear <- svm(class~., data=DF_Trainm, kernel ="linear", cost =1,
                     scale =FALSE)
Y_pred <- predict(SVM_fit_linear ,df_testm)
table(predict = Y_pred , truth= df_testm$class)
confusionMatrix(Y_pred,df_testm$class)
# Accuracy = 0.92

############### Multi-class:support vector classifier-Radial #####################
SVM_fit_RBF1 <- svm(class~., data=DF_Trainm, kernel ="radial", gamma = 0.1,
                   cost = 0.1)
Y_predr1 <- predict(SVM_fit_RBF1 ,df_testm)
table(predict = Y_predr1 , truth=df_testm$class)
confusionMatrix(Y_predr1,df_testm$class)
# Accuracy = 0.8333

###########
set.seed(1000)
tune.outr1 <- tune(svm, class ~., data=DF_Trainm, kernel = "radial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                 gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outr1)
bestmodr1 <- tune.outr1$best.model

#Parameters:
#SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#cost 1
#gamma 0.1


#Number of Support Vectors:  553

SVM_fit_RBF1 <- svm(class~., data=DF_Trainm, kernel ="radial", gamma = 0.1,
                   cost = 1)
Y_predr1 <- predict(SVM_fit_RBF1 ,df_testm)
table(predict = Y_predr1 , truth=df_testm$class)
confusionMatrix(Y_predr1,df_testm$class)
# Accuracy = 0.9877

#####################Multi-class:support vector classifier-Polynomial######################

SVM_fit_poly1 <- svm(class~., data=DF_Trainm, kernel="polynomial",
                    degree = 2, gamma = 0.1, cost = 0.01)

Y_predp1 <- predict(SVM_fit_poly1 ,df_testm)
table(predict = Y_predp1 , truth=df_testm$class)
confusionMatrix(Y_predp1,df_testm$class)
# Accuracy = 0.1852

#Tune
set.seed(1000)
tune.outp1 <- tune(svm, as.factor(class) ~., data=DF_Trainm, kernel = "polynomial",
                   ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                 degree=c(2,3,4)))
summary(tune.outp1)
bestmodp1 <- tune.outp1$best.model
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  polynomial 
# cost:  5 
# degree:  3 
# coef.0:  0 
# 
# Number of Support Vectors:  374


SVM_fit_poly2 <- svm(class~., data=DF_Trainm, kernel="polynomial",
                    degree = 3,cost = 5)

Y_predp2 <- predict(SVM_fit_poly2 ,df_testm)
table(predict = Y_predp2 , truth=df_testm$class)
confusionMatrix(Y_predp2,df_testm$class)
# Accuracy = 0.9877

################# Binary Class: support vector classifier- Linear###################

SVM_fit_linearbl1 <- svm(Genotype~., data = df_trainb, kernel ="linear", 
                        cost = 0.01, scale =FALSE)
Y_predbl1 <- predict(SVM_fit_linearbl1 ,df_testb)
table(predict = Y_predbl1 , truth= df_testb$Genotype)
confusionMatrix(Y_predbl1,df_testb$Genotype)
# Accuracy = 0.929

##Tune
set.seed(1000)
tune.outbl1 <- tune(svm, as.factor(Genotype) ~., data=df_trainb, kernel = "linear",
                    ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outbl1)
bestmod1 <- tune.outbl1$best.model
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  0.01 
# 
# Number of Support Vectors:  355
SVM_fit_linearbl1 <- svm(Genotype~., data = df_trainb, kernel ="linear", 
                        cost = 0.01, scale =FALSE)
Y_predbl1 <- predict(SVM_fit_linearbl1 ,df_testb)
table(predict = Y_predbl1 , truth= df_testb$Genotype)
confusionMatrix(Y_predbl1,df_testb$Genotype)
# Accuracy = 0.929

####Radial
SVM_fit_RBFbr1 <- svm(Genotype~., data=df_trainb, kernel ="radial", 
                     gamma = 1, cost = 1)
Y_predrb1 <- predict(SVM_fit_RBFbr1 ,df_testb)
table(predict = Y_predrb1 , truth= df_testb$Genotype)
confusionMatrix(Y_predrb1,df_testb$Genotype)
# Accuracy = 0.8951

set.seed(1000)
tune.outrbr1 <- tune(svm, as.factor(Genotype) ~., data=df_trainb, kernel = "radial",
                     ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                                   gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.outrbr1)
bestmodrb1 <- tune.outrbr1$best.model

# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# 
# Number of Support Vectors:  361

SVM_fit_RBFbr1 <- svm(Genotype~., data=df_trainb, kernel ="radial", 
                     gamma = 0.001, cost = 1)
Y_predrb1 <- predict(SVM_fit_RBFbr1 ,df_testb)
table(predict = Y_predrb1 , truth= df_testb$Genotype)
confusionMatrix(Y_predrb1,df_testb$Genotype)
# Accuracy = 0.9414

#####polynomial
SVM_fit_polypb2 <- svm(Genotype~., data=df_trainb, kernel="polynomial",
                      degree = 2, gamma = 1, cost = 0.01)
Y_predpb2 <- predict(SVM_fit_polypb2 ,df_testb)
table(predict = Y_predpb2 , truth= df_testb$Genotype)
confusionMatrix(Y_predpb2,df_testb$Genotype)
# Accuracy = 0.93
tune.outpb1 <- tune(svm, as.factor(Genotype) ~., data=df_trainb, kernel = "polynomial",
                    ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100),
                                  degree=c(2,3,4)))
summary(tune.outpb1)
bestmodpb1 <- tune.outpb1$best.model

# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  polynomial 
# cost:  10 
# degree:  3 
# coef.0:  0 
# 
# Number of Support Vectors:  211

SVM_fit_polypb2 <- svm(Genotype~., data=df_trainb, kernel="polynomial",
                      degree = 3, gamma = 1,cost = 100)
Y_predpb2 <- predict(SVM_fit_polypb2 ,df_testb)
table(predict = Y_predpb2 , truth= df_testb$Genotype)
confusionMatrix(Y_predpb2,df_testb$Genotype)
#Accuracy = 0.9877

#Reference
# Stackoverflow, Youtube,& other from Google 

