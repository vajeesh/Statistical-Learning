# Exercise 4
# Generate a simulated two-class data set with 100 observations and
# two features in which there is a visible but non-linear separation between
# the two classes. Show that in this setting, a support vector
# machine with a polynomial kernel (with degree greater than 1) or a
# radial kernel will outperform a support vector classifier on the training
# data. Which technique performs best on the test data? Make
# plots and report training and test error rates in order to back up
# your assertions.
##################################################################
#### not lineary separable data
set.seed(1)
x <- matrix(rnorm(200*2) , ncol =2)
plot(x)
x[1:75,] <- x[1:75,]+2
x[76:150,] <-  x[76:150,]-2
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x, y=as.factor(y))
plot(x,col=y)
##### another dataset
library(movMF)
library(ggplot2)
library(mvtnorm)
library(dplyr)
generateMFData <- function(n, theta, cluster, scale=1) {
  data = rmovMF(n, theta)
  data = data.frame(data[,1], data[,2])
  data = scale * data
  names(data) = c("x", "y")
  data = data %>% mutate(class=factor(cluster))
  data
}
# cluster 1
n <- 100
data1 <- generateMFData(n, 1 * c(-1, 1), 1, 5)
# cluster 2
n <- 100
data2 <- generateMFData(n, 1 * c(1, -1), 2, 1)
# all data
x <- rbind(data1[,1:2],data2[,1:2])
y <- c(rep(1,100), rep(2,100))
dat2 <- data.frame(x=x, y=as.factor(y))
plot(x,col=y, ylim=c(-6,6))
##################################################################
## create training and testing dataset for both cases
set.seed(1)
train <- sample(1:200, 100)
dat1training <- dat[train,]
dat1testing <- dat[-train,]
dat2training <- dat2[train,]
dat2testing <- dat2[-train,]
##################################################################
#### support vector classifier
library(e1071)
svmfit_linear <- svm(y~., data=dat1training, kernel ="linear", 
                     cost = 100,
              scale =FALSE)
plot(svmfit_linear , dat1training)
ypred <- predict(svmfit_linear ,dat1testing)
table(predict = ypred , truth= dat1testing$y)
##################################################################
#### support vector machine
svmfit_RBF <- svm(y~., data=dat1training, kernel ="radial", 
                  gamma = 3,
              cost = 3)
plot(svmfit_RBF , dat1training)
svmfit_poly <- svm(y~., data=dat1training, kernel="polynomial",
                   degree = 2, gamma = 80,
                  cost = 50)
plot(svmfit_poly , dat1training)
###################################################################
ypred <- predict(svmfit_RBF ,dat1testing)
table(predict = ypred , truth= dat1testing$y)
###################################################################
ypred <- predict(svmfit_poly ,dat1testing)
table(predict = ypred , truth= dat1testing$y)
###################################################################
###################################################################
#### second dataset
##################################################################
#### support vector classifier
svmfit_linear <- svm(y~., data = dat2training, kernel ="linear", cost = 1,
                     scale =FALSE)
plot(svmfit_linear , dat2training)
ypred <- predict(svmfit_linear ,dat2testing)
table(predict = ypred , truth= dat2testing$y)
##################################################################
#### support vector machine
svmfit_RBF <- svm(y~., data=dat2training, kernel ="radial", 
                  gamma = 0.05,
                  cost = 10)
plot(svmfit_RBF , dat2training)
svmfit_poly <- svm(y~., data=dat2training, kernel="polynomial",
                   degree = 2,
                   cost =100)
plot(svmfit_poly , dat2training)
###################################################################
ypred <- predict(svmfit_RBF ,dat2testing)
table(predict = ypred , truth= dat2testing$y)
###################################################################
ypred <- predict(svmfit_poly ,dat2testing)
table(predict = ypred , truth= dat2testing$y)
# Exercise 7
# In this problem, you will use support vector approaches in order to
# predict whether a given car gets high or low gas mileage based on the
# Auto data set.
library(ISLR)
data <- Auto
# (a) Create a binary variable that takes on a 1 for cars with gas
# mileage above the median, and a 0 for cars with gas mileage
# below the median.
median(data$mpg)
mileage <- ifelse(data$mpg <= 22.75,"Below","Above")
data$mileage <- as.factor(mileage)
# (b) Fit a support vector classifier to the data with various values
# of cost, in order to predict whether a car gets high or low gas
# mileage. Report the cross-validation errors associated with different
# values of this parameter. Comment on your results.
set.seed(1)
tune.out <- tune(svm, mileage ~. -mpg, data=data, kernel = "linear",
                 ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod <- tune.out$best.model
#################################################################
#################################################################
# (c) Now repeat (b), this time using SVMs with radial and polynomial
# basis kernels, with different values of gamma and degree and
# cost. Comment on your results.
#######################################################################
### RBF
#######################################################################
set.seed(1)
data <- data[,2:10]
tune.out <- tune(svm, mileage ~., data=data, kernel = "radial",
                 ranges = list(cost=c(0.001, 0.01, 0.1, 1,5,10,100), 
                               gamma= c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod <- tune.out$best.model
#######################################################################
### Polynomial
set.seed(1)
tune.out <- tune(svm, mileage ~., data=data, kernel = "polynomial",
                 ranges = list(cost=c(0.001, 10,100,1000000,10000000000),
                               degree=c(2,3,4)))
summary(tune.out)
# (d) Make some plots to back up your assertions in (b) and (c).
# Hint: In the lab, we used the plot() function for svm objects
# only in cases with p = 2. When p > 2, you can use the plot()
# function to create plots displaying pairs of variables at a time.
# Essentially, instead of typing plot(svmfit , dat)
# where svmfit contains your fitted model and dat is a data frame
# containing your data, you can type plot(svmfit , dat , x1~x4)
# in order to plot just the first and fourth variables. However, you
# must replace x1 and x4 with the correct variable names. To find
# out more, type ?plot.svm.
# library(ISLR)
# data <- Auto
# data$aaa <- data$mpg
# median(data$mpg)
# mileage <- ifelse(data$mpg <= 22.5,0,1)
# data$mileage <- as.factor(mileage)
# data <- data[,2:11]
# library(e1071)
# svmfit_RBF <- svm(mileage ~.-aaa, data=data, kernel ="radial", gamma = 0.1,
#                   cost = 10)
# 
# plot(svmfit_RBF, data[,-aaa], horsepower ~ acceleration)
###################################################################
# Exercise 8 
# This problem involves the OJ data set which is part of the ISLR package.
data <- OJ
# (a) Create a training set containing a random sample of 800
# observations, and a test set containing the remaining
# observations.
set.seed(5)
train <- sample(1:1070, 800)
datatraining <- data[train,]
datatesting <- data[-train,]
# (b) Fit a support vector classifier to the training data using
# cost=0.01, with Purchase as the response and the other variables
# as predictors. Use the summary() function to produce summary
# statistics, and describe the results obtained.
svmfit_linear <- svm(Purchase~., data=datatraining, 
                     kernel ="linear", cost = 0.01,
                     scale =FALSE)
summary(svmfit_linear)
# (c) What are the training and test error rates?
# training error rates
ypred <- predict(svmfit_linear ,datatraining)
table(predict = ypred , truth= datatraining$Purchase)
(457+148)/800
### testing error rates
ypred <- predict(svmfit_linear ,datatesting)
table(predict = ypred , truth= datatesting$Purchase)
(46+155)/270
# (d) Use the tune() function to select an optimal cost. Consider values
# in the range 0.01 to 10.
set.seed(2)
tune.out <- tune(svm, Purchase ~ . , data=datatraining ,kernel = "linear",
                 ranges = list(cost=c(0.001, 0.01, 0.1, 1,1000)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)
# (e) Compute the training and test error rates using this new value
# for cost.
svmfit_linear <- svm(Purchase~., data=datatraining, kernel ="linear", cost = 5,
                     scale =FALSE)
summary(svmfit_linear)
####################################################################
ypred <- predict(svmfit_linear ,datatraining)
table(predict = ypred , truth= datatraining$Purchase)
(431+238)/800
####################################################################
ypred <- predict(svmfit_linear ,datatesting)
table(predict = ypred , truth= datatesting$Purchase)
(76+142)/270
# (f) Repeat parts (b) through (e) using a support vector machine
# with a radial kernel. Use the default value for gamma.
set.seed(2)
tune.out <- tune(svm, Purchase ~ . , data=datatraining ,kernel = "radial",
                 ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)
#############
svmfit_RBF <- svm(Purchase~., data=datatraining, kernel ="radial",
              cost = 5)
ypred <- predict(svmfit_RBF ,datatraining)
table(predict = ypred , truth= datatraining$Purchase)
(451+246)/800
### testing error rates
ypred <- predict(svmfit_RBF ,datatesting)
table(predict = ypred , truth= datatesting$Purchase)
(75+142)/270
# (g) Repeat parts (b) through (e) using a support vector machine
# with a polynomial kernel. Set degree=2.
set.seed(2)
tune.out <- tune(svm, Purchase ~ . , data=datatraining ,kernel = "polynomial",
                 ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10)), degree=2)
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)
############# Polynomial kerner
svmfit_poly <- svm(Purchase~., data=datatraining, kernel ="polynomial",degree=2,
                  cost = 10)
ypred <- predict(svmfit_poly ,datatraining)
table(predict = ypred , truth= datatraining$Purchase)
(452+244)/800
### testing error rates
ypred <- predict(svmfit_poly ,datatesting)
table(predict = ypred , truth= datatesting$Purchase)
(75+141)/270
# (h) Overall, which approach seems to give the best results on this
# data?
# Based on what I have seen on the 3 different kernels, I would say that the 
# linear kernel seems to give the best results. It since has the 
# best performance on the testing dataset
###################################################################
set.seed(2)
tune.out <- glm(Purchase ~ . , data=datatraining, family="binomial")
a <- ifelse(predict(tune.out, datatesting, type="response")>0.5,1,0)
table(a, datatesting$Purchase)
(145+77)/270
