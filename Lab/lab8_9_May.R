#7. In the lab, we applied random forests to 
#the Boston data using mtry = 6
#  and using ntree = 25 and ntree = 500. 

library(MASS)
#install.packages("randomForest") do this if you have not installed it.
library(randomForest)


# Construct the train and test matrices
set.seed(1234)
train = sample(dim(Boston)[1], dim(Boston)[1]*0.8) 
X.train = Boston[train, -14]
X.test = Boston[-train, -14]
Y.train = Boston[train, 14]
Y.test = Boston[-train, 14]

#set.seed(1234)
rf.boston = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                           mtry = 6, ntree = 500)

plot(1:500, rf.boston$test$mse, col = "green", type = "l", xlab = "Number of Trees", 
     ylab = "Test MSE", ylim = c(10, 19))


#Create a plot displaying the
#  test error resulting from random forests on this data set for a more
#  comprehensive range of values for mtry and ntree. You can model
#  your plot after Figure 8.10. Describe the results obtained.
p.1 = 3
p.2 = p/2
p = dim(Boston)[2] - 1


rf.boston.p = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                           mtry = p, ntree = 500)
rf.boston.p.2 = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                             mtry = p.2, ntree = 500)
rf.boston.p.1 = randomForest(X.train, Y.train, xtest = X.test, ytest = Y.test, 
                              mtry = p.1, ntree = 500)


plot(1:500, rf.boston.p$test$mse, col = "green", type = "l", xlab = "Number of Trees", 
     ylab = "Test MSE", ylim = c(10, 19))
lines(1:500, rf.boston.p.1$test$mse, col = "red", type = "l")
lines(1:500, rf.boston.p.2$test$mse, col = "blue", type = "l")
legend("topright", c("m=p", "m=p/2", "m=3"), col = c("green", "red", "blue"), 
       cex = 1, lty = 1)


# 8. In the lab, a classification tree was applied to the Carseats data set after
# converting Sales into a qualitative response variable. Now we will
# seek to predict Sales using regression trees and related approaches,
# treating the response as a quantitative variable.


# (a) Split the data set into a training set and a test set.
library(ISLR2)
attach(Carseats)
set.seed(1234)

train = sample(dim(Carseats)[1], dim(Carseats)[1]*0.8)
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[-train, ]

# (b) Fit a regression tree to the training set. Plot the tree, and interpret
# the results. What test MSE do you obtain?

library(tree)
tree.carseats = tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0)
#MSE
pred.carseats = predict(tree.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.carseats)^2)


#   (c) Use cross-validation in order to determine the optimal level of
# tree complexity. Does pruning the tree improve the test MSE?
cv.carseats = cv.tree(tree.carseats, FUN = prune.tree)
par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

# Best size = 9
pruned.carseats = prune.tree(tree.carseats, best = 9)
par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty = 0)

pred.pruned = predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.pruned)^2)


#   (d) Use the bagging approach in order to analyze this data. What
# test MSE do you obtain? Use the importance() function to determine
# which variables are most important.
library(randomForest)
bag.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, 
                            importance = T)
bag.pred = predict(bag.carseats, Carseats.test)
mean((Carseats.test$Sales - bag.pred)^2)

importance(bag.carseats)

# (e) Use random forests to analyze this data. What test MSE do you
# obtain? Use the importance() function to determine which variables
# aremost important. Describe the effect of m, the number of
# variables considered at each split, on the error rate
# obtained.


rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = 5, ntree = 500, 
                           importance = T)
rf.pred = predict(rf.carseats, Carseats.test)
mean((Carseats.test$Sales - rf.pred)^2)

importance(rf.carseats)




for( i in 1:10)
{
  set.seed(1234)# if you wanna get the same outputs.
  rf.carseats = randomForest(Sales ~ ., data = Carseats.train, mtry = i, ntree = 500, 
                             importance = T)
  rf.pred = predict(rf.carseats, Carseats.test)
  print(i)
  print(mean((Carseats.test$Sales - rf.pred)^2))
}


#11. This question uses the Caravan data set.
#(a) Create a training set consisting of the first 1,000 observations,
#and a test set consisting of the remaining observations.

library(ISLR2)
train = 1:1000
attach(Caravan)
Caravan$Purchase = ifelse(Caravan$Purchase == "Yes", 1, 0)
Caravan.train = Caravan[train, ]
Caravan.test = Caravan[-train, ]

#(b) Fit a boosting model to the training set with Purchase as the
#response and the other variables as predictors. Use 1,000 trees,
#and a shrinkage value of 0.01. Which predictors appear to be
#the most important?
library (MASS)
library(gbm)

set.seed(1234)
boost.caravan = gbm(Purchase ~ ., data = Caravan.train, n.trees = 1000, shrinkage = 0.01, 
                    distribution = "bernoulli",verbose = F)
summary(boost.caravan)

#(c) Use the boosting model to predict the response on the test data.
#Predict that a person will make a purchase if the estimated probability
#of purchase is greater than 20 %. Form a confusion matrix.
#What fraction of the people predicted to make a purchase
#do in fact make one? How does this compare with the results
#obtained from applying KNN or logistic regression to this data
#set?
boost.prob = predict(boost.caravan, Caravan.test, n.trees = 1000, type = "response")
boost.pred = ifelse(boost.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, boost.pred)

#31/(127 + 31) About 19.6% of people predicted to make purchase.


lm.caravan = glm(Purchase ~ ., data = Caravan.train, family = binomial)
lm.prob = predict(lm.caravan, Caravan.test, type = "response")
lm.pred = ifelse(lm.prob > 0.2, 1, 0)
table(Caravan.test$Purchase, lm.pred)

#58/(350 + 58) About 14.2% of people predicted to make purchase. 



