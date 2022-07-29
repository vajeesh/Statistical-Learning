#https://www.es.utoronto.ca/wp-content/uploads/2019/03/Lecture_09.pdf 


#LAB 7 
# 8. In the lab, a classification tree was applied to the Carseats data set after
# converting Sales into a qualitative response variable. Now we will
# seek to predict Sales using regression trees and related approaches,
# treating the response as a quantitative variable.
# (a) Split the data set into a training set and a test set. 50% for training and 50% for testing
library(ISLR2)
attach(Carseats)
set.seed(1234) #change the random seed

train = sample(dim(Carseats)[1], dim(Carseats)[1]*0.5)
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




# Best size = ???
pruned.carseats = prune.tree(tree.carseats, best = )#???)
par(mfrow = c(1, 1))
plot(pruned.carseats)
text(pruned.carseats, pretty = 0)

pred.pruned = predict(pruned.carseats, Carseats.test)
mean((Carseats.test$Sales - pred.pruned)^2)




#9. This problem involves the OJ data set which is part of the ISLR2
#package.
#(a) Create a training set containing a random sample of 800 observations,
#and a test set containing the remaining observations.
library(ISLR2)
attach(OJ)

set.seed(1234)
train =sample(dim(OJ)[1],800)
OJ.train = OJ[train, ]
OJ.test = OJ[-train, ]

#(b) Fit a tree to the training data, with Purchase as the response
#and the other variables as predictors. Use the summary() function
#to produce summary statistics about the tree, and describe the
#results obtained. What is the training error rate? How many
#terminal nodes does the tree have?

library(tree)
oj.tree = tree(Purchase~.,data = OJ.train)
summary((oj.tree))

#(c) Type in the name of the tree object in order to get a detailed
#text output. Pick one of the terminal nodes, and interpret the
#information displayed.
oj.tree


#(d) Create a plot of the tree, and interpret the results.
plot(oj.tree)
text(oj.tree)

#(e) Predict the response on the test data, and produce a confusion
#matrix comparing the test labels to the predicted test labels.
#What is the test error rate?

#check training error rate
oj.pred_train = predict(oj.tree, OJ.train, type = "class")
table(OJ.train$Purchase, oj.pred_train)
#check testing error rate
oj.pred = predict(oj.tree, OJ.test, type = "class")
table(OJ.test$Purchase, oj.pred)


#(f) Apply the cv.tree() function to the training set in order to
#determine the optimal tree size.
cv.oj = cv.tree(oj.tree, FUN = prune.tree)

#(g) Produce a plot with tree size on the x-axis and cross-validated
#classification error rate on the y-axis.

plot(cv.oj$size, cv.oj$dev, type = "b", xlab = "Tree Size", ylab = "Deviance")


#(h) Which tree size corresponds to the lowest cross-validated classification
#error rate?
#5

#(i) Produce a pruned tree corresponding to the optimal tree size
#obtained using cross-validation. If cross-validation does not lead
#to selection of a pruned tree, then create a pruned tree with five
#terminal nodes.

oj.pruned = prune.tree(oj.tree, best =5)

#(j) Compare the training error rates between the pruned and unpruned
#trees. Which is higher?

summary(oj.pruned)
#Misclassification error of pruned tree is exactly same as that of original tree.

#(k) Compare the test error rates between the pruned and unpruned
#trees. Which is higher?
pred.unpruned = predict(oj.tree, OJ.test, type = "class")
misclass.unpruned = sum(OJ.test$Purchase != pred.unpruned)
misclass.unpruned/length(pred.unpruned)

pred.pruned = predict(oj.pruned, OJ.test, type = "class")
misclass.pruned = sum(OJ.test$Purchase != pred.pruned)
misclass.pruned/length(pred.pruned)

#Pruned and unpruned trees have same test error rate.