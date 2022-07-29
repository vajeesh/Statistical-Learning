# 13. This question should be answered using the "Weekly" data set, which
# is part of the "ISLR" package. This data is similar in nature to the
# "Smarket" data from this lab, except that it contains 1, 089
# weekly returns for 21 years, from the beginning of 1990 to the end of
# 2010.


# (a) Produce some numerical and graphical summaries of the Weekly
# data. Do there appear to be any patterns?
library(ISLR)
summary(Weekly)

pairs(Weekly)
cor(Weekly[, -9])

#-> Only "Year and Volume" have a relationship. 

#(b) Use the full data set to perform a logistic regression with
# Direction as the response and the five lag variables plus Volume
# as predictors. Use the summary function to print the results. Do
# any of the predictors appear to be statistically significant? If so,
# which ones?

attach(Weekly)
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
              data = Weekly,family = binomial)
summary(glm.fit)

#->lag2 has a relationship wih Direction.

#(c) Compute the confusion matrix and overall fraction of correct
# predictions. Explain what the confusion matrix is telling you
# about the types of mistakes made by logistic regression.
glm.probs = predict(glm.fit, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
table(glm.pred, Direction)

# (54+557)/(54+557+48+430) = 56.1%.
#  Weeks the market goes up the logistic regression 
#is right most of the time, 557/(557+48) = 92.1%.
#Weeks the market goes up the logistic regression is 
#wrong most of the time 54/(430+54) = 11.2%.

#(d) Now fit the logistic regression model using a training data period
# from 1990 to 2008, with Lag2 as the only predictor. Compute the
# confusion matrix and the overall fraction of correct predictions
# for the held out data (that is, the data from 2009 and 2010).

train = (Year < 2009)
Weekly.0910 = Weekly[!train, ]
glm.fit = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
glm.probs = predict(glm.fit, Weekly.0910, type = "response")
glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > 0.5] = "Up"
Direction.0910 = Direction[!train]
table(glm.pred, Direction.0910)
mean(glm.pred == Direction.0910)
#0.625 ~ 62.5%


#(e) Repeat (d) using LDA.
library(MASS)
lda.fit = lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.pred = predict(lda.fit, Weekly.0910)
table(lda.pred$class, Direction.0910)
mean(lda.pred$class == Direction.0910)
#0.625 ~ 62.5%


# (f) Repeat (d) using QDA.
qda.fit = qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.class = predict(qda.fit, Weekly.0910)$class
table(qda.class, Direction.0910)
mean(qda.class == Direction.0910)
#0.5865 ~ 58.65%


# (g) Repeat (d) using KNN with K = 1.

library(class)
train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.0910)
mean(knn.pred == Direction.0910)
#0.5~ 50%