# 9. In this exercise, we will predict the number of applications received
# using the other variables in the College data set.
set.seed(1)
library(ISLR2)
# (a) Split the data set into a training set and a test set.
train <- rbinom(length(College$Private), 1, 0.8)
# (b) Fit a linear model using least squares on the training set, and
# report the test error obtained.
colSums(is.na(College)) # Check NA
lapply(College, class) # Check classes
names(College)
lm.fit <- lm(Apps~. , data = College, subset = train==1)
summary(lm.fit)

lm.fit <- update(lm.fit, .~.-Terminal)
summary(lm.fit)

lm.fit <- update(lm.fit, .~.-S.F.Ratio)
summary(lm.fit)

lm.fit <- update(lm.fit, .~.-Books)
summary(lm.fit)

lm.fit <- update(lm.fit, .~.-perc.alumni)
summary(lm.fit)

lm.fit <- update(lm.fit, .~.-Personal)
summary(lm.fit)

lm.pred <- predict(lm.fit, newdata = College[train == 0,])
length(lm.pred)


train_error <- sum((lm.pred - College[train == 0,]$Apps)**2) # Training error
train_error


# (c) Fit a ridge regression model on the training set, with ?? chosen
# by cross-validation. Report the test error obtained.

library(glmnet)
# Extract a model matrix for glmnet
x <- model.matrix(Apps~., data=College) 
dim(x) # Check if the command runs correctly

# Only run on train
x_train <- x[train == 1,]
y <- College$Apps[train == 1]

glmnet.fit <- cv.glmnet(x_train, y, alpha = 0)
glmnet.fit

# Best model = 376.4
#
# Plot the MSE for each lambda
plot(glmnet.fit)
(glmnet.fit$lambda.min) # Pick this as lambda
glmnet.pred <- predict(glmnet.fit, 
                       newx = x[train == 0,],
                       lambda = glmnet.fit$lambda.min)
length(glmnet.pred)
glmmet_error <- sum((glmnet.pred - College$Apps[train == 0])**2)
glmmet_error # Somehow worse than normal linear regression

# (d) Fit a lasso model on the training set, with ?? chosen by crossvalidation.
# Report the test error obtained, along with the number
# of non-zero coefficient estimates.

# glmnet alpha = 1 = lasso
lasso.fit <- cv.glmnet(x_train, y, alpha = 1)
plot(lasso.fit)
# Use exp(5) since that looks nice on the plot
lasso.pred <- predict(lasso.fit, 
                      newx = x[train == 0,],
                      lambda = exp(5)
                      )

lasso.error <- sum((lasso.pred - College$Apps[train == 0])**2)
lasso.error

# (e) Fit a PCR model on the training set, with M chosen by crossvalidation.
# Report the test error obtained, along with the value
# of M selected by cross-validation.

library(pls)
pcr.fit <- pcr(Apps~. , data = College, subset = train==1)
names(pcr.fit)

# We want fewer components, use crossvalidation
pcr.fit <- pcr(Apps~. , data = College, 
               subset = train==1,
               validation = "CV")
summary(pcr.fit)

# 6 components seems reasonable
pcr.fit <- pcr(Apps~. , data = College, 
               subset = train==1,
               ncomp = 6)
summary(pcr.fit)

pcr.pred <- predict(pcr.fit,
                    newdata = College[train == 0,])

pcr.error <- sum((pcr.pred - College$Apps[train == 0])**2)
pcr.error
# It's horrible!

test_error <- function(x){
  x.pred <- predict(x,newdata = College[train == 0,])
  x.error <- sum((x.pred - College$Apps[train == 0])**2)
  print(x.error)
}

# (f) Fit a PLS model on the training set, with M chosen by crossvalidation.
# Report the test error obtained, along with the value
#  of M selected by cross-validation.

# "WE DO NOT DO PLS" - Modud 2022

# (g) Comment on the results obtained. How accurately can we predict
# the number of college applications received? Is there much
# difference among the test errors resulting from these five approaches?
