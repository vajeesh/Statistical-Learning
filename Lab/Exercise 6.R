# We continue to consider the use of a logistic regression model to
# predict the probability of default using income and balance on the
# Default data set. In particular, we will now compute estimates for
# the standard errors of the income and balance logistic regression coefficients
# in two different ways: (1) using the bootstrap, and (2) using
# the standard formula for computing the standard errors in the glm()
# function. Do not forget to set a random seed before beginning your
# analysis.
# (a) Using the summary() and glm() functions, determine the estimated
# standard errors for the coefficients associated with income
# and balance in a multiple logistic regression model that uses
# both predictors.

set.seed(1)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)

# (b) Write a function, boot.fn(), that takes as input the Default data
# set as well as an index of the observations, and that outputs
# the coefficient estimates for income and balance in the multiple
# logistic regression model.

boot.fn = function(data, index) return(coef(glm(default ~ income + balance, 
                                data = data, family = binomial, subset = index)))
boot.fn(Default,9800:9900)
# (c) Use the boot() function together with your boot.fn() function to
# estimate the standard errors of the logistic regression coefficients
# for income and balance.

library(boot)
boot(Default, boot.fn, 50)

# (d) Comment on the estimated standard errors obtained using the
# glm() function and using your bootstrap function.