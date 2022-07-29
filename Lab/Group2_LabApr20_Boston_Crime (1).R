# 11. We will now try to predict per capita crime rate in the Boston data
# set.
library(ISLR2)
# (a) Try out some of the regression methods explored in this chapter,
# such as best subset selection, the lasso, ridge regression, and
# PCR. Present and discuss results for the approaches that you
# consider.

Boston
lapply(Boston, class)

Y <- Boston$crim
train <- (rbinom(length(Y), 1, 0.8) == 1)

Y_train <- Y[train]
Y_test <- Y[!train]

X_train  <- Boston[train,]
X_test <- Boston[!train,]

# Subset selection ----
lm.fit.all <- lm(crim~.,data = X_train)
summary(lm.fit.all)

# Do it automatically
step(lm(crim~zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+lstat+medv,
        data = X_train), direction="backward")

# Read output
lm.fit.best <- lm(formula = crim ~ zn + nox + dis + rad + ptratio + medv, data = X_train)

lm.pred <- predict(lm.fit.best, newdata = X_test)
lm.error <- sum((lm.pred - Y_test)**2)
lm.error

# lasso ----
x_matrix_train <- model.matrix(crim~., data = X_train)
library(glmnet)
lasso.fit <- cv.glmnet(x_matrix_train, Y_train, alpha = 1)
X11()
plot(lasso.fit)

# Lasso of exp(-1)
x_matrix_test <- model.matrix(crim~., data = X_test)

lasso.pred <- predict(lasso.fit,
                      newx = x_matrix_test,
                      lambda = exp(-1))

lasso.error <- sum((lasso.pred - Y_test)**2)
lasso.error
# ridge regression ----

ridge.fit <- cv.glmnet(x_matrix_train, Y_train, alpha = 0)
plot(ridge.fit)

ridge.pred <- predict(ridge.fit,
                      newx = x_matrix_test,
                      lambda = ridge.fit$lambda.min)

ridge.error <- sum((ridge.pred - Y_test)**2)
ridge.error
# pcr ----

library(pls)
pcr.fit <- pcr(crim~.,data = X_train,
               validation = "CV")
summary(pcr.fit)
# 3 components explain 99.59%
pcr.fit.best <- pcr(crim~.,data = X_train,
               ncomp = 3)

pcr.pred <- predict(pcr.fit.best,
                    newdata = X_test)

pcr.error <- sum((pcr.pred - Y_test)**2)
pcr.error

# Result: ----
# LM best model (so far)


# (b) Propose a model (or set of models) that seem to perform well on
# this data set, and justify your answer. Make sure that you are
# evaluating model performance using validation set error, crossvalidation,
# or some other reasonable alternative, as opposed to
# using training error.
# (c) Does your chosen model involve all of the features in the data
# set? Why or why not?