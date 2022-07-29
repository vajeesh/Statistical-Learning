# In Chapter 4, we used logistic regression to predict the probability of
# default using income and balance on the Default data set. We will
# now estimate the test error of this logistic regression model using the
# validation set approach. Do not forget to set a random seed before
# beginning your analysis.

library(ISLR)
summary(Default)
attach(Default)
plot(density(Default$income))
plot(density(Default$balance))
# (a) Fit a logistic regression model that uses income and balance to predict default.

set.seed(1)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)

# (b) Using the validation set approach, estimate the test error of this model. 
# In order to do this, you must perform the following steps:

# i. Split the sample set into a training set and a validation set.
# ii. Fit a multiple logistic regression model using only the training observations.
# iii. Obtain a prediction of default status for each individual in
# the validation set by computing the posterior probability of
# default for that individual, and classifying the individual to
# the default category if the posterior probability is greater
# than 0.5.
# iv. Compute the validation set error, which is the fraction of
# the observations in the validation set that are misclassified.

FiveB = function() {
  # i.
  train = sample(dim(Default)[1], dim(Default)[1]/2)
  # ii.
  glm.fit = glm(default ~ income + balance, data = Default, family = binomial, 
                subset = train)
  # iii.
  glm.pred = rep("No", dim(Default)[1]/2)
  glm.probs = predict(glm.fit, Default[-train, ], type = "response")
  glm.pred[glm.probs > 0.5] = "Yes"
  # iv.
  return(mean(glm.pred != Default[-train, ]$default))
  #return(table(glm.pred,Default[-train, ]$default))
}
FiveB()

### read the dataset
normalized.dataset <- Default
### add the state to the previous dataset
setdiffDF <- function(A, B){ 
  f <- function(A, B) 
    A[!duplicated(rbind(B, A))[nrow(B) + 1:nrow(A)], ] 
  df1 <- f(A, B) 
  df2 <- f(B, A) 
  rbind(df1, df2) 
}
### set seed to reproduce the results and split the dataset into 10 folds
set.seed(41)
sample_set_1 <- normalized.dataset[sample(nrow(normalized.dataset), 
                                          as.integer(1000)), ]
test_set <- setdiffDF(sample_set_1,normalized.dataset)
sample_set_2 <-test_set[sample(nrow(test_set), 
                               as.integer(1000)), ]
test_set <- setdiffDF(sample_set_2,test_set)
sample_set_3 <- test_set[sample(nrow(test_set), 
                                as.integer(1000)), ]
test_set <- setdiffDF(sample_set_3,test_set)
sample_set_4 <- test_set[sample(nrow(test_set), 
                                as.integer(1000)), ]
test_set <- setdiffDF(sample_set_4,test_set)
sample_set_5 <- test_set[sample(nrow(test_set), 
                                as.integer(1000)), ]
test_set <- setdiffDF(sample_set_5,test_set)
sample_set_6 <- test_set[sample(nrow(test_set), 
                                as.integer(1000)), ]
test_set <- setdiffDF(sample_set_6,test_set)
sample_set_7 <-test_set[sample(nrow(test_set), 
                               as.integer(1000)), ]
test_set <- setdiffDF(sample_set_7,test_set)
sample_set_8 <- test_set[sample(nrow(test_set), 
                                as.integer(1000)), ]
test_set <- setdiffDF(sample_set_8,test_set)
sample_set_9 <- test_set[sample(nrow(test_set), 
                                as.integer(1000)), ]
sample_set_10 <- setdiffDF(sample_set_9,test_set)
################################################################################
samples <- list(sample_set_1,sample_set_2,sample_set_3,sample_set_4,sample_set_5,
                sample_set_6,sample_set_7,sample_set_8,sample_set_9,sample_set_10)
#### svm model #################################################################
#### store predictions from the model and the actual states
predictions <- c()
states <- c()
for (i in 1:10) {
  
  sample_set <- setdiffDF(as.data.frame(samples[i]),normalized.dataset)
  fit4 <- glm(default ~ income + balance, family = binomial, 
              data = sample_set)

  glm.pred = rep(1, 1000)
  glm.probs = predict(fit4, as.data.frame(samples[i]), type="response")
  glm.pred[glm.probs > 0.5] = 2
  predictions = c(predictions,glm.pred)
  states <- c(states,as.data.frame(samples[i])$default)
  
}
table(predictions,states)
226/9854
39/146

# (c) Repeat the process in (b) three times, using three different splits
# of the observations into a training set and a validation set. Comment
# on the results obtained.

FiveB()
FiveB()
FiveB()

# (d) Now consider a logistic regression model that predicts the probability
# of default using income, balance, and a dummy variable for student. 
# Estimate the test error for this model using the validation
# set approach. Comment on whether or not including a dummy variable for student 
# leads to a reduction in the test error rate.

train = sample(dim(Default)[1], dim(Default)[1]/2)
glm.fit = glm(default ~ income + balance + student, data = Default, family = binomial, 
              subset = train)
glm.pred = rep("No", dim(Default)[1]/2)
glm.probs = predict(glm.fit, Default[-train, ], type = "response")
glm.pred[glm.probs > 0.5] = "Yes"
mean(glm.pred != Default[-train, ]$default)
