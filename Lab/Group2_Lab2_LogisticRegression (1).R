library(ISLR2)
# Chapter 4, Task 13 ----
# a) ----
# Produce some numerical and graphical summaries of the Weekly
# data. Do there appear to be any patterns?
colnames(Weekly)
summary(Weekly)

# Plot all boxplots
counter <- 1
for (x in Weekly){
  
  boxplot(x ~ Direction, data = Weekly,
          ylab = colnames(Weekly[counter]))

  counter <- counter +1
}
# ANSWER: No variable seems to be very special

# (b) Use the full data set to perform a logistic regression with
# Direction as the response and the five lag variables plus Volume
# as predictors. Use the summary function to print the results. Do
# any of the predictors appear to be statistically significant? If so,
# which ones?

glm.fits <- glm(
  Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
  data = Weekly, family = binomial
)

summary(glm.fits)

# ANSWER: Lag2 seems to be significants
# R sorts by alphabet for categorical
# -> We have the correlation to "Direction = Up"

# (c) Compute the confusion matrix and overall fraction of correct
# predictions. Explain what the confusion matrix is telling you
# about the types of mistakes made by logistic regression.

# 1. Get the probabilities using the predict-function
glm.probs <- predict(glm.fits, type = "response")

# Find out what R uses for 0 and 1
contrasts(Weekly$Direction) 

# Create predictions 
# 1. Set every prediction to "Down"
glm.pred <- rep("Down", length(Weekly$Direction))
# 2. If glm.prob is greater than 50% set prediction to "Up"
glm.pred[glm.probs > .5] <- "Up"

# Summary of the Predictions
table(glm.pred, Weekly$Direction)
# Shows that it is really bad at predicting down 
prop.table(table(glm.pred, Weekly$Direction), 2) 
# Proportion of correct guesses:
mean(glm.pred == Weekly$Direction)


# (d) Now fit the logistic regression model using a training data period
# from 1990 to 2008, with Lag2 as the only predictor. Compute the
# confusion matrix and the overall fraction of correct predictions
# for the held out data (that is, the data from 2009 and 2010).

# Train-Test split
Weekly.Train <- Weekly[Weekly$Year <= 2008, ]
Weekly.Test <- Weekly[Weekly$Year > 2008, ]


glm.lag2.fit <- glm(
  Direction ~ Lag2,
  data = Weekly.Train,
  family = binomial
)

summary(glm.lag2.fit)
# Predict, notice that we do it on the "Weekly.Test"
glm.lag2.probs <- predict(glm.lag2.fit, Weekly.Test,
                          type = "response")

# Again transform probabilities into predictions
glm.lag2.pred <- rep("Down", length(glm.lag2.probs))
glm.lag2.pred[glm.lag2.probs > .5] <- "Up"

# Summary of result
table(glm.lag2.pred, Weekly.Test$Direction)
prop.table(table(glm.lag2.pred, Weekly.Test$Direction), 2)
mean(glm.lag2.pred == Weekly.Test$Direction)

# ANSWER = Somehow the Test is better, probably random chance

