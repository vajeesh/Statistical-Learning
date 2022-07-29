library(ISLR2)

# 
# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains
# a value above its median, and a 0 if mpg contains a value below
# its median. You can compute the median using the median()
# function. Note you may find it helpful to use the data.frame()
# function to create a single data set containing both mpg01 and
# the other Auto variables.
mpg01 <- (Auto$mpg > median(Auto$mpg)) * 1


Auto_2 <- data.frame(Auto)
Auto_2$mpg01 <- mpg01


# (b) Explore the data graphically in order to investigate the association
# between mpg01 and the other features. Which of the other
# features seem most likely to be useful in predicting mpg01? Scatterplots
# and boxplots may be useful tools to answer this question.
# Describe your findings.

X11() # Create a new window for the big pairs plot
pairs(Auto_2)


# (c) Split the data into a training set and a test set.
# Do a logistical regression

# Do a random train,test split of 80%
set.seed(1)
train <- rbinom(length(Auto_2$year), 1, 0.8)
Auto_2.Train <- Auto_2[train == 1, ]
Auto_2.Test <- Auto_2[train == 0, ]

# Run a GLM with all predictors "."
# except mpg and name
glm.fits <- glm(
  mpg01 ~ .-mpg-name,
  data = Auto_2, family = binomial
)

summary(glm.fits)
# Summary tells us that
# weight and year are very significant
# "***"

# Fit on the Training set with only 
# weight and year
glm.wy.fits <- glm(
  mpg01 ~ weight + year,
  data = Auto_2.Train, family = binomial()
)


# Do the same thing as in Question 14
glm.wy.prob <- predict(glm.wy.fits, Auto_2.Test,
                       type = "response")

glm.wy.pred <- rep(0, length(Auto_2.Test$mpg01))
glm.wy.pred[glm.wy.prob > .5] <- 1

# Results
table(glm.wy.pred, Auto_2.Test$mpg01)
prop.table(table(glm.wy.pred, Auto_2.Test$mpg01), 2)
mean(glm.wy.pred == Auto_2.Test$mpg01)

# ANSWER Our Predictor is really good
