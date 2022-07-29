# In this problem, you will generate simulated data, and then perform
# PCA and K-means clustering on the data.
# (a) Generate a simulated data set with 20 observations in each of
# three classes (i.e. 60 observations total), and 50 variables.

set.seed(1)
x <- matrix(rnorm(60*50) , ncol =50)
x[1:20,] <- x[1:20,]+7
x[21:40,] <-  x[21:40,]-7
y <- c(rep(1,20), rep(2,20), rep(3,20))
dat <- data.frame(x=x, y=as.factor(y))
# (b) Perform PCA on the 60 observations and plot the first two principal
# component score vectors. Use a different color to indicate
# the observations in each of the three classes. If the three classes
# appear separated in this plot, then continue on to part (c). If
# not, then return to part (a) and modify the simulation so that
# there is greater separation between the three classes.

pr.out <- prcomp(dat[,1:50], scale = TRUE)
summary(pr.out)
pr.out$rotation[,1]
plot(pr.out)
plot(pr.out$x[,1], pr.out$x[,2], col =y)