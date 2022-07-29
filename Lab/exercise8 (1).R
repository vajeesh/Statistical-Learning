# In Section 10.2.3, a formula for calculating PVE was given in Equation
# 10.8. We also saw that the PVE can be obtained using the sdev
# output of the prcomp() function.
# On the USArrests data, calculate PVE in two ways:
# (a) Using the sdev output of the prcomp() function, as was done in
# Section 10.2.3.
data <- USArrests
pr.out <- prcomp(USArrests, scale = TRUE)
cor(data$Murder, data$Rape)
cor(data$Murder, data$Assault)
cor(data$Assault, data$Rape)

cor(data$UrbanPop, data$Rape)
cor(data$UrbanPop, data$Assault)
cor(data$UrbanPop, data$Murder)
pr.out$rotation
summary(pr.out)
pr.var <- pr.out$sdev^2
pr.var
pve <- pr.var/sum(pr.var)
pve
reduced_data <- as.data.frame(pr.out$x[,1:2])
#################################################
pr.out$sdev
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)
#################################################
# (b) By applying Equation 10.8 directly. That is, use the prcomp()
# function to compute the principal component loadings. Then,
# use those loadings in Equation 10.8 to obtain the PVE.
# These two approaches should give the same results.
# Hint: You will only obtain the same results in (a) and (b) if the same
# data is used in both cases. For instance, if in (a) you performed
# prcomp() using centered and scaled variables, then you must center
# and scale the variables before applying Equation 10.3 in (b).
scaled_data <- apply(USArrests, 2, scale)
x_totalv <- sum(scaled_data^2)
####################################################################
comp1 <- unname(pr.out$rotation[,1])
comp2 <- unname(pr.out$rotation[,2])
comp3 <- unname(pr.out$rotation[,3])
comp4 <- unname(pr.out$rotation[,4])
####################################################################
a1 <- sum((as.matrix(t(comp1)) %*% as.matrix(t(scaled_data)))^2)
a2 <- sum((as.matrix(t(comp2)) %*% as.matrix(t(scaled_data)))^2)
a3 <- sum((as.matrix(t(comp3)) %*% as.matrix(t(scaled_data)))^2)
a4 <- sum((as.matrix(t(comp4)) %*% as.matrix(t(scaled_data)))^2)
sum(a1+a2+a3+a4)
#########################################
a1/x_totalv
a2/x_totalv
a3/x_totalv
a4/x_totalv
