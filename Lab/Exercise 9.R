# 9. We will now consider the Boston housing data set, from the MASS
# library.

library(MASS)
summary(Boston)
set.seed(1)
attach(Boston)

# (a) Based on this data set, provide an estimate for the population
# mean of medv. Call this estimate ^??.

medv.mean = mean(medv)
medv.mean

# (b) Provide an estimate of the standard error of ^??. Interpret this
# result.
# Hint: We can compute the standard error of the sample mean by
# dividing the sample standard deviation by the square root of the
# number of observations.

medv.err = sd(medv)/sqrt(length(medv))
medv.err

# (c) Now estimate the standard error of ^?? using the bootstrap. How
# does this compare to your answer from (b)?

boot.fn = function(data, index) return(mean(data[index]))
library(boot)
bstrap = boot(medv, boot.fn, 1000)
bstrap

#   (d) Based on your bootstrap estimate from (c), provide a 95% confidence
# interval for the mean of medv. Compare it to the results
# obtained using t.test(Boston$medv).
# Hint: You can approximate a 95% confidence interval using the
# formula [^?? ??? 2SE(^??), ^?? + 2SE(^??)].

t.test(medv)
c(bstrap$t0 - 2 * 0.4119, bstrap$t0 + 2 * 0.4119)

# (e) Based on this data set, provide an estimate, ^??med, for the median
# value of medv in the population.

medv.med = median(medv)
medv.med

# (f) We now would like to estimate the standard error of ^??med. Unfortunately,
# there is no simple formula for computing the standard
# error of the median. Instead, estimate the standard error of the
# median using the bootstrap. Comment on your findings.

boot.fn = function(data, index) return(median(data[index]))
boot(medv, boot.fn, 1000)

# (g) Based on this data set, provide an estimate for the tenth percentile
# of medv in Boston suburbs. Call this quantity ^??0.1. (You
#                                                       can use the quantile() function.)

medv.tenth = quantile(medv, c(0.1))
medv.tenth


# (h) Use the bootstrap to estimate the standard error of ^??0.1. Comment
# on your findings.
boot.fn = function(data, index) return(quantile(data[index], c(0.1)))
boot(medv, boot.fn, 1000)
