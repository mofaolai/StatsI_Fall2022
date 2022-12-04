#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Question 1
install.packages('car')
library(car)
library(ggplot2)
data(Prestige)
help(Prestige)

# a. Create a new variable professional by recoding the 
# variable type so that professionals are coded as 1
# and blue and white collar workers are coded as 0 (
# Hint: ifelse)

Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
head(Prestige)

# b. Run a linear model with prestige as an outcome
# and income, professional and the interaction of 
# the two as predictors (Note: this is a continuous
# dummy interaction.)

prestige.lm <- lm(prestige ~ income * professional, data = Prestige)
summary(prestige.lm)

ggplot(Prestige, aes(x = income,
                     y = prestige,
                     col = professional)) +
  geom_point() +
  geom_abline()

# c. Write the prediction equation based on the 
# result.

# Yi = B0 + B1Xi + B2Di + B3XiDi + e

# B0 = intercept = 21.1422589
# B1 = 0.0031709
# B2 = 37.7812800
# B3 = -0.0023257

# Yi = 21.1422589 + 0.0031709Xi + 37.7812800Di + (-0.0023257)


# f.

# g.


# Question 2: Political Science

assigned.b1 <- 0.042
assigned.se <- 0.016
assigned.n <- 30

adjacent.b2 <- 0.042
adjacent.se <- 0.013
adjacent.n <-76

constant <- 0.302
constant.se <- 0.011

r.squared <- 0.094
n <- 131

# Degrees of freedom: = n - 3
df.b1 <- 131 - 3
df.b1
# a. Use the results form a linear regression to determine whether 
# having these yard signs in a precinct affects vote share (e.g., 
# conduct a hypothesis test with alpha = 0.05)

# y = B0 + B1X1 + B2X2
# y = (0.042)D1 + (0.042)D2 + 0.302

# y = vote propotion to Cuccinelli
# B0 = Constant
# X1 = Precinct assigned lawn signs
# X2 = Precint adjacent to lawn signs

# y = 0.302 + (0.042)(Assigned) + (0.042)(Adjacent)

# The question asks us to determine whether having these yard
# signs in a precinct affects vote share. This means we have 
# to see whether having a yard sign in one's precinct has an 
# effect on vote share.

# This means testing if the B1 predictor coefficient is zero:

# H0: B1 = 0
# Ha: B1 ≠ 0

# R^2 = 0.094
# k (number of predictors) = 2
# n (number of observations) = 131

# T = (m - mo) / SE
# Where T is test statistic, m is the slope, mo is 0, and SE is the standard
# error

t.statistic.assigned <- (assigned.b1) / (assigned.se)
t.statistic.assigned

# Our test statistic value is 2.65

# We can calculate the critical values of a two tailed 
# t distribution at a significance level of alpha = 0.05.

# If the value of our assigned t statistic is greater
# than the critical value of the t-distribution, we can
# reject the null hypothesis and conclude that B1 ≠ 0

# This would mean that a precinct having these yard signs 
# does affect vote share.

df.b1 <- 131 - 3
qt(p = 0.05/2, df = df.b1, lower.tail = FALSE)
# The critical value is 1.978671

# Since our calculated t-statistic is greater than our 
# critical value, we can reject the null hypothesis that
# having the yard signs in a precinct has no effect on 
# voteshare. We can therefore conclude that having these
# signs in a precinct does affect voteshare. 

# We can furthermore perform the hypothesis test by forming a
# confidence interval around B1. If 0 is not within the 
# confidence interval, we can reject the null hypothesis.

# At a significance level of alpha = 0.05, we create a 95% 
# confidence interval with B0: B0 +/- t x se

# CI = [b1 - 1.96x seb1, b1 + 1.96 x seb1 ]
# CI = [0.042 - 1.96(0.016), 0.042 + 1.96(0.016)]

# CI = [0.01064, 0.07336]

# Since 0 does not fall within the 95% confidence interval, 
# we can reject the null hypothesis that Bo = 0 and conclude
# that having these yard signs in a precinct does affect
# vote share.

# Part b

t.statistic.adjacent <- (adjacent.b2) / (adjacent.se)
t.statistic.adjacent

df.b2 = 131-3
qt(p = 0.05/2, df = df.b2, lower.tail = FALSE)

