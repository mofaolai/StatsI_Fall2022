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
library(stargazer)
library(ggplot2)
library(broom)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/incumbents_subset.csv")
data <- read.csv('https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/incumbents_subset.csv')


# Question 1
# 1. Run a regression where the outcome variable is voteshare and the explantory
# variable is difflog.

# Outcome variable - dependent - y - voteshare
# Explanatory variable - independent - x - difflog

summary(data)
summary(data$voteshare)
summary(data$difflog)

# I. Independence between variables: only one explanatory variable, so we don't 
# have to check for any hidden relationships between variables

# II. Normality. We must check whether the dependent variable follows a normal
# distirbution, using the hist() function 

hist(data$voteshare)

# The histogram is roughly bell-shaped so we can proceed with the linear
# regression

# III. Linearity. We must check if the relationship between the explanatory and 
# response variable is linear, using a scatter plot.
plot(voteshare ~ difflog, data = data)
abline(lm(data$voteshare~data$difflog), col = "blue")

# The data does look roughly linear, so we can proceed with the linear model.

# IV. Homoscedacity - homogeneity of variance. This means the prediction error 
# doesn't change much over the range of prediction of the model.

# Performing the regression

# Relationship looks roughly linear, so we can proceed with the linear model
voteshare.difflog.lm <- lm(voteshare ~ difflog, data = data)
summary(voteshare.difflog.lm)
stargazer(voteshare.difflog.lm, type='latex', summary= FALSE)

#2. Make a scatterplot of the two variables and add the regression line

ggplot(data, aes(x=difflog, y=voteshare))+
  geom_point()+
  geom_smooth(method=lm)

#3. Save the residuals of the model in a separate object
residualsVoteshareDifflog <- lm(voteshare ~ difflog, data = data)$resid
residualsVoteshareDifflog

# 4. Write the prediction equation

# y = mx + c
# y = mx + 0.579031
# y = b0 + b1x
# b0 = y intercept = 0.579031
# b1 = slope = 0.041666
# y = 0.569031 + 0.041666x

# Question 2: 

# We are interested in knowing how the difference between incumbent and 
# challenger's spending and the vote of the presidential candidate of the 
# incumbent's party are related

# 1. Run a regression where the outcome variable is presvote and the explanatory
# variable is difflog

# I. Independence of observations - only one explanatory and one outcome
# response variable, therefore we don't need to test for any hidden 
# relationships among variables

# II. Normality (of outcome variable), using hist() function. It is roughly
# normally distributed.
hist(data$presvote)

# III. Linearity (between explanatory and outcome variable). Data looks roughly
# linear.
plot(data$presvote ~ data$difflog, data = data)
abline(lm(data$presvote~data$difflog), col = "blue")

# IV. Homoscedacity

# Performing the regression
presvote.difflog.lm <- lm(presvote ~ difflog, data = data)
summary(presvote.difflog.lm)
stargazer(presvote.difflog.lm, type='latex', summary= FALSE)

# 2. Make a scatterplot of the two variables and add the regression line.
ggplot(data, aes(x=difflog, y=presvote))+
  geom_point()+
  geom_smooth(method=lm)

# 3. Save the residuals of the model in a separate object.
residualsPresvoteDifflog <- lm(presvote ~ difflog, data = data)$resid
residualsPresvoteDifflog

# 4. Write the prediction equation.

# y = b0 + b1x
# b0 = y intercept = 0.507583
# b1 = slope = 0.023837
# y = 0.507583 + 0.023837x

# Question 3
# We are interested in knowing how the vote share of the presidential candidate
# incumbent's party is associated with the incumbent's electoral success.

# 1. Run a regression where the outcome variable is voteshare and the
# explanatory variable is presvote.

# I. Independence of observations. Only one explanatory and one outcome response
# variable, therefore we don't need to test for any hidden relationships among
# variables.

# II. Normality (of dependent variable), using hist() function. It is roughly 
# normally distributed.
hist(data$voteshare)

# III. Linearity (between outcome and explanatory variable). We check using a 
# scatter plot. The relationship seems roughly linear.
plot(data$voteshare ~ data$presvote)
abline(lm(data$voteshare~data$presvote), col = "blue")

# IV. Homoscedacity

# Performing the regression
voteshare.presvote.lm <- lm(voteshare ~ presvote, data = data)
summary(voteshare.presvote.lm)
stargazer(voteshare.presvote.lm, type='latex', summary= FALSE)

#2. Make a scatterplot of the two variables and add the regression line.
ggplot(data, aes(x=presvote, y=voteshare))+
  geom_point()+
  geom_smooth(method=lm)

# 3. Write the prediction equation
# y = b0 + b1x
# y = 0.441330 + 0.388018x

# Question 4

# 1. Run a regression where the outcome variable is the residuals from Question 
# 1 and the explanatory variable is the residuals from Question 2.

# Outcome - y - residuals from Q1 - residualsVoteshareDifflog
# Explanatory - x - residuals from Q2 - residualsPresvoteDifflog

# I. Independence of observations. Only one explanatory and one outcome response
# variable, therefore we don't need to test for any hidden relationships among
# variables.

# II. Normality (of outcome variable), using hist() function. It is roughly
# normally distributed. We can see it is roughly normally distributed
hist(residualsVoteshareDifflog)

# III. Linearity (between outcome and explanatory variable). We check using a 
# scatter plot. The relationship seems roughly linear.
plot(residualsVoteshareDifflog ~ residualsPresvoteDifflog)
abline(lm(residualsVoteshareDifflog~residualsPresvoteDifflog), col = "blue")

# IV. Homoscedacity

# Running the regression
rVoteshareDifflog.rPresvoteDifflog.lm <- lm(residualsVoteshareDifflog ~ residualsPresvoteDifflog, data = data)
summary(rVoteshareDifflog.rPresvoteDifflog.lm)
stargazer(rVoteshareDifflog.rPresvoteDifflog.lm, type='latex', summary= FALSE)

# 2. Make a scatterplot of the two residuals and add the regression line.
ggplot(data, aes(x=residualsPresvoteDifflog, y=residualsVoteshareDifflog))+
  geom_point()+
  geom_smooth(method=lm)

# 3. Write the prediction equation.
# y = b0 + b1x
# y = -4.860e-18 + 2.569e-01x

# Question 5

# 1. Run a regression where the outcome variable is the incumbent's voteshare
# and the explanatory variables are difflog and presvote.

# Outcome variable - y - voteshare
# Explanatory variables - x - difflog - presvote

# I. Independence of observations. Using cor() function, make sure difflog and 
# presvote aren't too highly correlated. Correlation is 0.2965653

cor(data$difflog, data$presvote)

# II. Normality. Make sure the outcome variable follows a normal distribution.
# It is roughly normally distributed.

hist(data$voteshare)

# III. Linearity. We can check this using two scatterplots
plot(data$voteshare ~ data$difflog)
abline(lm(data$voteshare~data$difflog), col = "blue")

# This one is roughly linear, as is the following one
plot(data$voteshare ~ data$presvote)
abline(lm(data$voteshare~data$presvote), col = "blue")

#IV. Homoscedacity.

#Running the regression
voteshare.difflog.presvote.lm <- lm(voteshare ~ difflog + presvote, data = data)
summary(voteshare.difflog.presvote.lm)
stargazer(voteshare.difflog.presvote.lm, type='latex', summary= FALSE)

# A one unit increase in difflog results in a 0.0355431 increase in voteshare.
# A one unit increase in presvote results in a 0.2568770 increase in voteshare.




