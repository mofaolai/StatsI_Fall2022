##############
# Question 1 #
##############

fo1 <- 14
fo2 <- 7
fo3 <- 6
fo4 <- 7
fo5 <- 7
fo6 <-1

rt1 <- 14 + 6 + 7
rt1 # 27

rt2 <- 7+7+1
rt2 # 15

ct1 <- 14+7
ct1 # 21

ct2 <- 6+7
ct2 # 13

ct3 <- 7+1
ct3 # 8

total = rt1 + rt2
total #42

fe1 <- (rt1)*(ct1)/total
fe1 # 13.5

fe2 <- (rt2)*(ct1)/total
fe2 # 7.5

fe3 <- (rt1)*(ct2)/total
fe3 # 8.357143

fe4 <- (rt2)*(ct2)/total
fe4 # 4.642857

fe5 <- (rt1)*(ct3)/total
fe5 # 5.142857

fe6 <-(rt2)*(ct3)/total
fe6  # 2.857143

# chi squared
chi_squared <- (((fo1-fe1)^2)/fe1) + (((fo2-fe2)^2)/fe2) + (((fo3-fe3)^2)/fe3) +
  (((fo4-fe4)^2)/fe4) + (((fo5-fe5)^2)/fe5) + (((fo6-fe6)^2)/fe6)

chi_squared
# 3.791168

# (b) degrees of freedom = (rows-1)*(columns -1)
df1 <- (2-1)*(3-1)
df1 # 2

# Calculating the p-values for chi-squared tests:
pv1 <- pchisq(chi_squared, df = df1, lower.tail = FALSE)
pv1 # p-value = 0.1502306

# (c) Calculating the standardised residuals
sr1 <- (fo1 -fe1)/(sqrt(fe1*(1-(rt1/total))*(1-(ct1/total))))
sr1 #0.3220306

sr2 <- (fo2 -fe2)/(sqrt(fe2*(1-(rt2/total))*(1-(ct1/total))))
sr2 # -0.3220306

sr3 <- (fo3 -fe3)/(sqrt(fe3*(1-(rt1/total))*(1-(ct2/total))))
sr3 # -1.642957

sr4 <- (fo4 -fe4)/(sqrt(fe4*(1-(rt2/total))*(1-(ct2/total))))
sr4 # 1.641957

sr5 <- (fo5 -fe5)/(sqrt(fe5*(1-(rt1/total))*(1-(ct3/total))))
sr5 # 1.523026

sr6 <- (fo6 -fe6)/(sqrt(fe6*(1-(rt2/total))*(1-(ct3/total))))
sr6 # -1.523026


############
#Question 2#
############

# The first step is to import the data
bengal_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T)

# We then carry out the bivariate regression
xreserved <- bengal_data$reserved
ywater <- bengal_data$water

bengalRegression <- lm(ywater ~ xreserved)
bengalRegression

# To get the full results
summary(bengalRegression)

# We get an estimate of the (alpha) intercept of 14.738. This is the value of
# the water variable when x = 0

# We get an estimate of the (beta) slope of 9.252. This is the value of the
# slope of our linear equation. If the reserved policy increaseds by one, on
# average the number of new or repaired water facilities increases by 9.252.

# We wish to decide whether or not to accept or reject the null hypothesis.
# We decide to use an significance level (alpha) of 0.05.

# Since this is a two tailed hypothesis, we divide 0.05 by two and get a 
# critical value is 0.025 on both sides of the distribution. 

# Since the p-value of the regression is 0.0197 and less than the critical value
# of 0.025, we can reject the null hypothesis that the reservation policy has no 
# impact on the number of new or repaired drinking water facilities in the 
# villages and accept the alternative hypothesis that the reservation policy
# does have and impact on the number of new or repaired drinking water 
# facilities.

