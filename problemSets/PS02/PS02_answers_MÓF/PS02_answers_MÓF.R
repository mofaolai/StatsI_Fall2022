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
rt1 #27

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

#We then perform the test
bengalRegression <- lm(bengal_data$reserved ~ bengal_data$water)
bengalRegression

#to get the full results
summary(bengalRegression)

#plot Bengal regression
plot(bengalRegression)


plot(x = bengal_data$reserved,
     y = bengal_data$water,
     xlab = "Reserved Policy",
     ylab = "Water")

lm(bengal_data$water ~ bengal_data$reserved)
# Intercept: 12.738 (alpha, intercept)
# bengal_data$reserved: 9.252 (beta, slope)

with(bengal_data, plot(bengal_data$reserved, bengal_data$water))

abline(lm(bengal_data$water ~ bengal_data$reserved, data = bengal_data))


boxplot(bengal_data$reserved, bengal_data$water)
