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

lapply(c(),  pkgTest)

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2021/problem_sets/PS1")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# PS01 Marcus Ó Faoláin 16327268

#####
# 1. Find a 90% confidence interval for the average student IQ in the school.

# This means our confidence coefficient = 0.90

# Since n < 30, we must use a t-score instead of a Z score
# As the confidence coefficient is 0.90, we are looking for a t-value
# corresponding with (1-0.90)/2 = 0.05 using one tailed test scores or 0.1 for 
# two tailed test scores.

# Since degrees of freedom = (n-1), we calculate the degrees of freedom = (25-1)
# df = 24
# T-value at these points is 1.711

# We calculate the sample mean to be ¯x = 98.44
mean1 <- mean(y)

#We create an empty vector to hold the demeaned sum of yn.
demeaned_sum_y <- NULL

#We add the demeaned sum of each element of y to the demeaned_sum_y vector. This
#is yn - the mean of y (98.44)

for(i in 1: length(y)){
  demeaned_sum_y[i] <- y[i] - mean(y)
}

#We calculate the Squared Error for each element of demeaned_sum_y by squaring 
#each element.
squaredError_y <- demeaned_sum_y^2

#We then find the sum of the elements of the vector - 4114.16
sum(squaredError_y)

#We find the variance of y by dividing the sum of the squared errors of y by the
#number of elements minus 1. Variance = 171.4233
variance_y <- sum(squaredError_y)/(length(y)-1)

#We find the standard deviation by finding the square root of the variance. SD =
# 13.09287
sd1 <- sqrt(variance_y)

# We calculate the standard error using by dividing the standard deviation by
# the square root of the number of elements in y. SE = 2.618575
se1 <- sd1/sqrt(length(y))

# We now multiply the standard error by the t-value we found earlier = 4.480381
t_by_se1 <- 1.711*se1

# We now find the lower limit and the upper limit for the confidence interval by
# subtracting t_by_se1 from the mean and by adding it to it.

lower_limit1 <- mean1 - t_by_se1
upper_limit1 <- mean1 + t_by_se1
confidence_interval1 <- c(lower_limit1, upper_limit1)
confidence_interval1

# We calculate that the confidence interval is [93.95962, 102.92038]

# 2. Next, the school counselor was curious whether the average student in her 
# school is higher than the average IQ score (100) among all the schools in the 
# country.

# 5 steps to Hypothesis testing

# Step 1: Assumptions. Since n < 30, we will use a t-test to test the hypotheses.
# According to the question, the data was randomly selected and is a random sample
#

# Because we want to know whether the mean of the average school student is
# higher than the average among all schools in the country, we perform a one
# tailed test.

# Step 2: We set up our null and alternative hypotheses.

# H0 <- mu =< 100
# Null: The average IQ of the students in the school is less than or equal to 
# 100
# Ha <- mu > 100
# Alternative: The average IQ of the students in the school is greater than 100.

#Step 3: We calculate a test statistic.

# Test statistic= (¯x-mu0)/(s/√n)

ts1 <- (mean(y)-100)/(sd(y)/sqrt(25))
ts1

# We calculate the test statistic to be -0.5957439.
# The degrees of freedom are (n-1) = (25-1) = 24

# Step 4 - we must calculate the p-value

# We use the following formula in R to compute the probability of this test
# statistic occuring with degrees of freedom = 24.

p1 <- pt(-0.5957439, 24)
p1

# The p-value is equal to 0.2784617

# Since the p-value is greater than the alpha (0.2784617 > 0.05), the result is
# not extreme enough to be statistically significant. There is not 
# enough evidence allowing us to reject the null hypothesis and to accept the
# alternative hypothesis.

# Therefore, the school counselor cannot conclude that the average student IQ in
# her school is higher than the average IQ score (100) among all the schools in
# the country.

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", header=T)

#plotting
#Y Per capita expenditure on shelters/housing assistance in state
#X1 Per capita personal income in state
#X2 Number of residents per 100,000 that are 'financially insecure' in state
#X3 Number of people per thousand residing in urban areas in state.

# Please plot the relationships among Y, X1, X2, and X3? What are the 
# correlations among them (you just need to describe the graphs and the 
# relationships among them)?

#Plot X1 against Y
plot(x = expenditure$X1,
     y = expenditure$Y, 
     xlab ="Personal income (PC)", 
     ylab = "Expenditure on housing assistance (PC)", 
     main = "Personal Income (PC) VS Expenditure on Housing Assistance (PC)")

#Plot X2 against Y
plot(x = expenditure$X2, 
     y = expenditure$Y, 
     xlab ="Number of 'financially insecure' residents per 100,000", 
     ylab = "Expenditure on housing assistance (PC)", 
     main = "Number of 'Financially Insecure' per 100,000 VS Expenditure on 
     Housing Assistance (PC)")

#Plot X3 against Y
plot(x = expenditure$X3,
     y = expenditure$Y,
     xlab ="Number of people per 1,000 per state in urban areas",
     ylab = "Expenditure on housing assistance (PC)",
     main = "Number of People per 1,000 Residing in Urban Areas in State VS 
     Expenditure on Housing Assistance (PC)")

# Plot X2 against X1
plot(x = expenditure$X2,
     y = expenditure$X1, 
     xlab ="Residents per 100,000 that are 'financially insecure'", 
     ylab = "Personal Income (Per Capita)", 
     main = "Financially Insecure per 100,000 VS Personal Income (Per Capita)")

# Plot X3 against X1
plot(x = expenditure$X3,
     y = expenditure$X1, 
     xlab ="Number of people per 1,000 per state in urban areas", 
     ylab = "Personal Income (Per Capita)", 
     main = "Number of People per 1,000 Residing in Urban Areas in State VS Personal Income (Per Capita)")

# Plot X3 against X2
plot(x = expenditure$X3,
     y = expenditure$X2, 
     xlab ="Number of people per 1,000 per state in urban areas", 
     ylab = "Residents per 100,000 that are 'financially insecure'", 
     main = "Number of People per 1,000 Residing in Urban Areas in State VS Financially Insecure per 100,000")

# Please the relationship between Y and Region? On average, which region has the
# highest per capita expenditure on housing assistance.

boxplot(expenditure$Y ~ expenditure$Region, 
        data =expenditure, 
        main = "Expenditure on Housing Assistance per Region", 
        xlab="Region", 
        ylab="Expenditure on Housing", 
        col = "green",
        border = "red")

# On average, Region 4 (West) has the highest per capita expenditure on housing 
# assistance.

# Please plot the relationship between Y and X1
plot(x = expenditure$X1,
     y = expenditure$Y, 
     xlab ="Personal Income (PC)", 
     ylab = "Expenditure on Housing Assistance (PC)", 
     main = "Personal Income (PC) VS Expenditure on Housing Assistance (PC)")

# Describe this graph and the relationship. 

# The graph appears to indicate a positive correlation between personal income and
# expenditure on housing assistance.

# Reproduce the above graph including one more variable Region and display 
# different regions with different types of symbols and colours. 

plot(x = expenditure$X1,
     y = expenditure$Y, 
     xlab ="Personal Income (PC)", 
     ylab = "Expenditure on Housing Assistance (PC)", 
     main = "Personal Income (PC) VS Expenditure on Housing Assistance (PC)",
     col = c('red', 'green', 'blue', 'purple')[expenditure$Region],
     pch  = c(0, 1, 2, 5)[expenditure$Region]
     #1Northeast, 2NorthCentral, 3South, 4West
)


