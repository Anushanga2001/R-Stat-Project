########################################################################
#                       Two-sample tests of hypothesis 

## Two sample t-tests for independent variables

#Requirements:
#  1.The sampled populations follow the normal distribution (check Shapiro-Wilk test)
#  2.The sampled populations are independent. 

# Case I: Equal Population Variance
# Case II: Unequal Population Variance
# Check the equality of variances with "var.test"

# Hypothesis tesing:
help("t.test")

#t.test(sample1,sample2, alternative = c("two.sided", "less", "greater"), 
#       conf.level = 0.95, var.equal = FALSE)

# var.equal:	
# a logical variable indicating whether to treat the two variances as being equal. 
# If TRUE then the pooled variance is used to estimate the variance 
# otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.


## Two sample t-tests for paired samples

#Requirement:
#  1.The sampled populations follow the normal distribution (check Shapiro-Wilk test)

#t.test(sample1,sample2, alternative = c("two.sided", "less", "greater"), 
#       paired = TRUE, conf.level = 0.95)

# Note: paired = TRUE

########################################################################

# Application 1: A Customer Behavior Analysis

# A retail company wants to determine if there is a significant difference in 
# the average purchase amounts between two customer segments: 
# 1. online shoppers and 2. in-store shoppers. 
# They want to test whether online shoppers tend to spend more on average 
# compared to in-store shoppers.

# Sample data for online shoppers' purchase amounts
online_shoppers <- c(75, 80, 85, 90, 95, 100, 105, 110, 115, 120)
# Sample data for in-store shoppers' purchase amounts
in_store_shoppers <- c(60, 75, 115, 85, 140, 95, 100, 105, 110, 115)

#1. Test for normality:
#   H0= Sample data comes from a normally distributed population 
#   H1= Sample data does not come from a normally distributed population 
help("shapiro.test")

# H0= Sample data for online shoppers' purchase amounts comes from a normally distributed population 
shapiro.test(online_shoppers)
# Result: Since p-value = 0.8924 > 0.05, we do not reject H0.
# i.e, Sample data for online shoppers' purchase amounts is normally distributed.

shapiro.test(in_store_shoppers)
# i.e, Sample data for in-store shoppers' purchase amounts is normally distributed.

boxplot(online_shoppers, in_store_shoppers)


#2. Are the sampled populations independent?
# Yes, the since the purchase amounts of online shoppers does not depend on
# the purchase amounts of in-store shopper, the populations are independent.

#3. Test whether the population variances are equal or not
#   H0: The population variances are equal
#       F = Larger variance/smaller variance
help("var.test")
var.test(online_shoppers, in_store_shoppers)
# Result: p-value > 0.05, we do not reject H0
# The population variances are equal

# We want to check whether the online shoppers tend to spend more on average compared to in-store shopperss
# H0: online shoppers DO NOT tend to spend more on average compared to in-store shoppers
# H1: online shoppers tend to spend more on average compared to in-store shoppers
t.test(online_shoppers, in_store_shoppers, alternative = "greater", var.equal = TRUE)

# Since p-value > 0.05, we do not reject H0
# We do not have enough evidence to conclude that the online shoppers tend to 
# spend more on average compared to in-store shoppers at 0.05 significance level.

########################################################################
# Application 2: An IT Service Response Times

# An IT service provider wants to determine if there is a significant difference 
# in the response times of two customer support teams (Team A and Team B). 
# They collect data on response times for customer inquiries: 

# Response time data for two customer support teams
team_A_response <- c(12, 10, 14, 11, 13, 5, 21, 13, 14, 9, 15, 20, 18, 14)
team_B_response <- c(15, 16, 17, 15, 18, 19)

# Test the IT service provider's hypothesis at 0.01 level of significance.

# H0: there is no significant difference in the response times of two teams: mu_A = mu_B
# H1: there is a significant difference in the response times of two teams: mu_A != mu_B


## Checking for conditions:
#1. Test for normality:
shapiro.test(team_A_response)
shapiro.test(team_B_response)

#2. Are the sampled populations independent?
# Yes, the since the response times of two distinct teams has no such dependency
# the response time of Team A and Team B are independent.


#3. Test whether the population variances are equal or not
var.test(team_A_response, team_B_response)

## We want to check whether to check whether there is a significant difference in 
# the response times of two customer support teams (Team A and Team B).
# H0: there is no significant difference in the response times of two teams: mu_A = mu_B
# H1: there is a significant difference in the response times of two teams: mu_A != mu_B

t.test(team_A_response, team_B_response, alternative = "two.sided", var.equal = FALSE)

# Conclusion:


########################################################################
# Application 3: Manufacturing - Quality Control

# A manufacturer wants to assess whether a new production process improves 
# the quality of a product. They collect measurements of product quality 
# before and after implementing the new process in 20 production lines.

# Quality measurements before and after the new production process
# Line No      <-    1   2   3   4   5                                                      19  20
before_process <- c(72, 99, 81, 75, 93, 50, 75, 96, 71, 86, 97, 93, 49, 75, 74, 67, 79, 39, 78, 84)
after_process  <- c(75, 98, 85, 80, 95, 65, 84, 97, 74, 93, 95, 95, 55, 77, 81, 74, 82, 46, 84, 91)

# Test the manufacturer's hypothesis at 0.05 level of significance.

# H0: The new production process DOES NOT improve the quality of a product
# H1: The new production process improves the quality of a product

#1. Test for normality:
shapiro.test(before_process)
shapiro.test(after_process)

#2. Are the sampled populations independent?
# No, although the product quality in most lines have changed after the implementing 
# the new production method, production quality of a line is a unique characteristic 
# to the line itself.
# Hence, the production quality of each line after implementing the new production process
# is depend on the production quality of the line (before implementing the new production process).
# In a line, the product quality before and after are paired measurements (depend on each other).


## We want to test whether a new production process improves the quality of a product.
# H0: new production process does not improve the quality of a product: mu_Before >= mu_After
# H1: new production process improves the quality of a product: mu_Before < mu_After
t.test(before_process, after_process, alternative = "less", paired = TRUE)

# Alternatively, we can test the following:
# H0: new production process improves the quality of a product: mu_After >= mu_Before
# H1: new production process does not improve the quality of a product: mu_After < mu_Before
t.test(after_process, before_process, alternative = "less", paired = TRUE)