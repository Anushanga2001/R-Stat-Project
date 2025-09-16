########################################################################
#                 One way repeated ANOVA

# We compare whether there is a difference between three or more groups 
# using ANalysis Of VAriance (ANOVA) technique

# We use one way repeated ANOVA test under the following conditions: 
# 1. Normality – using Shapiro test
# 2. Sphericity: The variance of the differences between all combinations of related groups must be equal -
#               this is automatically checked during the computation of the ANOVA test in the function “anova_test”
# 3. Repeated measurements are taken from the same subjects.

# If the all three conditions are satisfied, we perform ANOVA parametric test
#install.packages("rstatix")
#library(rstatix)
help("anova_test")

rm(list=ls())

########################################################################

# The company wants to determine if there are significant changes in waiting time over the updates.
# Test this hypothesis at 0.05 level of significance and perform a post-hoc analysis if required.

# H0: There are NO significant changes in waiting time in 3 types
# H1: There is a significant changes in waiting time in 3 types

# Reading the ResponseTimes.csv.
waiting_times <- read.csv("C:/Users/AnushangaKularachchi/Downloads/Q7.csv")

# Extracting response times of the application 
# 1. before the first update, 2. after the first update, 3. after the second update, and 4. after the third update. 

waiting_times1 <- waiting_times$Walk.in
waiting_times2 <- waiting_times$App
waiting_times3 <- waiting_times$Call.ahead

# Are the sampled populations independent?
# No, the response time measurements from the same set of users are taken at four different time points.
# (repeated measurements from the same users)

## 1. Checking for normality using Shapiro-Wilk normality test 
# H0: the response time data (in update i) follow from a normal distribution.
shapiro.test(waiting_times1) 
shapiro.test(waiting_times2)
shapiro.test(waiting_times3)


# Perform one-way repeated measures ANOVA 
# -using anova_test() avaialble in rstatix package

# install.packages("rstatix")
library(rstatix)
help("anova_test")

# To use anova_test(), we have to convert UserID and time into factor variables
ID_period_responsetime <- waiting_times %>%
  gather(key = "period", value = "responsetime", Walk.in, App, Call.ahead) %>%

head(ID_period_responsetime, 3)

# H0: There are NO significant changes in application speed over the updates
# H1: There is a significant change in application speed over the updates

res.aov <- anova_test(data = ID_period_responsetime, 
                          wid = UserID,                   # individuals/subjects identifier
                          within = period,                # within-subjects factor variables
                          dv = responsetime)              # dependent variable 
res.aov

# 2. Check for spehricity
# H0: The variance of the differences between all combinations of related groups are equal
# H1: The variance of the differences between all combinations of related groups are not equal

# Results for Spehricity availalble under the results obtained from anova_test():
# Spehricity condition does not satisfy as the p-value of spehericity (6.59e-21) is less than 0.05.

# Therefore, we cannot use oneway repeated ANOVA to test whether there are significant changes in application speed over the updates
# Hence, we do not make conclusions from the ANOVA results we obtained from anova_test().

# Note: we do not have to perform the post-hoc analysis which test pairwise comparisons. 

## A function to perform the post-hoc analysis is 
# pairwise_t_test(formula, paired = TRUE, p.adjust.method = "bonferroni")

# Using this function to this application (Please note the post-hoc test is not recommended to perform 
# for this problem as we couldn't perform the oneway repeated ANOVA)
pwc <- ID_period_responsetime %>%
  pairwise_t_test(responsetime ~ period, paired = TRUE, p.adjust.method = "bonferroni")
pwc