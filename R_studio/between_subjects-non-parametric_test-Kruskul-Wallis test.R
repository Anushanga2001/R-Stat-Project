########################################################################
# One way ANOVA between subjects - non-parametric test - Kruskul-Wallis test

# We compare whether there is a difference between three or more groups 
# using ANalysis Of VAriance (ANOVA) technique
# H0: There is NO significant difference between the groups
# H1: There is a significant difference between the groups

#To use ANOVA, we check the following conditions: 
# 1. The populations follow the normal distribution – using Shapiro test
# 2. The populations have equal standard deviations (σ) – using Levene’s test
# 3. The populations are independent

# If the all three conditions are satisfied, we perform ANOVA parametric test
help("aov")

# If normality or equal variance conditions (1 or 2) do not satisfied for independent populations, 
# we perform ANOVA non-parametric test: Kruskul-Wallis test
help("kruskal.test")

# Note:
# When independent populations are normally distributed and the population variances are equal, 
# we can use either the parametric or non-parametric test. 
# However, the ANOVA parametric test is preferred as it is more powerful.

rm(list=ls())

########################################################################

# Application 1: E-commerce Industry - Website Load Time

rm(list=ls())

# A large e-commerce company is interested in comparing the load times 
# of its website for different web browsers. 
# They have collected data on website load times for three different web browsers: 
# Google Chrome, Mozilla Firefox, and Microsoft Edge. The data is stored in LoadTimes.csv.

# Test whether there is a significant difference in load times among these browsers
# at 0.05 level of significance.

# H0: There is NO significant difference in load times among the browsers Google Chrome, Mozilla Firefox, and Microsoft Edge
# H1: There is a significant difference in load times among the browsers Google Chrome, Mozilla Firefox, and Microsoft Edge


# Reading the LoadTimes.csv.
LoadTimes_Data <- read.csv("C:/Users/AnushangaKularachchi/Downloads/LoadTimes(in).csv")

# Extracting load times of three browsers
LT_Chrome <- LoadTimes_Data$Chrome
LT_Firefox <- LoadTimes_Data$Firefox
LT_Edge <- LoadTimes_Data$Edge


## 1. Checking for normality using Shapiro-Wilk normality test 
# H0: the load time of a browser comes from a normal distribution.
shapiro.test(LT_Chrome) 
shapiro.test(LT_Firefox)
# Conclusion: the load time of chrome follows a normal distribution 
# but the load time of firefox does not follow a normal distribution

# Therefore, we cannot perform ANOVA parametric test to check whether 
# there is a significant difference in load times among these browsers.


# Note that the load times of three browsers are independent.
# Therefore, we perform Kruskal-Wallis test to test
# H0: There is NO significant difference in load times among the browsers Google Chrome, Mozilla Firefox, and Microsoft Edge
# H1: There is a significant difference in load times among the browsers Google Chrome, Mozilla Firefox, and Microsoft Edge

Loadtimes <- c(LT_Chrome, LT_Firefox, LT_Edge)
Browser <- rep(c("Chrome", "Firefox", "Edge"), each = 50)
Loadtimes_Browser <- data.frame(Loadtimes, Browser)

# Perform Kruskal-Wallis test
kruskal.test(Loadtimes ~ Browser, data = Loadtimes_Browser)

# Decision: since p-value < 0.05, we reject H0.

# Conclusion: 
# We can conclude that there is a difference in load times among the browsers 
# Google Chrome, Mozilla Firefox, and Microsoft Edge.


## Posthoc test

# Which browser(s) has(have) significantly different load times?

# We perform multiple pairwise Wilcoxon test between browsers
help("pairwise.wilcox.test")

pairwise.wilcox.test(Loadtimes, Browser, p.adjust.method = "BH")


# We get p-value < 0.05 for all the pairs.  

# Therefore, the e-commerce website has significantly different load times 
# in Chrome-Firefox, Edge-Firefox and Chrome-Edge at 0.05 significance level.


########################################################################

# Application 2: Customer Satisfaction with Different Store Brands

rm(list=ls())

# A retail company wants to determine if there are significant differences in 
# customer satisfaction levels among four different store brands 
# (Brand A, Brand B, Brand C, and Brand D). 

# Customer satisfaction scores for three store brands are given below:
brand_A <- c(4, 5, 4, 3, 4, 5, 4, 2, 3, 5, 4, 3, 3, 4, 5, 2, 3, 4, 4, 5, 4)
brand_B <- c(3, 3, 4, 3, 4, 3, 2, 4, 4)
brand_C <- c(5, 4, 5, 4, 5, 4, 5, 4, 4, 5, 5, 4, 5)
brand_D <- c(3, 2, 4, 3, 2, 3, 2, 2, 3, 1, 1)

# Test whether there is a significant difference in satisfaction levels among the brands
# at 0.05 level of significance.


customer_satisfaction <- c(brand_A, brand_B, brand_C, brand_D)
Brands <- c(rep("A", each = 21),rep("B", each = 9), rep("C", each = 13),rep("D", each = 11))
customer_satisfaction_Brand <- data.frame(customer_satisfaction, Brands)



# Perform Kruskal-Wallis test
# H0: There is NO significant difference between the customer satisfaction scores of four brands
# H1: There is a significant difference between the customer satisfaction scores of four brands

# Note: 
# We cannot use ANOVA parametric test as the customer satisfaction scores are qualitative (ordered) data.
# Therefore, the normality assumptions in the ANOVA parametric test do not work for customer satisfaction scores.

kruskal.test(customer_satisfaction ~ Brands, data = customer_satisfaction_Brand)



## Post-hoc tests:
## Which pairs of brand(s) has(have) significantly different customer satisfaction scores?


# We perform multiple pairwise Wilcoxon test between browsers
pairwise.wilcox.test(customer_satisfaction, Brands, p.adjust.method = "BH")

# We get p-value < 0.05 except for the pair A & B

# Therefore, the other than brand A & B, all the pairs of brands have significantly different 
# customer satisfaction scores at 0.05 significance level.

########################################################################

# Application 3: 

# A manufacturing company wants to compare the mean production speeds of three different machines 
# (Machine X, Machine Y, and Machine Z) to identify any significant differences.

# H0: There is NO significant difference between the mean production speeds of three machines
# H1: There is a significant difference between the mean production speeds of three machines

# Production speed data for three machines are stored in ProductionSpeed.csv.
Pro_Speed <- read.csv("C:/Users/Admin/OneDrive - University of Kelaniya/Teaching/MGTE 44373/2.1-One-way ANOVA- Parametric test/ProductionSpeed.csv")

# Extracting production speeds of machines X, Y, and Z
M_X <- Pro_Speed$machine_X
M_Y <- Pro_Speed$machine_Y
M_Z <- Pro_Speed$machine_Z


## 1. Checking for normality using Shapiro-Wilk normality test 
# H0: the production speed of the machine comes from a normal distribution.
shapiro.test(M_X)
shapiro.test(M_Y)
shapiro.test(M_Z)
# Conclusion: production speeds of three machines follow normal distributions


## 2. Testing for equal variance using leveneTest
library(car)

# Combining production speeds of machines X, Y, and Z
production_speed <- c(M_X, M_Y, M_Z)
machine <- rep(c("X", "Y", "Z"), each = 40)
data <- data.frame(production_speed, machine)

# leveneTest
# H0: The production speeds of three machines have equal variance
leveneTest(production_speed ~ machine, data)
# Conclusion: The production speeds of three machines have equal variance


## 3. The production speeds of three machines are independent.


###################################################
## Perform one-way ANOVA
# H0: There is NO significant difference between the mean production speeds of three machines
# H1: There is a significant difference between the mean production speeds of three machines


# We perform ANOVA parametric test as normality and equal variance conditions are satisfied.
res.aov <- aov(production_speed ~ machine, data = data)
summary(res.aov)
# We can conclude that there is a significant difference between the mean production speeds of 
# three machines at 0.05 level of significance.

# Alternatively, we perform ANOVA non-parametric test although normality and equal variance conditions are satisfied.
kruskal.test(production_speed ~ machine, data = data)
# We can conclude that there is a significant difference between the mean production speeds of 
# three machines at 0.05 level of significance.

## Note:
# When the independent populations are normally distributed and the population variances are equal, 
# we can use either the parametric or non-parametric test. 
# However, the ANOVA parametric test is preferred as it uses data 
# rather than the ranks of data which is used in the  Kruskal Wallis test.


