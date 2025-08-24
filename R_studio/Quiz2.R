########################################################################
#   One way ANOVA between subjects-  parametric test

# We compare whether there is a difference between three or more groups  
# using Analysis Of variance (ANOVA) technique

# H0: There is NO significant difference between the group means
# H1: There is a significant difference between the group means

#To use ANOVA, we check the following conditions: 
# 1. The populations follow the normal distribution – using Shapiro test
# 2. The populations have equal standard deviations (σ) – using Levene’s test
# 3. The populations are independent

# If the all three conditions are satisfied, we perform ANOVA parametric test
help("aov")

rm(list=ls())

########################################################################


# Application 1

# A manufacturing company wants to compare the mean production speeds of three different machines
# (Machine X, Machine Y, and Machine Z) to identify any significant differences.

# H0: There is NO significant difference between the mean production speeds of three machines
#     mu_x = mu_Y = mu_Z
# H1: There is a significant difference between the mean production speeds of three machines

# Production speed data for three machines are stored in ProductionSpeed.csv.
Pro_Speed <- read.csv("C:/Users/AnushangaKularachchi/Downloads/ProductionSpeed.csv")

# Extracting production speeds of machines X, Y, and Z
M_X <- Pro_Speed$machine_X
M_Y <- Pro_Speed$machine_Y
M_Z <- Pro_Speed$machine_Z


## 1. Checking for normality using Shapiro-Wilk normality test 
# H0: the production speed of the machine comes from a normal distribution.

# H0: the production speed of machine X has a normal distribution.
# Ha: the production speed of machine X does NOT have a normal distribution.
shapiro.test(M_X)
# since p-value = 0.2974 > 0.05 = significance level, we do not reject H0
# We do not have enough evidence to conclude the production speed of machine X 
# does NOT have a normal distribution at 0.05 significance level.
# That means, we conclude the production speed of machine X has a normal distribution.


## 1. Checking for normality using Shapiro-Wilk normality test 
shapiro.test(M_X)
shapiro.test(M_Y)
shapiro.test(M_Z)
# Conclusion: production speeds of three machines follow normal distributions


## 2. Testing for equal variance using leveneTest()
#install.packages("car")
library(car)
help("leveneTest")

# Combining production speeds of machines X, Y, and Z
production_speed <- c(M_X, M_Y, M_Z)
machine <- rep(c("X", "Y", "Z"), each = 40)

data <- data.frame(production_speed, machine)

# leveneTest
# H0: The production speeds of three machines have equal variance
leveneTest(production_speed ~ machine, data)
# Note: Pr(>F) in the output corresponds to the p-value of the test.
# Conclusion: The production speeds of three machines have equal variance


## 3. The production speeds of three machines are independent.


###################################################
## Perform one-way ANOVA
# H0: There is NO significant difference between the mean production speeds of three machines
# H1: There is a significant difference between the mean production speeds of three machines

# Since all three conditions are satisfied, we perform ANOVA
help("aov")

res.aov <- aov(production_speed ~ machine, data = data)
summary(res.aov)

# Note: Pr(>F) in the output corresponds to the p-value of the test.

# Decision: since p-value < 0.05, we reject H0.

# Conclusion: 
# We can conclude that there is a significant difference between the mean production speeds of 
# three machines at 0.05 level of significance.


###################################################

# Post hoc test: 
# Between which groups do the production speeds differ?
# We use Tukey Post hoc test.
help(TukeyHSD)

tukey.test <- TukeyHSD(res.aov)

tukey.test

# For visualization:
plot(TukeyHSD(res.aov, conf.level=.95), las = 2)

# Result:
# Since the pairwise confidence intervals of Y-X and Z-Y do not include 0, 
# the production speeds of those pairs are significantly different at 0.05 level of significance.

# Since the confidence interval of Z-X includes 0, the production speeds machine Z and X 
# are significantly different at 0.05 level of significance.

