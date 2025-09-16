# H0: There is NO significant difference between fertilizers A, B and C for weekly growth (in cm) of 40 plants
# H1: There is significant difference between fertilizers A, B and C for weekly growth (in cm) of 40 plants

Growth_Speed <- read.csv("C:/Users/AnushangaKularachchi/Downloads/PlantGrowth.csv")

# Extracting Growth speeds of Fertilizer A, B and C
G_A <- Growth_Speed$A
G_B <- Growth_Speed$B
G_C <- Growth_Speed$C

## 1. Checking for normality using Shapiro-Wilk normality test 
# H0: the Growth speeds of Fertilizer A, B and C comes from a normal distribution.

# H0: the Growth speeds of Fertilizer A has a normal distribution.
# Ha: the Growth speeds of Fertilizer A has a not normal distribution.
shapiro.test(G_A)

## 1. Checking for normality using Shapiro-Wilk normality test 
shapiro.test(G_A)
shapiro.test(G_B)
shapiro.test(G_C)

# Conclusion: Growth speeds of three Fertilizer follow normal distributions


## 2. Testing for equal variance using leveneTest()
#install.packages("car")
library(car)
help("leveneTest")

# Combining growth speeds of fertilizer A, B and C
Growth_speed <- c(G_A, G_B, G_C)
Growth <- rep(c("A", "B", "C"), each = 40)

data <- data.frame(Growth_speed, Growth)

# leveneTest

# H0: The Growth speeds of three fertilizer have equal variance
leveneTest(Growth_speed ~ Growth, data)
# Note: Pr(>F) in the output corresponds to the p-value of the test.
# Conclusion: The Growth speeds of three fertilizer have equal variance


## 3. The growth speeds of three fertilizer are independent.

###################################################

## Perform one-way ANOVA
# H0: There is NO significant difference between the mean growth speeds of three fertilizer
# H1: There is a significant difference between the mean growth speeds of three fertilizer

# Since all three conditions are satisfied, we perform ANOVA
help("aov")

res.aov <- aov(Growth_speed ~ Growth, data = data)
summary(res.aov)

# Note: Pr(>F) in the output corresponds to the p-value of the test.

# Decision: since p-value < 0.05, we reject H0.

# Conclusion: 
# We can conclude that there is a significant difference between the mean growth speeds of 
# three fertilizer at 0.05 level of significance.


###################################################

# Post hoc test: 
# Between which groups do the growth speeds differ?
# We use Tukey Post hoc test.
help(TukeyHSD)

tukey.test <- TukeyHSD(res.aov)

tukey.test

# For visualization:
plot(TukeyHSD(res.aov, conf.level=.95), las = 2)

# Result:
# Since the pairwise confidence intervals of A-B, B-C and A-C do not include 0, 
# the growth speeds of those pairs are significantly different at 0.05 level of significance.