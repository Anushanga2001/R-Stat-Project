########################################################################
#                 One way repeated ANOVA - Non-parametric test

# We compare whether there is a difference between three or more groups 
# using ANOVA technique

# We use one way repeated ANOVA - parametric test under the following conditions: 
# 1. Normality – using Shapiro test
# 2. Sphericity: The variance of the differences between all combinations of related groups must be equal -
#               this is automatically checked during the computation of the ANOVA test in the function “anova_test”
# 3. Repeated measurements are taken from the same subjects.

# If the all three conditions are satisfied, we perform ANOVA parametric test
#install.packages("rstatix")
#library(rstatix)
help("anova_test")

# If condition 1 or 2 is not satisfied, we use the Friedman test:
help("friedman.test")

rm(list=ls())


########################################################################

# Application 1: 
# A food processing company is testing three different cooling methods 
# (Method A, Method B, Method C) on production batches.
# They want to know if the methods result in different cooling times (minutes).
# Each of 10 batches is tested under all three methods.

# Cooling times (minutes) for 10 batches under 3 different methods
# Batch    1   2   3   4   5   6   7   8   9   10
MethodA = c(12,10, 9,11, 8,13, 7,14, 6,15)
MethodB = c(15,14,13,16,12,17,11,18,10,19)
MethodC = c(8, 7, 6, 12, 5, 20, 4, 25, 3, 30)
times <- cbind(MethodA, MethodB, MethodC)

# Hypotheses:
# H0: There is no significant difference in cooling times between the three methods
# H1: There is a significant difference in cooling times between the three methods

# Checking the normality assumption:
shapiro.test(MethodA)
shapiro.test(MethodB)
shapiro.test(MethodC)
# Cooling times of Method C are not normally distributed. Therefore, we cannot 
# use the parametric test and have to use the Friedman test.


# Friedman test
friedman.test(times)

# Posthoc test (pairwise Wilcoxon signed-rank test with Bonferroni correction)
pairwise.wilcox.test(as.vector(times),
                     rep(1:3, each=10),
                     paired = TRUE,
                     p.adjust.method = "bonferroni")

########################################################################

# Application 2: 
# A retail company wants to evaluate customer satisfaction ratings for 
# three different store layouts (Layout A, Layout B, Layout C).
# The same 8 customers visited each store layout and gave satisfaction scores 
# on a 1–5 Likert scale.

# Satisfaction scores of 8 customers:
# Customer  1 2 3 4 5 6 7 8
LayoutA = c(4,3,5,4,3,4,5,4)
LayoutB = c(3,2,3,2,2,3,4,2)
LayoutC = c(5,4,4,3,4,5,5,3)

scores <- cbind(LayoutA, LayoutB, LayoutC)

# H_0: There is no significant difference between the customer satisfaction ratings of three different store layouts
# H_1: There is a significant difference between the customer satisfaction ratings of three different store layouts

# Which statistical technique is suitable to perform this hypothesis?

friedman.test(scores)
# Conclusion? 

# Posthoc test
pairwise.wilcox.test(as.vector(scores), 
                     rep(1:3, each=8), 
                     paired = TRUE, p.adjust.method = "bonferroni")