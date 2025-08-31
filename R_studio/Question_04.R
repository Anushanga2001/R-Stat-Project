# Hypothesis tesing:
help("wilcox.test")

# There is a significant difference in social media usage 
# between the two groups at the 5% significance level

# Social Media usage of two groups, Group A and Group B are given below:

Group_A <- c(3, 4, 2, 5, 6, 4)
Group_B <- c(1, 2, 2, 3, 1, 2)

# Check for normality using histograms/boxplots

# Histograms
par(mfrow=c(2,1))
hist(Group_A)
hist(Group_B)
# Boxplot
par(mfrow=c(1,1))
boxplot(Group_A, Group_B)


# Test for normality using Shapiro test
# H0; data comes from a normal population
shapiro.test(Group_A)
shapiro.test(Group_B)
# Result: The data (Group_A/Group_B usage) come from a normal population

help("wilcox.test")

# H0: There is no significant difference in social media usage between the two groups
# H1: There is a significant difference in social media usage between the two groups
wilcox.test(Group_A,Group_B, alternative = "two.sided",correct = FALSE) 

# Note: We got a warning as  cannot compute exact p-value with ties
# Let's remove 63.4 from orange bulbs
Group_A_1 <- c(3, 4, 2, 5, 6)
Group_B_1 <- c(1)

wilcox.test(Group_A_1,Group_B_1, alternative = "two.sided",correct = FALSE) 
# No warnings as there are no ties
# Note: We can perform hypothesis testing with ties and do not have to remove ties 

# At 0.05 level, we do not have enough evidence to conclude that 
# There is a significant difference in social media usage between the two groups