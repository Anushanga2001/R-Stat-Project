# Hypothesis tesing:
help("wilcox.test")

Before <- c(82, 76, 90, 85, 88, 79, 92, 80);
After <- c(80, 75, 87, 83, 86, 78, 89, 79);

# H0: The exercise program significantly reduced body weight at the 5% significance level
# H1: The exercise program not significantly reduced body weight

wilcox.test(Before, After, alternative = "two.sided", paired=TRUE)

# Since p-value < 0.05, we reject H0.

# Conclusion:
# We can conclude that the exercise program not significantly reduced body weight
# at 0.05 significance level. 