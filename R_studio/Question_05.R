# Hypothesis tesing:
help("wilcox.test")

Before <- c(45, 50, 55, 60, 52, 48, 58, 62, 54, 56);
After <- c(50, 55, 57, 65, 54, 52, 60, 66, 58, 59);

# H0: Revision class significantly improved scores at the 5% significance level
# H1: Revision class significantly not improved scores

wilcox.test(Before, After, alternative = "two.sided", paired=TRUE)

# Since p-value < 0.05, we reject H0.

# Conclusion:
# We can conclude that the Revision class significantly not improved scores
# at 0.05 significance level. 