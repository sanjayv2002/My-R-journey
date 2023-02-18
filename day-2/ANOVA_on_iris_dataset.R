# ANOVA on iris data set
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-02-18

# Run ANOVA on petal Length
petal.length.aov <- aov(formula = Petal.Length ~ Species, data = iris)

# Save results to file
sink(file = "day-2/output/petal-length-anova.txt")
summary(object = petal.length.aov)
sink()

# Challenge 2
sepal.length.aov <- aov(formula = Sepal.Length ~ Species, data = iris)
summary(object = sepal.length.aov)

