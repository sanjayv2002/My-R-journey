# Test relationship between life expectancy and GDP
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-02-18

all_gapminder <- read.csv(file = "day-2/data/gapminder-FiveYearData.csv",
                          stringsAsFactors = TRUE)

head(all_gapminder)
summary(all_gapminder)

# Subset 2007 data
gapminder <- all_gapminder[all_gapminder$year == 2007,]

# Plot to look at data
plot(x = gapminder$gdpPercap, y = gapminder$lifeExp)

# Create Log-transformed GDP
gapminder$LogGDP <- log10(gapminder$gdpPercap)

# Plot new variable
plot(x = gapminder$logGDP,
     y = gapminder$lifeExp,
     xlab = "log10(GDP)",
     ylab = "Life Expectancy")

# Run Linear Model
lifeExp.v.gdp <- lm(formula = lifeExp ~ LogGDP, data = gapminder)

# Save results to file
sink(file = "day-2/output/lifeExp-gdp-regression.txt")
summary(lifeExp.v.gdp)
sink()

# Challenge 3
gapminder2 <- all_gapminder[all_gapminder$year == 1982,]

plot(x = gapminder2$gdpPercap, y = gapminder2$lifeExp)

gapminder2$LogGDP <- log2(gapminder2$gdpPercap)

plot(x =gapminder2$LogGDP,
     y = gapminder2$lifeExp,
     xlab = "Log2(GDP)",
     ylab = "Life Expectancy")

lifeExp2.v.gdp <- lm(formula = lifeExp ~ LogGDP, data = gapminder2)

sink(file = "day-2/output/lifeExp-gdp-log2-regression.txt")
summary(lifeExp2.v.gdp)
sink()
