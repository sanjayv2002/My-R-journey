# first R script
# Sanjay kumar V
# sanjay.murthy.29@gmail.com
# 2023-2-17

# Reading the data
all_gapminder <- read.csv(file = "data/gapminder-FiveYearData.csv",
                          stringsAsFactors = TRUE)

# Sub-setting data for the year 2002
gapminder <- all_gapminder[all_gapminder$year == 2002,]

# plotting the values
plot(x = gapminder$gdpPercap,
     y = gapminder$lifeExp,
     main = "Life expectancy V. GDP",
     xlab = "GDP per capita",
     ylab = "Life expectance (years)")

