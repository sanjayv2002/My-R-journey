# Applying log transformation
# Sanjay kumar V
# sanjay.murthy,29@gmail.com
# 2023-02-17


# fetching the dataset
all_gapminder <- read.csv(file = 'data/gapminder-FiveYearData.csv',
                     stringsAsFactors = TRUE)

collect <- all_gapminder[all_gapminder$year == 2002,]

collect$Log10GDP <- log10(collect$gdpPercap)

plot(x = collect$Log10GDP,
     y = collect$lifeExp,
     main = "Life expectancy V, GDP",
     xlab = "Log(GDP Per Capita",
     ylab = "Life expectancy (years)")

