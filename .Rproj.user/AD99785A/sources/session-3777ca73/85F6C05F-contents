# Colored Plot
# Sanjay kumar V
# sanjay.murthy,29@gmail.com
# 2023-02-17


# fetching the dataset
all_gapminder <- read.csv(file = 'data/gapminder-FiveYearData.csv',
                          stringsAsFactors = TRUE)

# Sub setting the data for the year 2002
gapminder <- all_gapminder[all_gapminder$year == 2002,]

# apply log transformation and create a new vector
gapminder$Log10GDP <- log10(gapminder$gdpPercap)

# creating an empty vector for the colors
gapminder$colors <-NA

#Assigning colors based on the continent "gapminder$continent
gapminder$colors[gapminder$continent == 'Africa'] <- "red"
gapminder$colors[gapminder$continent == 'Americas'] <- "orange"
gapminder$colors[gapminder$continent == 'Asia'] <- "forestgreen"
gapminder$colors[gapminder$continent == 'Europe'] <- "darkblue"
gapminder$colors[gapminder$continent == 'Ocenia'] <- "violet"

#Create the plot
plot(x = gapminder$Log10GDP,
     y = gapminder$lifeExp,
     main = "Life expectancy V, GDP",
     xlab = "Log(GDP Per Capita",
     ylab = "Life expectancy (years)",
     col = gapminder$colors,
     pch = 18)

legend("topleft",
       legend = levels(gapminder$continent),
       col = c("red","orange","forestgreen","darkblue","violet"),
       pch = 18)

