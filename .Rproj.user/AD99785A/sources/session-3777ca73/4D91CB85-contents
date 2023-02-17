# Abstracting code and adding regression line
# Sanjay kumar V
# sanjay.murthy,29@gmail.com
# 2023-02-17

# Read in data
all_gapminder <- read.csv(file = "data/gapminder-FiveYearData.csv",
                          stringsAsFactors = TRUE)

# Subset data
gapminder <- all_gapminder[all_gapminder$year == 2002,]

# Making a new vector for transformed GDP
gapminder$Log10GDP <- log10(gapminder$gdpPercap)

# Storing values for plotting parameters
symbol <- 18
sym_size <- 1.2
continents <- levels(gapminder$continent)
continent_colors <- c("red","orange","forestgreen","darkblue","violet")

# Assign the colors for each continent in a new vector
gapminder$colors <- NA
gapminder$colors[gapminder$continent == continents[1]] <-continent_colors[1]
gapminder$colors[gapminder$continent == continents[2]] <-continent_colors[2]
gapminder$colors[gapminder$continent == continents[3]] <-continent_colors[3]
gapminder$colors[gapminder$continent == continents[4]] <-continent_colors[4]
gapminder$colors[gapminder$continent == continents[5]] <-continent_colors[5]

# Create main plot
plot(x = gapminder$Log10GDP,
     y = gapminder$lifeExp,
     main = "Life expectancy v. GDP", 
     xlab = "Log(GDP Per capita)", 
     ylab = "Life expectancy (years)",
     col = gapminder$colors,
     pch = symbol,
     cex = sym_size,
     lwd = 1.5)

# adding legend
legend("topleft",
       legend = continents,
       col = continent_colors,
       pch = symbol)

# adding a regression line

lifeExp_lm <- lm(gapminder$lifeExp ~ gapminder$Log10GDP)
abline(reg = lifeExp_lm, lty = 2, lwd= 2)

