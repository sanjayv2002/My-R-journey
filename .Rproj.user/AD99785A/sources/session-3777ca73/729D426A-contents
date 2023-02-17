# optimizing with loops
# Sanjay kumar V
# sanjay.murthy,29@gmail.com
# 2023-02-17

# reading data
all_gapminder <- read.csv(file = "data/gapminder-FiveYearData.csv",
                          stringsAsFactors = TRUE)

#Subsetting data
gapminder <- all_gapminder[all_gapminder$year == 2002,]

# making a new vector for Logged transformations
gapminder$Log10GDP <- log10(gapminder$gdpPercap)

# Store values to use for some plotting parameters
symbol <- 18
sym_size <- 1.2
continents <- levels(gapminder$continent)
continent_colors <- c("red", "orange", "forestgreen", "darkblue", "violet")

# establishing the empty column to store colors
gapminder$colors <- NA
for (i in 1:length(continents)) {
  gapminder$colors[gapminder$continent == continents[i]] <- continent_colors[i]
}


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

# Add a legend
legend("topleft", 
       legend = continents, 
       col = continent_colors,
       pch = symbol)

# Add a regression line
lifeExp_lm <- lm(gapminder$lifeExp ~ gapminder$Log10GDP)
abline(reg = lifeExp_lm, lty = 2, lwd = 2)

