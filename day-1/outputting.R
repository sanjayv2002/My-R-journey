# Saving the outputs
# Sanjay kumar V
# sanjay.murthy,29@gmail.com
# 2023-02-17

# Read in data
all_gapminder <- read.csv(file = "data/gapminder-FiveYearData.csv",
                          stringsAsFactors = TRUE)

# Subset data
gapminder <- all_gapminder[all_gapminder$year == 2002, ]

# Make new vector of Log GDP
gapminder$Log10GDP <- log10(gapminder$gdpPercap)

# Store values to use for some plotting parameters
symbol <- 18
sym_size <- 1.2
continents <- levels(gapminder$continent)
continent_colors <- c("red", "orange", "forestgreen", "darkblue", "violet")

# Create new vector for colors
gapminder$colors <- NA
# Loop over continents and assign colors
for (i in 1:length(continents)) {
  gapminder$colors[gapminder$continent == continents[i]] <- continent_colors[i]
}

# Open PDF device
pdf(file = "output/Life_expectancy_graph.pdf", useDingbats = FALSE)

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

# Close PDF device
dev.off()
