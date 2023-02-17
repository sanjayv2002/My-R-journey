# Automation
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-02-17

# Read in data
all_gapminder <- read.csv(file = "./data/gapminder-FiveYearData.csv",
                          stringsAsFactors = TRUE)

# Hold off on subsetting the data
# Make new vector of Log GDP
all_gapminder$Log10GDP <- log10(all_gapminder$gdpPercap)

# Store values to use for some plotting parameters
symbol <- 18
sym_size <- 1.2
continents <- levels(all_gapminder$continent)
continent_colors <- c("red", "orange", "forestgreen", "darkblue", "violet")

# Create new vector for colors
all_gapminder$colors <- NA
# Loop over continents and assign colors
for (i in 1:length(continents)) {
  all_gapminder$colors[all_gapminder$continent == continents[i]] <- continent_colors[i]
}

# Find the unique values in the all_gapminder$year vector
years <- unique(all_gapminder$year)

# Now loop over each of the different years to create the PDFs.
for (curr_year in years) {
  # Subset data
  gapminder_one_year <- all_gapminder[all_gapminder$year == curr_year, ]
  
  # Open PDF device
  filename <- paste0("output/Life_exp_", curr_year, "_graph.pdf")
  pdf(file = filename, useDingbats = FALSE)
  # Create main plot
  plot(x = gapminder_one_year$Log10GDP, 
       y = gapminder_one_year$lifeExp, 
       main = "Life expectancy v. GDP", 
       sub = curr_year, 
       xlab = "Log(GDP Per capita)", 
       ylab = "Life expectancy (years)",
       col = gapminder_one_year$colors,
       pch = symbol,
       cex = sym_size,
       lwd = 1.5)
  
  # Add a legend
  legend("topleft", 
         legend = continents, 
         col = continent_colors,
         pch = symbol)
  
  # Add a regression line
  lifeExp_lm <- lm(gapminder_one_year$lifeExp ~ gapminder_one_year$Log10GDP)
  abline(reg = lifeExp_lm, lty = 2, lwd = 2)
  
  # Close PDF device
  dev.off()
}

