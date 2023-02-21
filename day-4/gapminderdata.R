# plot gapminder data
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-02-21


# install.packages("ggplot2")
library("ggplot2")

# Load data
gapminder <- read.csv(file = "day-4/data/gapminder.csv",
                      stringsAsFactors = TRUE)

# Creating plot object
lifeExp.plot <- ggplot(data = gapminder,
                       mapping = aes(x = gdpPercap, y = lifeExp, color = continent)) +
                  geom_point(alpha = 0.5) + scale_x_log10() + scale_color_manual(values = c("red", "orange", "forestgreen", "darkblue", "violet"))+
                  xlab("GDP per capita") + ylab("Life Expectancy")
# Draw plot
print(lifeExp.plot)

# saving plot as png
ggsave(filename = "day-4/output/gdp-lifeExp-plot.png", plot = lifeExp.plot)

# saving plot as tiff
ggsave(filename = "day-4/output/gdp-lifeExp-plot.tiff", plot = lifeExp.plot)
