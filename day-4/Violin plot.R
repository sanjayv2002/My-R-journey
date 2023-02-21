# Violing plot of GDP data
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-02-21

# clearing all the variable's data from the memory
rm(list = ls())

# importing ggplot2
library("ggplot2")

#Load data
gapminder <- read.csv(file = "day-4/data/gapminder.csv",
                      stringsAsFactors = TRUE)

# creating a violing plot object
gdp.plot <- ggplot(data =gapminder[gapminder$continent != "Oceania", ], 
                   mapping = aes(x = continent, y = gdpPercap, fill = continent)) +
  geom_violin() +
  scale_y_log10() +
  ylab("GDP per capita") +
  theme_bw() +
  facet_wrap(~ year) +
  scale_fill_manual(values = c("red", "orange", "forestgreen", "darkblue", "violet")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())
# Printing the plot
print(gdp.plot)
