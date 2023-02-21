# Heat map of mine pit microbe diversity
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-02-21

# importing libraries
library("tidyr")
library("ggplot2")

#Read data

mine.data <- read.csv(file = "day-6/data/mine-data.csv",
                      stringsAsFactors = TRUE)

# formatting for heat map

mine.long <- pivot_longer(data =mine.data,
                          cols = -c(1:3),
                          names_to = "Class",
                          values_to = "Abundance")

# Transform abundance data for better visualization
mine.long$Sqrt.abundance <- sqrt(mine.long$Abundance)

# plotting abundance

mine.heatmap <- ggplot(data = mine.long, mapping = aes(x = Sample.name, y = Class, fill = Abundance)) +
  geom_tile() + 
  xlab(label = "Depth (m)") + 
  facet_grid(~ Depth, switch = "x", scales = "free_x", space = "free_x") +
  scale_fill_gradient(name = "Sqrt(Abundance)", 
                      low = "#FFFFFF",
                      high = "#012345") +
  theme_bw() +
  theme(strip.placement = "outside", # Move depth boxes to bottom of plot
        plot.title = element_text(hjust = 0.5), # Center-justify plot title
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
  ggtitle(label = "Microbe Class Abundance") +
  scale_y_discrete(limits = rev(levels(as.factor(mine.long$Class))))

mine.heatmap

