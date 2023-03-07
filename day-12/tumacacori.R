# Plotting Tumacacori vegetation
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-03-07
# Libraries required for data wrangling and plotting
library(dplyr)
library(ggplot2)

plant_data <- read.csv(file = "day-12/data/tumacacori-vegetation.csv",
                       stringsAsFactors = TRUE)

# Do not want rows with missing data
plant_data <- na.omit(plant_data)

# Only focus on four families
families_keep <- c("Fabaceae", "Poaceae", "Brassicaceae", "Amaranthaceae")

# Create new data with only four families
subset_data <- plant_data[plant_data$Family %in% families_keep, ]

# Re-level data in the Family column
subset_data$Family <- factor(subset_data$Family)

# Calculate summary statistics for all species in subsetted data
cover_mean <- mean(subset_data$Percent_Cover)
cover_sd <- sd(subset_data$Percent_Cover)

# Calculate mean and standard deviation for mesquite alone
mesquite_mean <- mean(subset_data$Percent_Cover[subset_data$Common_Name == "velvet mesquite"])
mesquite_sd <- sd(subset_data$Percent_Cover[subset_data$Common_Name == "velvet mesquite"])

# Calculate mean and standard deviation for each species separately
summary_stats <- subset_data %>%
  # make a separate group of data for each Common_Name
  group_by(Common_Name) %>%
  # calculate the mean and standard deviation of Percent_Cover and store them 
  #    in columns called "mean_cover" and "sd_cover" respectively
  summarize(mean_cover = mean(Percent_Cover),
            sd_cover = sd(Percent_Cover))

# Calculate familiy-level total percent cover for each plot separately
family_data <- subset_data %>%
  group_by(Plot_Code, Family, Community) %>%
  summarize(Family_Percent_Cover = sum(Percent_Cover))

# Too few observations in two communities, so remove them
family_data <- family_data[!(family_data$Community %in% c("Wooded Herbaceous", "Woodland")), ]

# Re-order levels of Community so they plot in desired order
family_data$Community <- factor(family_data$Community,
                                levels = c("Shrubland", "Wooded Shrubland", "Forest"))

# Boxplot of family-level data
cover_plot <- ggplot(data = family_data, 
                     mapping = aes(x = Community, y = Family_Percent_Cover,
                                   fill = Family)) +
  geom_boxplot()
print(cover_plot)

ggsave(plot = cover_plot, filename = "day-12/output/family-cover-plot.pdf")

