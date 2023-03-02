# Plot iris trait measurements
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-03-01

# loading the tidyverse package
library("tidyverse")
library("ggplot2")
rm(list = ls())

# Load the tidyverse packages
library("tidyverse")

# Create data of summary statistics
iris.means <- iris %>%
  pivot_longer(cols = -Species,
               names_to = "trait",
               values_to = "measurement") %>%
  group_by(Species, trait) %>%
  summarize(trait.mean = mean(measurement),
            trait.se = sd(measurement)/sqrt(n()))

# Update trait names, replacing period with space
iris.means$trait <- gsub(pattern = ".",
                         replacement = " ",
                         x = iris.means$trait,
                         fixed = TRUE)

# Plot each trait separately
ggplot(data = iris.means, mapping = aes(x = Species, y = trait.mean)) +
  geom_point() +
  geom_errorbar(mapping = aes(ymin = trait.mean - trait.se,
                              ymax = trait.mean + trait.se),
                width = 0.3) +
  ylab(label = "Trait mean values") +
  facet_wrap(~ trait, scales = "free_y")

