# Cluster and heatmap on otter data
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-02-21

# Load Dependencies
library("ggplot2")
library("ggdendro")
library("tidyr")
library("grid")

# Read in data 
otter <- read.csv(file = "day-8/data/otter-mandible-data.csv",
                  stringsAsFactors = TRUE)


# taking two species
two_species <-c("A. cinerea", "L. canadensis")

# filtering database
otter <- otter[otter$species %in% two_species,]


# scaling data
otter_scaled <- otter
otter_scaled[, c(4:9)] <- scale(otter_scaled[, 4:9])

# Run clustering 
otter_matrix <- as.matrix(otter_scaled[, -c(1:3)])
rownames(otter_matrix) <- otter_scaled$accession
otter_dendro <- as.dendrogram(hclust(d = dist(x = otter_matrix)))

# Create dendro
dendro_plot <- ggdendrogram(data = otter_dendro, rotate = TRUE)

dendro_plot <- dendro_plot + theme(axis.text.y = element_text(size = 6))


# Creating Heatmap

# Data wrangling
otter_long <- pivot_longer(data = otter_scaled,
                           cols = -c(species, museum, accession),
                           names_to = "measurement",
                           values_to = "value")

# Extract the order of the tips in the dendrogram
otter_order <- order.dendrogram(otter_dendro)

# Order the levels according to their position in the cluster
otter_long$accession <- factor(x = otter_long$accession,
                               levels = otter_scaled$accession[otter_order],
                               ordered = TRUE)

# Create heatmap plot
heatmap_plot <- ggplot(data = otter_long, aes(x = measurement, y = accession)) +
  geom_tile(aes(fill = value))+
  scale_fill_gradient2() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "top")

# combining them both
grid.newpage()
print(heatmap_plot, 
      vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
print(dendro_plot, 
      vp = viewport(x = 0.90, y = 0.43, width = 0.2, height = 0.92))
