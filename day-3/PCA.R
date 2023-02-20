# PCA - Principal Component Analysis
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-02-20

# Reading Data
otter <- read.csv(file = "day-3/data/otter-mandible-data.csv",
                  stringsAsFactors = TRUE)


# Drop rows with NA
otter <- na.omit(otter)

# Renumber rows
rownames(otter) <- NULL

# Run PCA
pca.fit <- prcomp(x = otter[, -c(1:3)], scale. = TRUE)
pca.summary <- summary(pca.fit)

# Plotting results
# Pull out the unique values in the 'species' column for legend
species.names <- unique(otter$species)

# Set up a vector of colors for the *legend*
legend.cols <- c("black", "green4", "cyan3", "red3") 

# Set up a vector of colors for the actual *plot*, based on values in the 
# 'species' column and the legend colors vector. This vector has one element 
# corresponding to each row of the otter data frame. 
pt.cols <- rep(x = legend.cols[1], length = nrow(otter))
pt.cols[otter$species == species.names[2]] <- legend.cols[2]
pt.cols[otter$species == species.names[3]] <- legend.cols[3]
pt.cols[otter$species == species.names[4]] <- legend.cols[4]

# Plot the first two components
plot(x = pca.fit$x[, 1],
     y = pca.fit$x[, 2],
     xlab = "PC 1",
     ylab = "PC 2",
     pch = 19,
     col = pt.cols)
legend("bottomleft", 
       legend = species.names, 
       pch = 19, 
       col = legend.cols, 
       cex = 0.8)


