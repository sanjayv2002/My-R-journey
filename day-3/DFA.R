# DFA - Discriminant Function Analysis
# Sanjay Kumar V
# sanjay.murthy.29@gmail.com
# 2023-02-20

# Drop rows with NA
otter <- na.omit(otter)

# Renumber rows
rownames(otter) <- NULL

# Load MASS library
library("MASS")

# Run DFA
lda.fit <- lda(formula = species ~ m1 + m2 + m3 + m4 + m5 + m6, 
               data = otter, 
               prior = c(1,1,1,1)/4, 
               CV = TRUE)

# Setup vectors for labeling x-axis with a priori assignments. 
# Besides the first element, which is 0, the remaining values in the vector we 
# created are the cutoff points for each species. The second element takes the 
# value of the position of the last occurrence of the first species, the third 
# element takes the value of the position of the last occurrence of the second 
# species, and so on.
tick.pos <- c(0, 
              nrow(otter) - match(levels(otter$species)[1], rev(otter$species)) + 1, 
              nrow(otter) - match(levels(otter$species)[2], rev(otter$species)) + 1, 
              nrow(otter) - match(levels(otter$species)[3], rev(otter$species)) + 1, 
              nrow(otter) - match(levels(otter$species)[4], rev(otter$species)) + 1)

label.pos <- c(floor((tick.pos[2] - tick.pos[1]) / 2),
               floor((tick.pos[3] - tick.pos[2]) / 2 + tick.pos[2]),
               floor((tick.pos[4] - tick.pos[3]) / 2 + tick.pos[3]),
               floor((tick.pos[5] - tick.pos[4]) / 2 + tick.pos[4]))

legend.cols <- c("black", "green4", "cyan3", "red3") 

# Store graphics defaults
mar.default <- c(5, 4, 4, 2) + 1 # from par documentation
par(mar = mar.default + c(2, 0, 0, 0)) # add space to bottom margin

barplot(posteriors,
        ylab = "Posterior Probability",
        space = 0,
        border = NA,
        xaxt = 'n',
        xlim = c(0, ncol(posteriors) + 45),
        col = legend.cols,
        legend.text = levels(otter$species),
        args.legend = list(cex = 0.7, x = ncol(posteriors) + 55, y = 0.6, xpd = TRUE))
# Add x-axis labels
axis(side = 1, at = tick.pos, labels = FALSE) # tick marks
axis(side = 1, at = label.pos, labels = levels(otter$species), tick = FALSE, par(las = 2, cex = 0.8))

# Restore graphics defaults
par(mar = mar.default)