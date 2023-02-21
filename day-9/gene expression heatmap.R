# Gene expression heatmap
# Sanjay Kumar v
# sanjay.murthy.29@gmail.com
# 2023-02-21

library("tidyr")
library("ggplot2")

# Read data
exp.data <- read.csv(file = "day-9/data/GSE68849-expression.csv",
                     stringsAsFactors = TRUE)

# Transfrom data to "long" format
exp.long <- pivot_longer(data = exp.data, 
                         cols = -c(subject, treatment),
                         names_to = "gene", 
                         values_to = "expression")

# applying log transformation
exp.long$log.expression <- log(exp.long$expression)


# Visualizing the heatmap
exp.heatmap <- ggplot(data=exp.long, mapping = aes(x = subject,
                                                   y = gene,
                                                   fill = log.expression)) + geom_tile() +
  xlab(label = "Subject") + # Add a nicer x-axis title
  facet_grid(~ treatment, switch = "x", scales = "free_x", space = "free_x") + 
  theme(axis.title.y = element_blank(), # Remove the y-axis title
        axis.text.x = element_text(angle = 45, vjust = 0.5)) # Rotate the x-axis labels

exp.heatmap


# Saving the output

ggsave(filename = "day-9/output/expression-heatmap.pdf", plot = exp.heatmap)
