# install.packages("pheatmap")
library(pheatmap)

# Read dataset
bc <- read.csv("D:/Aqsa/Internships/HackBIO/data-3.csv")

# Select 6 key features
features <- bc[, c("radius_mean", "texture_mean",
                   "perimeter_mean", "area_mean",
                   "smoothness_mean", "compactness_mean")]

# Correlation matrix
cor_mat <- cor(features, use = "complete.obs")

# Heatmap with numbers
pheatmap(cor_mat,
         display_numbers = TRUE,
         number_format = "%.1f",
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         border_color = "black",
         color = colorRampPalette(c("white", "lightblue", "blue"))(100),
         main = "Correlation Heatmap (6 Breast Cancer Features)")