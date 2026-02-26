install.packages("pheatmap")   
library(pheatmap)

# Read normalized counts (genes in rows, samples in columns)
counts <- read.csv("D:/Aqsa/Internships/HackBIO/hbr_uhr_top_deg_normalized_counts.csv", row.names = 1)

# Convert to matrix
mat <- as.matrix(counts)

# Plot clustered heatmap with blue gradient
pheatmap(mat,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         color = colorRampPalette(c("white", "blue"))(100),
         show_rownames = TRUE,
         show_colnames = TRUE,
         main = "Top DEGs Heatmap (HBR vs UHR)"
)
