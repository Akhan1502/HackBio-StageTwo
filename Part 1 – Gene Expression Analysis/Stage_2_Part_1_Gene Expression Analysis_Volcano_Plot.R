install.packages("ggplot2")   # run once if needed
library(ggplot2)

deg_data <- read.csv("D:/Aqsa/Internships/HackBIO/hbr_uhr_deg_chr22_with_significance.csv")

# Check structure
str(deg_data)

# Convert padj to numeric (if needed)
deg_data$padj <- as.numeric(deg_data$PAdj)

# Create -log10(padj)
deg_data$negLog10Padj <- -log10(deg_data$PAdj)

# Create category
deg_data$category <- "ns"
deg_data$category[deg_data$log2FoldChange >= 1 & deg_data$padj < 0.05] <- "up"
deg_data$category[deg_data$log2FoldChange <= -1 & deg_data$padj < 0.05] <- "down"

# Plot
ggplot(deg_data, aes(x = log2FoldChange, y = negLog10Padj, color = category)) +
  geom_point(alpha = 0.6) +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
  scale_color_manual(values = c("up" = "green", "down" = "orange", "ns" = "grey")) +
  theme_minimal()
