# Read dataset
bc <- read.csv("D:/Aqsa/Internships/HackBIO/data-3.csv")

# Colors for diagnosis
bc_cols <- c("B" = "orange", "M" = "blue")

# Scatter plot
plot(bc$smoothness_mean, bc$compactness_mean,
     col = bc_cols[bc$diagnosis],
     pch = 19,
     xlab = "smoothness_mean",
     ylab = "compactness_mean",
     main = "Smoothness Mean vs Compactness Mean")

# Gridlines
grid()

# Legend
legend("topleft",
       legend = c("M", "B"),
       col = c("blue", "orange"),
       pch = 19,
       title = "diagnosis")