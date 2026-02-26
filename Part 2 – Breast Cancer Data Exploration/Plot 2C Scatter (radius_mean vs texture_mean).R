# Read dataset
bc <- read.csv("D:/Aqsa/Internships/HackBIO/data-3.csv")

# Colors for diagnosis
bc_cols <- c("B" = "orange", "M" = "blue")

# Scatter plot
plot(bc$radius_mean, bc$texture_mean,
     col = bc_cols[bc$diagnosis],
     pch = 19,
     xlab = "radius_mean",
     ylab = "texture_mean",
     main = "Radius Mean vs Texture Mean")

# Legend
legend("topright",
       legend = c("M", "B"),
       col = c("blue", "orange"),
       pch = 19,
       title = "diagnosis")