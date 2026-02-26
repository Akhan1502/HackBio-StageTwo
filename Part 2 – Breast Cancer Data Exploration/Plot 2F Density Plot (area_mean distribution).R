# Read dataset
bc <- read.csv("D:/Aqsa/Internships/HackBIO/data-3.csv")

# Density for each group
dens_M <- density(bc$area_mean[bc$diagnosis == "M"])
dens_B <- density(bc$area_mean[bc$diagnosis == "B"])

# Plot density curves
plot(dens_M,
     col = "blue",
     lwd = 2,
     xlab = "area_mean",
     ylab = "Density",
     main = "Area Mean Density (M vs B)")

lines(dens_B, col = "orange", lwd = 2)

# Optional fill (simple + nice)
polygon(dens_M, col = rgb(0, 0, 1, 0.25), border = NA)
polygon(dens_B, col = rgb(1, 0.5, 0, 0.25), border = NA)
lines(dens_M, col = "blue", lwd = 2)
lines(dens_B, col = "orange", lwd = 2)

# Legend
legend("topright",
       legend = c("M", "B"),
       col = c("blue", "orange"),
       lwd = 2,
       title = "diagnosis")