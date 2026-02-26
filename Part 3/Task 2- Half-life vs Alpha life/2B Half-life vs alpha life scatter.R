library(readxl)

transparent_color <- function(color, percent = 50, name = NULL) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  invisible(t.col)
}

hb_pal <- c("#4e79a7", "#8cd17d", "#e15759", "#fabfd2", "#a0cbe8",
            "#59a14f", "#b07aa1", "#ff9d9a", "#f28e2b", "#f1ce63",
            "#79706e", "#d4a6c8", "#e9e9e9", "#ffbe7d", "#bab0ac",
            "#9d7660", "#d37295", "#86bcb6", "#362a39", "#cd9942")

path <- "D:/Aqsa/Internships/HackBIO/hb_stage_2.xlsx"
df_b <- read_xlsx(path, sheet = "b")

# Log2 transform
x <- log2(df_b$half_life)
y <- log2(df_b$alpha)

# Thresholds (match panel)
x_cut <- 2.5
y_cut <- -3.6

# Assign quadrants
df_b$quadrant <- ifelse(x <= x_cut & y >= y_cut, "Q1",
                        ifelse(x >  x_cut & y >= y_cut, "Q2",
                               ifelse(x <= x_cut & y <  y_cut, "Q3", "Q4")))

# Use HB palette colors
quad_cols <- c(
  Q1 = hb_pal[2],   # green-ish
  Q2 = hb_pal[3],   # red-ish
  Q3 = hb_pal[11],  # grey-ish
  Q4 = hb_pal[1]    # blue-ish
)

quad_cols_transparent <- sapply(quad_cols, transparent_color, percent = 25)

plot(x, y,
     col = quad_cols_transparent[df_b$quadrant],
     pch = 19,
     cex = 0.9,
     xlab = "log2(Half-life)",
     ylab = "log2(Alpha)",
     main = "Panel 2b: Half-life vs Alpha")

# Cutoff lines
abline(v = x_cut, lty = 2, col = "black")
abline(h = y_cut, lty = 2, col = "black")

# Optional gene labels (positioned cleanly)
text(mean(range(x[x <= x_cut])),
     mean(range(y[y >= y_cut])),
     "Ccr2", cex = 1.2, font = 2)

text(mean(range(x[x > x_cut])),
     mean(range(y[y < y_cut])),
     "Camp", cex = 1.2, font = 2)

# Legend
legend("bottomleft",
       legend = names(quad_cols),
       col = quad_cols,
       pch = 19,
       title = "Kinetic regimes",
       bty = "n")