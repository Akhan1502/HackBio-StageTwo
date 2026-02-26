library(readxl)
library(pheatmap)

# ── Helpers ──────────────────────────────────────────────────────────
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

# ── Read sheet "d_1" ─────────────────────────────────────────────────
path <- "D:/Aqsa/Internships/HackBIO/hb_stage_2.xlsx"
df_d <- read_xlsx(path, sheet = "d_1")

# First column = pathway names
rownames(df_d) <- df_d[[1]]
df_d <- df_d[, -1]

# Convert safely to numeric matrix
mat <- as.matrix(df_d)
mat <- apply(mat, 2, as.numeric)
rownames(mat) <- rownames(df_d)

# ── Diverging color scale centered at 0 ──────────────────────────────
max_abs <- max(abs(mat), na.rm = TRUE)

breaks <- seq(-max_abs, max_abs, length.out = 101)

# Use HB colors instead of default blue/red
low_col  <- transparent_color(hb_pal[3], percent = 10)   # red tone
mid_col  <- "white"
high_col <- transparent_color(hb_pal[1], percent = 10)   # blue tone

cols <- colorRampPalette(c(low_col, mid_col, high_col))(100)

# ── Plot heatmap (NO clustering) ─────────────────────────────────────
pheatmap(mat,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         color = cols,
         breaks = breaks,
         border_color = NA,
         fontsize_row = 9,
         fontsize_col = 9,
         main = "Panel 2d: Pathway enrichment across timepoints")