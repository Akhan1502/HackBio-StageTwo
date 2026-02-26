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

# ── Read sheet "c" ───────────────────────────────────────────────────
path <- "D:/Aqsa/Internships/HackBIO/hb_stage_2.xlsx"
df_c <- read_xlsx(path, sheet = "c")

# First column = gene names
rownames(df_c) <- df_c[[1]]
df_c <- df_c[, -1]

# Convert to numeric matrix safely
mat <- as.matrix(df_c)
mat <- apply(mat, 2, as.numeric)
rownames(mat) <- rownames(df_c)

# ── Build column annotations (robust to formats like: "Bcell_n6h") ──
cn <- colnames(mat)

# Time: capture ending like n6h / n72h
Time <- sub(".*(n[0-9]+h)$", "\\1", cn)

# CellType: everything before _n6h / _n72h
CellType <- sub("(_n[0-9]+h)$", "", cn)

annotation_col <- data.frame(
  CellType = CellType,
  Time = Time,
  row.names = cn
)

# Annotation colors (use HB palette)
cell_types <- unique(annotation_col$CellType)
times <- unique(annotation_col$Time)

ann_colors <- list(
  CellType = setNames(hb_pal[1:length(cell_types)], cell_types),
  Time = setNames(hb_pal[(length(cell_types)+1):(length(cell_types)+length(times))], times)
)

# ── Heatmap colors (HB style blues) ──────────────────────────────────
heat_cols <- colorRampPalette(c("white", hb_pal[5], hb_pal[1]))(100)

# ── Plot heatmap ─────────────────────────────────────────────────────
pheatmap(mat,
         annotation_col = annotation_col,
         annotation_colors = ann_colors,
         cluster_rows = TRUE,         # cluster genes
         cluster_cols = FALSE,        # preserve time order
         show_colnames = FALSE,
         show_rownames = FALSE,
         scale = "row",
         color = heat_cols,
         border_color = NA,
         main = "Panel 2c: Temporal structure across immune compartments")