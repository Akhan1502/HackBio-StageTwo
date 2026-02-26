# install.packages(c("readxl","ggplot2","pheatmap","igraph"))

library(readxl)
library(ggplot2)
library(pheatmap)
library(igraph)

# ── Helpers provided ────────────────────────────────────────────────
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

# test the color palette
plot(1:length(hb_pal), 1:length(hb_pal), col = hb_pal, pch = 19)

# ── Inspect the Excel file ─────────────────────────────────────────
# Put hb_stage_2.xlsx in your working directory OR set full path here:
excel_path <- "D:/Aqsa/Internships/HackBIO/hb_stage_2.xlsx"

# List all sheets (these should correspond to panels a–g)
sheets <- excel_sheets(excel_path)
print(sheets)

# Quick preview: dimensions + first rows of each sheet
for (sh in sheets) {
  cat("\n============================\n")
  cat("Sheet:", sh, "\n")
  df <- read_excel(excel_path, sheet = sh)
  print(dim(df))
  print(head(df, 3))
}

# ── Map each sheet to Figure 2 panel ───────────────────────────────
panel_map <- list(
  a   = "Panel a: Cell type ratio distribution (boxplot-ready table)",
  b   = "Panel b: log2(Half-life) vs log2(Alpha) scatter (quadrant plot)",
  c   = "Panel c: Time-series / cell-type expression matrix (heatmap)",
  d_1 = "Panel d: Pathway activity scores across cell types (heatmap)",
  e   = "Panel e: Bubble plot data (half_life, alpha, count, stage)",
  f   = "Panel f: Cell-type proportions by stage (stacked barplot)",
  g   = "Panel g: Interaction adjacency matrix (network/igraph)"
)

cat("\n\n=== SHEET → PANEL MAPPING ===\n")
print(panel_map)