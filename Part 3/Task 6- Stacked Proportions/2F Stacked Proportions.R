library(readxl)
library(ggplot2)

# ---- Helpers provided ----
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

# ---- Load data (sheet f) ----
excel_path <- "D:/Aqsa/Internships/HackBIO/hb_stage_2.xlsx"
df_f <- read_excel(excel_path, sheet = "f")

# (Expected columns: stage, cell_type, proportion)
str(df_f)

# ---- Subset to s00h and s72h ----
df_f_sub <- df_f[df_f$stage %in% c("s00h", "s72h"), ]

# Optional: keep stage order consistent (s00h first)
df_f_sub$stage <- factor(df_f_sub$stage, levels = c("s00h", "s72h"))

# Optional: make cell_type a factor (stable palette mapping)
df_f_sub$cell_type <- factor(df_f_sub$cell_type)

# ---- Build a color map for cell types using hb_pal ----
cell_levels <- levels(df_f_sub$cell_type)
cell_cols <- setNames(hb_pal[seq_along(cell_levels)], cell_levels)

# Optional: add transparency (looks nicer in stacked bars)
cell_cols_alpha <- sapply(cell_cols, transparent_color, percent = 25)

# ---- Stacked barplot ----
ggplot(df_f_sub, aes(x = stage, y = proportion, fill = cell_type)) +
  geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.3) +
  scale_fill_manual(values = cell_cols_alpha) +
  scale_y_continuous(limits = c(0, 0.3), expand = c(0, 0)) +
  labs(
    title = "Panel 2f: Cell-type proportions at s00h vs s72h",
    x = "Stage",
    y = "Proportion",
    fill = "Cell Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 13, face = "bold"),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )