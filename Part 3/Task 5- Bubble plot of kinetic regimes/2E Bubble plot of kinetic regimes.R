
library(readxl)
library(ggplot2)

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

# ── Read sheet "e" ───────────────────────────────────────────────────
path <- "D:/Aqsa/Internships/HackBIO/hb_stage_2.xlsx"
df_e <- read_xlsx(path, sheet = "e")

# Ensure correct types
df_e$half_life <- as.numeric(df_e$half_life)
df_e$alpha     <- as.numeric(df_e$alpha)
df_e$count     <- as.numeric(df_e$count)
df_e$stage     <- as.factor(df_e$stage)

# Stage colors (HB palette + slight transparency)
stage_levels <- levels(df_e$stage)
stage_cols <- hb_pal[seq_along(stage_levels)]
stage_cols <- sapply(stage_cols, transparent_color, percent = 10)
names(stage_cols) <- stage_levels

# ── Bubble plot ──────────────────────────────────────────────────────
ggplot(df_e, aes(x = half_life,
                 y = alpha,
                 color = stage,
                 size = count)) +
  
  geom_point(alpha = 0.85) +
  
  scale_size_continuous(range = c(4, 14)) +
  scale_color_manual(values = stage_cols) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  ) +
  
  labs(
    title = "Panel 2e: Kinetic Regimes of Gene Expression",
    x = "Half-life",
    y = "Alpha (synthesis rate)",
    color = "Stage",
    size = "Gene count"
  )