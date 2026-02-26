# install.packages(c("readxl","ggplot2"))
library(readxl)
library(ggplot2)

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


path <- "D:/Aqsa/Internships/HackBIO/hb_stage_2.xlsx"   # change if needed
df_a <- read_xlsx(path, sheet = "a")

df_a$cell_type  <- factor(df_a$cell_type)
df_a$new_ratio  <- as.numeric(df_a$new_ratio)

# Build a consistent color map for cell types (uses hb_pal)
cell_levels <- levels(df_a$cell_type)
cell_cols <- hb_pal[seq_along(cell_levels)]
names(cell_cols) <- cell_levels

ggplot(df_a, aes(x = cell_type, y = new_ratio, fill = cell_type)) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.shape  = 16,
    outlier.size   = 1.6,
    linewidth      = 0.4
  ) +
  scale_fill_manual(values = cell_cols) +
  coord_cartesian(ylim = c(0, max(df_a$new_ratio, na.rm = TRUE) * 1.1)) +  # safer than ylim()
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "none",
    plot.margin      = margin(6, 6, 6, 6)
  ) +
  labs(
    title = "Cell-type Ratio Distributions",
    x = "Cell type",
    y = "new_ratio"
  )