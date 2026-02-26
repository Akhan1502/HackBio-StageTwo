install.packages(c("readxl","ggplot2","pheatmap","igraph","gridExtra","png"))
library(readxl)
library(ggplot2)
library(pheatmap)
library(igraph)
library(grid)
library(gridExtra)
library(png)

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

# Base plots (igraph) → grid grob (so we can arrange everything together)
capture_base_plot <- function(expr) {
  tf <- tempfile(fileext = ".png")
  png(tf, width = 2200, height = 1700, res = 220)
  eval(expr)
  dev.off()
  img <- png::readPNG(tf)
  grid::rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))
}

# HB-style label (top-left)
label_plot <- function(grob, label) {
  arrangeGrob(
    grob,
    top = textGrob(label,
                   x = 0.02, just = "left",
                   gp = gpar(fontsize = 14, fontface = "bold"))
  )
}

excel_path <- "D:/Aqsa/Internships/HackBIO/hb_stage_2.xlsx"
# Panel a) Cell-type ratio distributions (sheet "a")
df_a <- read_excel(excel_path, sheet = "a")
df_a$cell_type <- factor(df_a$cell_type)

pA <- ggplot(df_a, aes(x = cell_type, y = new_ratio, fill = cell_type)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1.6) +
  scale_fill_manual(values = hb_pal[1:length(levels(df_a$cell_type))]) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        plot.margin = margin(6, 6, 6, 6)) +
  labs(x = "Cell type", y = "new_ratio", title = NULL)

pA_g <- label_plot(ggplotGrob(pA), "a) Cell-type ratio distribution")

# Panel b) Half-life vs alpha scatter (sheet "b")
df_b <- read_excel(excel_path, sheet = "b")

# log2 values + quadrant rules (use thresholds taught in class)
x <- log2(df_b$half_life)
y <- log2(df_b$alpha)

quad <- ifelse(x <= 2.5 & y >= -3.6, "Q1",
               ifelse(x > 2.5 & y >= -3.6, "Q2",
                      ifelse(x <= 2.5 & y < -3.6, "Q3", "Q4")))
df_b$quad <- factor(quad, levels = c("Q1","Q2","Q3","Q4"))

pB <- ggplot(df_b, aes(x = log2(half_life), y = log2(alpha), color = quad)) +
  geom_point(size = 1.8, alpha = 0.85) +
  geom_vline(xintercept = 2.5, linetype = "dashed") +
  geom_hline(yintercept = -3.6, linetype = "dashed") +
  scale_color_manual(values = c("Q1" = "green", "Q2" = "red", "Q3" = "grey40", "Q4" = "blue")) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        plot.margin = margin(6, 6, 6, 6)) +
  labs(x = "log2(Half-life)", y = "log2(Alpha)", title = NULL)

# Add exemplar gene labels (if gene column exists)
# If your sheet uses a different column name, update "gene" below.
if ("gene" %in% names(df_b)) {
  pB <- pB + geom_text(
    data = subset(df_b, gene %in% c("Camp", "Ccr2")),
    aes(label = gene),
    color = "black",
    fontface = "bold",
    vjust = -0.8
  )
}

pB_g <- label_plot(ggplotGrob(pB), "b) Half-life vs alpha (log2)")

# Panel c) Heatmap across cell types and time (sheet "c")
df_c <- read_excel(excel_path, sheet = "c")
rownames(df_c) <- df_c[[1]]
df_c <- df_c[, -1]
mat_c <- as.matrix(df_c)
class(mat_c) <- "numeric"

cn <- colnames(mat_c)
CellType <- gsub("n[0-9]+h", "", cn)
Time <- gsub(".*(n[0-9]+h)", "\\1", cn)

annotation_col <- data.frame(CellType = CellType, Time = Time, row.names = cn)

pC <- pheatmap(mat_c,
               annotation_col = annotation_col,
               cluster_rows = TRUE,
               cluster_cols = FALSE,
               scale = "row",
               show_rownames = FALSE,
               show_colnames = FALSE,
               color = colorRampPalette(c("white", hb_pal[1]))(120),
               silent = TRUE)$gtable

pC_g <- label_plot(pC, "c) Time-series expression heatmap")

# Panel d) Pathway enrichment heatmap (sheet "d_1")
df_d <- read_excel(excel_path, sheet = "d_1")
rownames(df_d) <- df_d[[1]]
df_d <- df_d[, -1]
mat_d <- as.matrix(df_d)
mat_d <- apply(mat_d, 2, as.numeric)
rownames(mat_d) <- rownames(df_d)

max_abs <- max(abs(mat_d), na.rm = TRUE)
breaks_d <- seq(-max_abs, max_abs, length.out = 101)
cols_d <- colorRampPalette(c(hb_pal[3], "white", hb_pal[1]))(100)

pD <- pheatmap(mat_d,
               cluster_rows = FALSE,
               cluster_cols = FALSE,
               color = cols_d,
               breaks = breaks_d,
               silent = TRUE)$gtable

pD_g <- label_plot(pD, "d) Pathway enrichment heatmap")

# Panel e) Bubble plot (sheet "e")
df_e <- read_excel(excel_path, sheet = "e")
df_e$half_life <- as.numeric(df_e$half_life)
df_e$alpha <- as.numeric(df_e$alpha)
df_e$count <- as.numeric(df_e$count)

pE <- ggplot(df_e, aes(x = half_life, y = alpha, color = stage, size = count)) +
  geom_point(alpha = 0.85) +
  scale_color_manual(values = c("6h" = hb_pal[1], "72h" = hb_pal[2])) +
  scale_size_continuous(range = c(2.5, 10)) +
  theme_minimal(base_size = 11) +
  theme(plot.margin = margin(6, 6, 6, 6)) +
  labs(x = "Half-life", y = "Alpha", color = "Stage", size = "Count", title = NULL)

pE_g <- label_plot(ggplotGrob(pE), "e) Bubble plot (kinetic regimes)")

# Panel f) Stacked proportions (sheet "f") — fixed y-axis 0–0.3
df_f <- read_excel(excel_path, sheet = "f")
df_f_sub <- subset(df_f, stage %in% c("s00h", "s72h"))
df_f_sub$cell_type <- factor(df_f_sub$cell_type)
df_f_sub$stage <- factor(df_f_sub$stage, levels = c("s00h", "s72h"))

fill_cols <- hb_pal[1:length(levels(df_f_sub$cell_type))]
names(fill_cols) <- levels(df_f_sub$cell_type)

pF <- ggplot(df_f_sub, aes(x = stage, y = proportion, fill = cell_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = fill_cols) +
  scale_y_continuous(limits = c(0, 0.3), expand = c(0, 0)) +
  theme_minimal(base_size = 11) +
  theme(plot.margin = margin(6, 6, 6, 6)) +
  labs(x = "Time", y = "Proportion", fill = "Cell type", title = NULL)

pF_g <- label_plot(ggplotGrob(pF), "f) Stacked proportions (s00h vs s72h)")

# Panel g) Directed cell–cell network (sheet "g") — use transparent_color + hb
df_g <- read_excel(excel_path, sheet = "g")
node_names <- df_g[[1]]
adj <- as.matrix(df_g[, -1])
adj <- apply(adj, 2, as.numeric)
rownames(adj) <- node_names
colnames(adj) <- colnames(df_g)[-1]

g_net <- graph_from_adjacency_matrix(adj, mode = "directed", weighted = TRUE, diag = FALSE)
g_net <- delete_edges(g_net, E(g_net)[weight == 0])

# Styling
V(g_net)$color <- transparent_color(hb_pal[4], percent = 20)
V(g_net)$frame.color <- "white"
V(g_net)$size <- 18
V(g_net)$label.cex <- 1.0

w <- E(g_net)$weight
E(g_net)$arrow.size <- (w / max(w)) * 2
E(g_net)$width <- 0.6 + (w / max(w)) * 2
E(g_net)$color <- transparent_color(hb_pal[1], percent = 35)

set.seed(123)
lay <- layout_with_fr(g_net)

pG <- capture_base_plot(quote({
  plot(g_net,
       layout = lay,
       edge.curved = 0.15,
       main = "")
}))

pG_g <- label_plot(pG, "g) Directed interaction network")

# FINAL ASSEMBLY (layout like the sample): 3 rows with controlled spans
final_panel <- arrangeGrob(
  pA_g, pB_g,
  pC_g, pD_g,
  pE_g, pF_g, pG_g,
  layout_matrix = rbind(
    c(1,1,1,2,2,2),
    c(3,3,3,4,4,4),
    c(5,5,6,6,7,7)
  )
)
grid.newpage(); grid.draw(final_panel)
