library(readxl)
library(igraph)

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

# ---- Load data (sheet g) ----
excel_path <- "D:/Aqsa/Internships/HackBIO/hb_stage_2.xlsx"
df_g <- read_excel(excel_path, sheet = "g")

# First column = node names, remaining columns = adjacency weights
node_names <- df_g[[1]]
adj <- as.matrix(df_g[, -1])

# Ensure numeric
adj <- apply(adj, 2, as.numeric)
rownames(adj) <- node_names
colnames(adj) <- colnames(df_g)[-1]

# ---- Build directed weighted graph ----
g <- graph_from_adjacency_matrix(adj, mode = "directed", weighted = TRUE, diag = FALSE)

# Remove zero-weight edges
g <- delete_edges(g, E(g)[weight == 0])

# ---- Styling ----
V(g)$color <- transparent_color(hb_pal[4], percent = 20)  # soft pink
V(g)$frame.color <- "white"
V(g)$size <- 18
V(g)$label.cex <- 1.0

# Edge scaling: arrow size proportional to weight (safe scaling)
w <- E(g)$weight
w_scaled <- (w / max(w)) * 2  # scales to ~0–2
E(g)$arrow.size <- w_scaled
E(g)$width <- 0.5 + (w / max(w)) * 2
E(g)$color <- transparent_color(hb_pal[1], percent = 35)  # semi-transparent blue

# ---- Force-directed layout ----
set.seed(123)
lay <- layout_with_fr(g)

# ---- Plot ----
plot(
  g,
  layout = lay,
  main = "Panel 2g: Directed cell–cell interaction network",
  edge.curved = 0.15
)