library(magrittr)
library(ggplot2)
library(DescTools)
library(dbscan)

# Read coefficients output
df <-
  read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")

base <- "mds"

x_column <- paste0(base, ".x")
y_column <- paste0(base, ".y")

# Remove non-participle columns
df_copy <- df[!is.na(df[x_column]),]
# Remove zeroes
df_copy <- df_copy[df_copy["coefficient"] != 0,]

# Only keep the required coordinates
pass_coords <- df_copy[, c(x_column, y_column), drop = FALSE]

pts_range <- 2:100
eps_range <- seq(0, 1, by = 0.01)

pts_col <- c()
eps_col <- c()
cluster_no_col <- c()
c_values <- c()

for (pts in pts_range) {
  for (eps in eps_range) {
    clusters <-
      dbscan(pass_coords, minPts = pts, eps = eps)$cluster %>% as.numeric()
    
    cluster_no = length(unique(clusters))
    
    pts_col <- append(pts_col, pts)
    eps_col <- append(eps_col, eps)
    cluster_no_col <- append(cluster_no_col, cluster_no)
    
    if (cluster_no > 1) {
      df_shim <- df_copy
      df_shim$cluster <- clusters %>% as.character() %>% as.factor()
      
      fit <- glm(coefficient ~ cluster, data=df_shim)
      c_value <- Cstat(fit)
    } else {
      c_value <- NA
    }
    
    c_values <- append(c_values, c_value)
  }
}

results <- data.frame(pts = pts_col,
                      eps = eps_col,
                      cluster_no = cluster_no_col,
                      cluster_no_log = log10(cluster_no),
                      c_value = c_values)

ggplot() +
  geom_tile(data = results, aes(
    x = pts,
    y = eps,
    fill = c_value
  )) +
  scale_fill_distiller(palette = "Greys", direction=1)


ggplot() +
  geom_tile(data = results, aes(
    x = pts,
    y = eps,
    fill = cluster_no_log
  )) +
  scale_fill_distiller(palette = "Greys", direction=1)

View(results)
