library(dplyr)
library(magrittr)

# Read coefficients output
df <- read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")

df$order <- ifelse(df$coefficient < 0, "green",
                   ifelse(df$coefficient > 0, "red", NA))

cluster_stats <- function(grouping_col) {
  df %>%
    group_by_at(grouping_col) %>%
    summarise(avg = mean(coefficient),
              med = median(coefficient),
              count = length(coefficient),
              adjectiveness = mean(adjectiveness),
              redness = (table(order) / length(coefficient))[["red"]],
              greenness = (table(order) / length(coefficient))[["green"]],
              t.test = t.test(coefficient)$p.value <= 0.05) %>%
    filter(!is.na(!! sym(grouping_col)))
}

### FULL

cluster_stats("all.kmeans.full")
cluster_stats("non_zero.kmeans.full")
cluster_stats("outside_sd.kmeans.full")

### PCA2D

cluster_stats("all.kmeans.pca2d")
cluster_stats("non_zero.kmeans.pca2d")
cluster_stats("outside_sd.kmeans.pca2d")

### PCA10D

cluster_stats("all.kmeans.pca10d")
cluster_stats("non_zero.kmeans.pca10d")
cluster_stats("outside_sd.kmeans.pca10d")

### TSNE2D

cluster_stats("all.kmeans.tsne2d")
cluster_stats("non_zero.kmeans.tsne2d")
cluster_stats("outside_sd.kmeans.tsne2d")

### TSNE3D

cluster_stats("all.kmeans.tsne3d")
cluster_stats("non_zero.kmeans.tsne3d")
cluster_stats("outside_sd.kmeans.tsne3d")

# Frequency counts
table(df$all.kmeans)



