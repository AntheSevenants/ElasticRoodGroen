source("2-0 Clustering vis base.R")

mds_results_dbscan <- read.csv("output/dbscan_mds.csv")
tsne_results_dbscan <- read.csv("output/dbscan_tsne.csv")
umap_results_dbscan <- read.csv("output/dbscan_umap.csv")

plot_tile <- function(data, fill_column) {
  ggplot(data = data) +
    geom_tile(aes(
      x = pts,
      y = eps,
      fill = eval(as.name(fill_column))
    )) +
    scale_fill_distiller(palette = "Greys", direction = 1) +
    labs(fill = fill_column)
}

plot_all_dbscan <- function(results) {
  cluster_count_plot <- plot_tile(results, "cluster_count")
  cluster_count_log_plot <-  plot_tile(results, "cluster_count_log")
  r2_plot <- plot_tile(results, "r2")
  vuong_plot <- plot_tile(results, "vuong_p")
  
  plot_grid(cluster_count_plot, cluster_count_log_plot, r2_plot, vuong_plot,
            labels=c("Clusters", "Clusters (log)", "R^2 values", "Vuong p value"))
}