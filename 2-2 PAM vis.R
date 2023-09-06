source("2-0 Clustering vis base.R")

mds_results_pam <- read.csv("output/pam_mds.csv")
tsne_results_pam <- read.csv("output/pam_tsne.csv")
umap_results_pam <- read.csv("output/pam_umap.csv")

mds_results_pam$vuong_p <- mds_results_pam$vuong_p %>% 
  round(digits = 2)
tsne_results_pam$vuong_p <- tsne_results_pam$vuong_p %>% 
  round(digits = 2)
umap_results_pam$vuong_p <- umap_results_pam$vuong_p %>% 
  round(digits = 2)

plot_bar <- function(results, fill_column) {
  ggplot(data=results) +
    geom_bar(aes(
      x = k,
      y = eval(as.name(fill_column)),
    ), stat="identity") +
    labs(y = fill_column)
}

plot_all_pam <- function(technique, results) {
  title <- ggdraw() +
    draw_label(
      paste(technique, "PAM results"),
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(# add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7))
  
  c_plot <- plot_bar(results, "r2")
  vuong_plot <- plot_bar(results, "vuong_p")
  
  grid <- plot_grid(c_plot, vuong_plot, labels=c("R^2 value", "Vuong p value")) %>%
    return
  plot_grid(title, grid, ncol = 1, rel_heights = c(0.1, 1))
}
