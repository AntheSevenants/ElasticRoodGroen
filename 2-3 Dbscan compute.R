library(MuMIn)

source("2-0 Clustering base.R")

get_clustering_results_dbscan <- function(base, df) {
  column_x <- paste0(base, ".x")
  column_y <- paste0(base, ".y")
  
  df <- df[!is.na(df[[column_x]]),]
  
  pass_coords <- df[, c(column_x, column_y), drop = FALSE]
  
  pts_range <- 2:25
  eps_range <- seq(0, 0.25, 0.001)
  
  pts_col <- c()
  eps_col <- c()
  r2_col <- c()
  cluster_count_col <- c()
  vuong_p_col <- c()
  
  for (pts in pts_range) {
    for (eps in eps_range) {
      clusters <- dbscan(pass_coords, minPts = pts, eps = eps)$cluster %>%
        as.numeric() %>% as.factor()
      cluster_count <- length(unique(clusters))
      
      pts_col <- append(pts_col, pts)
      eps_col <- append(eps_col, eps)
      cluster_count_col <- append(cluster_count_col, cluster_count)
      
      if (cluster_count > 1) {
        df_shim <- df
        df_shim$cluster <- clusters
        
        fit <- glm(coefficient ~ cluster, data = df_shim)
        r2_value <- r.squaredGLMM(fit)[,1] %>% unname
        
        
        # In addition, how much better does a random model perform?
        # First, generate random clusters
        fake_clusters <- sample(1:cluster_count,
                                size = nrow(df_shim),
                                replace = TRUE)
        # Then, assign them to the data
        df_shim$fake_cluster <- fake_clusters %>% as.factor()
        # We fit a model using the "random" model
        fake_fit <- glm(coefficient ~ fake_cluster, data = df_shim)
        
        # How, the question is, which model is better? And is the real model
        # fundamentally better than the baseline random model?
        # Since the models are not nested, we cannot use LRT
        # Therefore, we use the Vuong (1989) test,
        # graciously implemented in nonnest2
        vuong <- vuongtest(fit, fake_fit)
        vuong_p <-
          vuong$p_omega # if p < 0.05, the models can be distinguished
      } else {
        r2_value <- NA
        vuong_p <- NA
      }
      
      r2_col <- append(r2_col, r2_value)
      vuong_p_col <- append(vuong_p, vuong_p_col)
    }
  }
  
  
  #icci(fit, fake_fit)
  
  results <- data.frame(
    pts = pts_col,
    eps = eps_col,
    cluster_count = cluster_count_col,
    cluster_count_log = log10(cluster_count_col),
    r2 = r2_col,
    vuong_p = vuong_p_col
  )
  
  return(results)
}

mds_results_dbscan <- get_clustering_results_dbscan("mds.non_zero", df_copy)
tsne_results_dbscan <- get_clustering_results_dbscan("tsne.mds.non_zero", df_copy)
umap_results_dbscan <- get_clustering_results_dbscan("umap.mds.non_zero", df_copy)

write.csv(mds_results_dbscan, "output/dbscan_mds.csv", row.names=FALSE)
write.csv(tsne_results_dbscan, "output/dbscan_tsne.csv", row.names=FALSE)
write.csv(umap_results_dbscan, "output/dbscan_umap.csv", row.names=FALSE)
