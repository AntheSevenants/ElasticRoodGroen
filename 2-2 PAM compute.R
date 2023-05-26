library(MuMIn)

source("2-0 Clustering base.R")

get_clustering_results_pam <- function(base, df) {
  column_x <- paste0(base, ".x")
  column_y <- paste0(base, ".y")
  
  df <- df[!is.na(df[[column_x]]),]
  
  pass_coords <- df[, c(column_x, column_y), drop = FALSE]
  
  distance_matrix <- dist(pass_coords, method="euclidean", diag=T, upper=T)
  
  plots <- list()
  
  
  k_range <- 2:60
  
  results_list <- mclapply(k_range, function(k) {
    test <- pam(pass_coords, k = k)
    clusters <- test$clustering
    df$cluster <- clusters
    
    # pam_plot <- ggplot(data = df) +
    #   geom_point(aes(x = mds.x, y = mds.y, color = cluster))
    # plots[[as.character(k)]] <- pam_plot
    
    # Now, let's run a regression
    df_shim <- df
    df_shim$cluster <- clusters
    
    # Remove all items with negative silhouette width
    silhouette <- clusterqualSIL(distance_matrix, df_shim$cluster)
    
    df_shim$sil <- silhouette$pointqual
    df_shim$cluster <- ifelse(df_shim$sil >= 0, df_shim$cluster, 0)
    
    fit <- glm(coefficient ~ cluster, data = df_shim)
    r2_value <- r.squaredGLMM(fit)[,1] %>% unname
    
    
    # In addition, how much better does a random model perform?
    # First, generate random clusters
    fake_clusters <- sample(1:k,
                            size = nrow(df_shim),
                            replace = TRUE)
    # Then, assign them to the data
    df_shim$fake_cluster <- fake_clusters
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
    
    list("vuong_p" = vuong_p,
         "r2" = r2_value,
         "k" = k) %>% return
  }, mc.cores=min(detectCores(), 8))
  
  results <- do.call(rbind, results_list) %>% as.data.frame()
  results$k <- as.numeric(results$k)
  results$r2 <- as.numeric(results$r2)
  results$vuong_p <- as.numeric(results$vuong_p)
  
  results %>% return()
}

mds_results_pam <- get_clustering_results_pam("mds.all", df_copy)
tsne_results_pam <- get_clustering_results_pam("tsne.all", df_copy)
umap_results_pam <- get_clustering_results_pam("umap.all", df_copy)

write.csv(mds_results_pam, "output/pam_mds.csv", row.names = FALSE)
write.csv(tsne_results_pam, "output/pam_tsne.csv", row.names = FALSE)
write.csv(umap_results_pam, "output/pam_umap.csv", row.names = FALSE)
