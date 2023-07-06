source("2-4 GAM.R")

squares_test <- function(df, technique, kind, spawn.x, spawn.y, width, squares_per_row,
             too.far=NA) {
  fit <- build_gam(df, technique, kind)
  squares_df <- compute_squares_stats(df, fit, technique, kind, spawn.x, spawn.y, width, squares_per_row, too.far)
  
  df_copy <- df
  squares_vector <- rep(NA, dim(df)[1])
  for(i in 1:nrow(squares_df)) {
    square <- squares_df[i,]
    
    point_indices <- strsplit(square[["point_indices"]], ",")[[1]]
    for (point_index in point_indices %>% as.numeric()) {
      squares_vector[point_index] <- i
    }
  }
  df_copy$square <- squares_vector
  
  pairwise_t <- pairwise.t.test(df_copy$adjectiveness, df_copy$square)
  
  plot_gam_squares(df, fit, technique, kind, squares_df, pairwise_t, too.far)
}

squares_test(df, "mds", "non_zero", -1.8, 1.7, 3.2, 3, 0.1)
squares_test(df, "tsne", "non_zero", -5, 5, 10, 3, 0.1)
squares_test(df, "umap", "non_zero", -2.8, 2.6, 5.25, 3, 0.1)
