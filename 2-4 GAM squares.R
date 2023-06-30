source("2-4 GAM.R")

squares_test <- function(df, technique, kind, spawn.x, spawn.y, width, squares_per_row,
             too.far=NA) {
  fit <- build_gam(df, technique, kind)
  
  squares_df <- compute_squares_stats(df, fit, technique, kind, spawn.x, spawn.y, width, squares_per_row, too.far)
  plot_gam_squares(df, fit, technique, kind, squares_df, too.far)
}

squares_test(df, "mds", "non_zero", -1.8, 1.7, 3.2, 3, 0.1)