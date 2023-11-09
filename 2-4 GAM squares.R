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

scatter_pairs <- function(df, technique, kind, spawn.x, spawn.y, width, squares_per_row,
                          too.far=NA) {
  fit <- build_gam(df, technique, kind)
  squares_df <- compute_squares_stats(df, fit, technique, kind, spawn.x, spawn.y, width, squares_per_row, too.far)
  
  ggplot(squares_df, aes(y = mean_adjectiveness,
                         x = mean_coefficient,
                         color = dominant_order)) +
    geom_point(shape=15, size=5) +
    scale_color_manual(values=c("green", "grey", "red"))
}

conc_disc_pairs <- function(df, technique, kind, spawn.x, spawn.y, width, squares_per_row,
                            too.far=NA) {
  fit <- build_gam(df, technique, kind)
  squares_df <- compute_squares_stats(df, fit, technique, kind, spawn.x, spawn.y, width, squares_per_row, too.far)
  
  d_table <- as.table(rbind(squares_df$mean_adjectiveness,
                             squares_df$mean_coefficient))
  
  #ConDisPairs(d_table) %>% return
  DescTools:::.DoCount(squares_df$mean_adjectiveness, squares_df$mean_coefficient) %>% return
}

do_squares_regression <- function(df, fit, technique, kind, spawn.x, spawn.y,
                                  width, squares_per_row, too.far=NA) {
  large_squares_df <- compute_squares_stats(df, fit, technique, kind,
                                            spawn.x, spawn.y,
                                            width, squares_per_row, too.far)
  small_squares_df <- compute_squares_stats(df, fit, technique, kind,
                                            spawn.x, spawn.y,
                                            width, squares_per_row * 2, too.far,
                                            FALSE)
  
  small_square_parents <- c()
  for (i in 0:(squares_per_row - 1)) {
    current_line <- c()
    for (j in 1:squares_per_row) {
      current_line <- append(current_line, rep(j + i * squares_per_row , 2))
    }
    
    current_line <- rep(current_line, 2)
    small_square_parents <- append(small_square_parents, current_line)
  }
  
  small_squares_df$parent <- small_square_parents
  small_squares_df$has_dominant_order <- NULL
  small_squares_df$dominant_order <- NULL
  
  small_squares_df <- merge(x = small_squares_df,
                            y = large_squares_df[,c("index", "has_dominant_order", "dominant_order")],
                            by.x="parent", by.y="index", all.x = TRUE)
  
  small_squares_df$dominant_order <- ifelse(is.na(small_squares_df$dominant_order),
                                            "none",
                                            small_squares_df$dominant_order)
  
  glm(has_dominant_order ~ points_count, family="binomial", data=small_squares_df)
}
