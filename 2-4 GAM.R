library(ggplot2) # plots
library(cowplot) # arranging plots
library(mgcv) # GAM
library(tidyr) # expand_grid

df <-
  read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")
df$sign <- ifelse(df$coefficient > 0, "red", "green")

# https://stackoverflow.com/a/46489816
round_any = function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

check_kind <- function(kind) {
  if (!kind %in% c("all", "non_zero", "outside_sd")) {
    stop("Kind should be: all, non-zero, outside-sd")
  }
}

build_lm <- function(df, technique, kind) {
  check_kind(kind)
  
  x_column <- paste0(technique, ".", kind, ".x")
  y_column <- paste0(technique, ".", kind, ".y")
  
  # Remove NA values
  df <- df[!is.na(df[[x_column]]), ]
  
  formula <-
    as.formula(paste0("coefficient ~ ", x_column, " + ", y_column))
  
  fit <- lm(formula, data = df)
  
  return(fit)
}

build_gam <- function(df, technique, kind) {
  check_kind(kind)
  
  x_column <- paste0(technique, ".", kind, ".x")
  y_column <- paste0(technique, ".", kind, ".y")
  
  # Remove NA values
  df <- df[!is.na(df[[x_column]]), ]
  
  formula <-
    as.formula(paste0("coefficient ~ te(", x_column, ", ", y_column, ")"))
  
  fit <-
    gam(formula,
        data = df,
        method = "REML")
  
  return(fit)
}

get_predictions_df <- function(df, fit, technique, kind, too.far=NA) {
  check_kind(kind)
  
  x_column <- paste0(technique, ".", kind, ".x")
  y_column <- paste0(technique, ".", kind, ".y")
  
  # Remove NA values
  df <- df[!is.na(df[[x_column]]), ]
  
  df_pred <- expand_grid(
    x = seq(
      from = round_any(min(df[[x_column]]), 0.1, floor),
      to = round_any(max(df[[x_column]]), 0.1, ceiling),
      length.out = 100
    ),
    y = seq(
      from = round_any(min(df[[y_column]]), 0.1, floor),
      to = round_any(max(df[[y_column]]), 0.1, ceiling),
      length.out = 100
    )
  )
  
  if (!is.na(too.far)) {
    too_far <-
      exclude.too.far(df_pred$x, df_pred$y, df[[x_column]], df[[y_column]], too.far)
  } else {
    too_far <- rep(FALSE, df_pred$x %>% length)
  }
  
  colnames(df_pred) <- c(x_column, y_column)
  
  # Turn into dataframe
  df_pred <- predict(fit, newdata = df_pred,
                     se.fit = TRUE) %>%
    as_tibble() %>%
    cbind(df_pred)
  
  # Remove "too far" data points
  df_pred$too_far <- too_far
  df_pred <- df_pred[!df_pred$too_far,]
  
  return(df_pred)
}

# Can also be used to plot linear models, but would be kind of useless...
plot_gam <- function(df, fit, technique, kind, too.far=NA) {
  check_kind(kind)
  
  x_column <- paste0(technique, ".", kind, ".x")
  y_column <- paste0(technique, ".", kind, ".y")
  
  df_pred <- get_predictions_df(df, fit, technique, kind, too.far)
  
  output_plot <- ggplot() +
    geom_tile(data = df_pred, aes(
      x = eval(as.name(x_column)),
      y = eval(as.name(y_column)),
      fill = fit
    )) +
    scale_fill_gradientn(colours = c("#96E637", "#FEFEBD", "#DE193E"), limits=c(-0.5, 0.5)) +
    #geom_contour(data=df_pred, aes(x=x, y=y, z = fit), colour = "white")
    geom_point(
      data = df,
      size = 0.5,
      #width = 0.02,
      #height = 0.02,
      alpha = 0.2,
      aes(
        x = eval(as.name(x_column)),
        y = eval(as.name(y_column)),
        color = sign
      )
    ) +
    scale_color_manual(values = c("green", "red")) +
    xlab(x_column) +
    ylab(y_column)
  
  return(output_plot)
}


plot_gam_squares <-
  function(df,
           fit,
           technique,
           kind,
           squares_df,
           too.far = NA) {
    output_plot <- plot_gam(df, fit, technique, kind, too.far)
    
    output_plot <- output_plot +
      geom_rect(
        aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax,
          color = dominant_order
        ),
        fill = "transparent",
        data = squares_df
      ) +
      geom_text(aes(
        label = points_count,
        x = xmin + 0.1,
        y = ymin - 0.1
      ),
      data = squares_df)
    
    return(output_plot)
  }

get_squares_coords <- function(spawn.x, spawn.y, width, squares_per_row) {
  xmin <- c()
  xmax <- c()
  ymin <- c()
  ymax <- c()
  
  small_rect_size <- width / squares_per_row
  
  for (i in 1:squares_per_row) {
    for (j in 1:squares_per_row) {
      xmin <- append(xmin, spawn.x + (i - 1) * small_rect_size)
      xmax <- append(xmax, spawn.x + (i) * small_rect_size)
      ymin <- append(ymin, spawn.y - (j - 1) * small_rect_size)
      ymax <- append(ymax, spawn.y - (j) * small_rect_size)
    }
  }
  
  squares_df <- data.frame(xmin = xmin,
                           xmax = xmax,
                           ymin = ymin,
                           ymax = ymax)
  
  return(squares_df)
}

compute_squares_stats <- function(df, fit, technique, kind, spawn.x, spawn.y, width, squares_per_row, too.far=NA) {
  # We get the coordinates of the different squares we want to compute statistics for
  squares_df <- get_squares_coords(spawn.x, spawn.y, width, squares_per_row)
  
  #print(squares_df)
  
  # Get the names of the x and y columns
  # TODO: this should really be generalised at this point
  x_column <- paste0(technique, ".", kind, ".x")
  y_column <- paste0(technique, ".", kind, ".y")

  # Get 'tuples' of all coordinates
  coords <- mapply(c, df[[x_column]], df[[y_column]], SIMPLIFY=FALSE)
  
  # These vectors will be attached to squares_df
  points_count_c <- c()
  dominant_order_c <- c()
  has_dominant_order_c <- c()
  
  # Compute stats for each square
  for(i in 1:nrow(squares_df)) {
    square <- squares_df[i,]
    
    # For each square, go over all points
    in_square <- lapply(coords, function(coord) {
      x <- coord[1]
      y <- coord[2]
      
      # If a coordinate is NA, there is no bounding box to be calculated
      if (is.na(x)) {
        return(FALSE)
      }
      
      # Bounding box calculations
      return(x < square[["xmax"]] && x > square[["xmin"]] && 
             y > square[["ymax"]] && y < square[["ymin"]])
    }) %>% as.logical()
    
    # We now have a vector which, for each point, says whether it is in the
    # square or not
    # We use it to index our dataframe of points
    in_square_points <- df[in_square,]
    
    # How many points are in the square?
    points_count <- dim(in_square_points)[1]
    points_count_c <- append(points_count_c, points_count)
    
    # Is this square red, green or ambivalent?
    cross_tab <- table(df$sign)
    t_test_results <- t.test(in_square_points$coefficient)
    
    #print(t_test_results)
    
    has_dominant_order <- t_test_results$p.value <= 0.05
    has_dominant_order_c <- append(has_dominant_order_c, has_dominant_order)
    
    dominant_order <- NA
    if (has_dominant_order) {
      dominant_order <- ifelse(t_test_results$estimate < 0, "green", "red")
    }
    
    dominant_order_c <- append(dominant_order_c, dominant_order)
  }

  squares_df$has_dominant_order <- has_dominant_order_c
  squares_df$points_count <- points_count_c
  squares_df$dominant_order <- dominant_order_c

  return(squares_df)
}

tri_lm_plot <- function(df, technique) {
  title <- ggdraw() +
    draw_label(
      paste(technique, "semantic space"),
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(# add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7))
  
  fit_all <- build_lm(df, technique, "all")
  fit_non_zero <- build_lm(df, technique, "non_zero")
  fit_sd <- build_lm(df, technique, "outside_sd")
  
  plot_all <- plot_gam(df, fit_all, technique, "all")
  plot_non_zero <- plot_gam(df, fit_non_zero, technique, "non_zero")
  plot_sd <- plot_gam(df, fit_sd, technique, "outside_sd")
  
  grid <- plot_grid(plot_all,
                    plot_non_zero,
                    plot_sd,
                    labels = c('All', 'Non-zero', 'SD'))
  plot_grid(title, grid, ncol = 1, rel_heights = c(0.1, 1))
}

tri_gam_plot <- function(df, technique) {
  title <- ggdraw() +
    draw_label(
      paste(technique, "semantic space"),
      fontface = 'bold',
      x = 0,
      hjust = 0
    ) +
    theme(# add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7))
  
  fit_all <- build_gam(df, technique, "all")
  fit_non_zero <- build_gam(df, technique, "non_zero")
  fit_sd <- build_gam(df, technique, "outside_sd")
  
  plot_all <- plot_gam(df, fit_all, technique, "all")
  plot_non_zero <- plot_gam(df, fit_non_zero, technique, "non_zero")
  plot_sd <- plot_gam(df, fit_sd, technique, "outside_sd")
  
  grid <- plot_grid(plot_all,
                    plot_non_zero,
                    plot_sd,
                    labels = c('All', 'Non-zero', 'SD'))
  plot_grid(title, grid, ncol = 1, rel_heights = c(0.1, 1))
}
