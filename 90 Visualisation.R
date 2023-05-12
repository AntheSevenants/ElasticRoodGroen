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

# Can also be used to plot linear models, but would be kind of useless...
plot_gam <- function(df, fit, technique, kind) {
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
  
  colnames(df_pred) <- c(x_column, y_column)
  
  # Turn into dataframe
  df_pred <- predict(fit, newdata = df_pred,
                     se.fit = TRUE) %>%
    as_tibble() %>%
    cbind(df_pred)
  
  ggplot() +
    geom_tile(data = df_pred, aes(
      x = eval(as.name(x_column)),
      y = eval(as.name(y_column)),
      fill = fit
    )) +
    scale_fill_distiller(palette = "RdYlGn") +
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

#
# MDS
#

tri_lm_plot(df, "mds")
tri_gam_plot(df, "mds")

#
# TSNE
#

tri_lm_plot(df, "tsne")
tri_gam_plot(df, "tsne")

#
# UMAP
#

tri_lm_plot(df, "umap")
tri_gam_plot(df, "umap")
