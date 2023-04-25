library(ggplot2) # plots
library(mgcv) # GAM
library(tidyr) # expand_grid

df <-
  read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")
df <- df[!is.na(df$mds.x),]
df <- df[abs(df$coefficient) >= sd(df$coefficient),]
df <- df[df$coefficient != 0,]
df$sign <- ifelse(df$coefficient > 0, "red", "green")

# https://stackoverflow.com/a/46489816
round_any = function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

plot_gam <- function(fit, technique) {
  x_column <- paste0(technique, ".x")
  y_column <- paste0(technique, ".y")
  
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
      size = 1,
      #width = 0.02,
      #height = 0.02,
      alpha = 0.5,
      aes(x = eval(as.name(x_column)),
          y = eval(as.name(y_column)),
          color = sign)
    ) +
    scale_color_manual(values = c("green", "red"))
}

#
# MDS
#
mds.fit <-
  gam(coefficient ~ te(mds.x, mds.y),
      data = df,
      method = "REML")
plot_gam(mds.fit, "mds")

vis.gam(
  mds.fit,
  view = c("mds.x", "mds.y"),
  plot.type = "contour",
  color = "heat"
)

#
# TSNE
#

tsne.fit <-
  gam(coefficient ~ te(tsne.x, tsne.y),
      data = df,
      method = "REML")
plot_gam(tsne.fit, "tsne")
vis.gam(
  tsne.fit,
  view = c("tsne.x", "tsne.y"),
  plot.type = "contour",
  color = "heat"
)

#
# UMAP
#

umap.fit <-
  gam(coefficient ~ te(umap.x, umap.y),
      data = df,
      method = "REML")
plot_gam(umap.fit, "umap")
vis.gam(
  umap.fit,
  view = c("umap.x", "umap.y"),
  plot.type = "contour",
  color = "heat"
)
