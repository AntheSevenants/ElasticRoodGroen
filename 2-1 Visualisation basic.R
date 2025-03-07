source("2-0 Clustering base.R")

df$order <- ifelse(df$coefficient > 0, "red", "green")
df$order <- ifelse(df$coefficient == 0, "removed", df$order)
df$order <- factor(df$order, levels=c("red", "green", "removed"))

colours = c("red", "green", "gray")

plot_basic_coords <- function(technique, legend=FALSE) {
  x_column <- paste0(technique, ".all.x")
  y_column <- paste0(technique, ".all.y")
  
  par(mar=c(4, 4, 1, 1))
  
  plot(
    df[, x_column],
    df[, y_column],
    pch = 19,
    cex = 0.5,
    col = colours[factor(df$order)],
    xlab = "x",
    ylab = "y",
    asp = 1
  )
  
  if (legend) {
    legend("bottomright",
           legend = levels(df$order),
           pch = 19,
           col = colours,
           bty = "n",
           horiz = FALSE)
  }
}
