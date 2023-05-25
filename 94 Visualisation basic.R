df <-
  read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")
#df <- df[df$coefficient != 0,]

df$order <- ifelse(df$coefficient > 0, "red", "green")
df$order <- ifelse(df$coefficient == 0, "removed", df$order)

colours = c("green", "red", "gray")

plot_basic_coords <- function(technique) {
  x_column <- paste0(technique, ".all.x")
  y_column <- paste0(technique, ".all.y")
  
  plot(
    df[, x_column],
    df[, y_column],
    ylim = c(-2.5, 2.5),
    pch = 19,
    cex = 0.5,
    col = colours[factor(df$order)],
    xlab = "",
    ylab = "",
    asp = 1
  )
}
