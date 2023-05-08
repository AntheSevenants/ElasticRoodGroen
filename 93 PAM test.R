library(magrittr)
library(cluster)

df <- read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")

df_copy <- df
df_copy <- df_copy[df_copy$coefficient != 0, ]

base <- "mds"

column_x <- paste0(base, ".x")
column_y <- paste0(base, ".y")

df_copy <- df_copy[!is.na(df_copy[[column_x]]),]

pass_coords <- df_copy[, c(column_x, column_y), drop = FALSE]

test <- pam(pass_coords, k=2)
clusters <- test$clustering
df_copy$cluster <- clusters

ggplot(data=df_copy) +
  geom_point(aes(x=mds.x, y=mds.y, color=cluster))
