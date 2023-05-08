library(magrittr)
library(cluster)
library(cowplot) # arranging plots

df <- read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")

df_copy <- df
df_copy <- df_copy[df_copy$coefficient != 0, ]

base <- "mds"

column_x <- paste0(base, ".x")
column_y <- paste0(base, ".y")

df_copy <- df_copy[!is.na(df_copy[[column_x]]),]

pass_coords <- df_copy[, c(column_x, column_y), drop = FALSE]

k_range <- 2:100

plots <- list()
c_col <- c()
vuong_p_col <- c()

for (k in k_range) {
  test <- pam(pass_coords, k = k)
  clusters <- test$clustering
  df_copy$cluster <- clusters
  
  # pam_plot <- ggplot(data = df_copy) +
  #   geom_point(aes(x = mds.x, y = mds.y, color = cluster))
  # plots[[as.character(k)]] <- pam_plot
  
  # Now, let's run a regression
  df_shim <- df_copy
  df_shim$cluster <- clusters
  
  fit <- glm(coefficient ~ cluster, data = df_shim)
  c_value <- Cstat(fit)
  
  
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
  
  c_col <- append(c_col, c_value)
  vuong_p_col <- append(vuong_p, vuong_p_col)
}

results <- data.frame(
  k = k_range,
  c = c_col,
  vuong_p = vuong_p_col
)

plot_bar <- function(results, fill_column) {
  ggplot(data=results) +
    geom_bar(aes(
      x = k_range,
      y = eval(as.name(fill_column)),
    ), stat="identity") +
    labs(y = fill_column)
}

plot_bar(results, "c")
plot_bar(results, "vuong_p")

plot_grid(plotlist=plots, labels=k_range)
