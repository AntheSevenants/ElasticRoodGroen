library(dplyr)
library(magrittr)

# Read coefficients output
df <- read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")

df$order <- ifelse(df$coefficient < 0, "green",
                   ifelse(df$coefficient > 0, "red", NA))

cluster_stats <- function(grouping_col) {
  df %>%
    group_by_at(grouping_col) %>%
    summarise(avg = mean(coefficient),
              med = median(coefficient),
              count = length(coefficient),
              adjectiveness = mean(adjectiveness),
              redness = (table(order) / length(coefficient))[["red"]],
              greenness = (table(order) / length(coefficient))[["green"]]) 
}

cluster_stats("all.kmeans")
cluster_stats("non_zero.kmeans")
cluster_stats("outside_sd.kmeans")

# Frequency counts
table(df$all.kmeans)



