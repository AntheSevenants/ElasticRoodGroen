library(dplyr)
library(magrittr)
library(forcats)
library(tidyr)
library(ggplot2)

# Read coefficients output
cl_df <- read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")

cl_df$order <- ifelse(cl_df$coefficient < 0, "green",
                      ifelse(cl_df$coefficient > 0, "red", NA))

cluster_stats <- function(grouping_col) {
  cl_df %>%
    group_by_at(grouping_col) %>%
    summarise(avg = mean(coefficient),
              med = median(coefficient),
              count = length(coefficient),
              adjectiveness = mean(adjectiveness),
              arousal_num = which(!is.na(arousal)) %>% length,
              arousal = mean(arousal, na.rm = T),
              redness = (table(order) / length(coefficient))[["red"]],
              greenness = (table(order) / length(coefficient))[["green"]],
              t.test = t.test(coefficient)$p.value <= 0.05) %>%
    filter(!is.na(!! sym(grouping_col)))
}

cluster_plot <- function(cluster_stats) {
  # Order by greenness
  cluster_stats <- cluster_stats[order(cluster_stats$greenness),]
  # Remember what order the clusters are in
  cluster_order <- cluster_stats[[1]]
  # Now, create a row for each redness/greenness value
  cluster_stats <- cluster_stats %>% pivot_longer(redness:greenness)
  # Re-instate the cluster order
  cluster_stats[[1]] <- factor(cluster_stats[[1]], levels=cluster_order)
  # Extract cluster ids
  cluster_ids <- cluster_stats[[1]]
  
  ggplot(cluster_stats, aes(fill=name, y=value, x=cluster_ids,
                            linetype=!t.test)) +
    geom_bar(position="fill", stat="identity", color="black") +
    scale_fill_manual(values=c("green", "red"))
}

#cluster_stats("non_zero.kmeans.full") 

### FULL

#cluster_stats("all.kmeans.full") %>% cluster_plot()
#cluster_stats("non_zero.kmeans.full") %>% cluster_plot()
#cluster_stats("outside_sd.kmeans.full")

# Frequency counts
#table(df$all.kmeans)



