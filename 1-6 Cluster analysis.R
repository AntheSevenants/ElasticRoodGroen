library(dplyr)
library(magrittr)
library(forcats)
library(tidyr)
library(ggplot2)

# Read coefficients output
cl_df <- read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")
sem_df <- read.csv("output/RoodGroenAnthe_coefficients_semantics_full.csv")

cl_df$order <- ifelse(cl_df$coefficient < 0, "green",
                      ifelse(cl_df$coefficient > 0, "red", NA))

cl_df <- merge(cl_df, sem_df[c(
  "feature",
  "valency",
  "control",
  "attributive",
  "spatial",
  "cognitive",
  "dynamic",
  "ValenceVsNeutral"
)], by="feature", all.x=T)

cl_df$valenced_numeric <- as.numeric(factor(cl_df$ValenceVsNeutral, levels=c("neutral", "valenced"))) - 1
cl_df$cognitive_numeric <- as.numeric(factor(cl_df$cognitive, levels=c("False", "True"))) - 1

cluster_stats <- function(grouping_col) {
  cl_df %>%
    group_by_at(grouping_col) %>%
    summarise(avg = mean(coefficient),
              med = median(coefficient),
              count = length(coefficient),
              adjectiveness = mean(adjectiveness),
              arousal_num = which(!is.na(arousal)) %>% length,
              arousal = mean(arousal, na.rm = T),
              cognitive_num = which(!is.na(cognitive)) %>% length,
              cognitive = mean(cognitive_numeric, na.rm = T),
              valence = mean(valenced_numeric, na.rm = T),
              redness = (table(order) / length(coefficient))[["red"]],
              greenness = (table(order) / length(coefficient))[["green"]],
              t.test = t.test(coefficient)$p.value <= 0.05) %>%
    filter(!is.na(!! sym(grouping_col)))
}

cluster_plot <- function(cluster_stats) {
  # Order by redness
  cluster_stats <- cluster_stats[order(cluster_stats$redness),]
  cluster_stats$new_id <- 1:nrow(cluster_stats)
  # Remember what order the clusters are in
  cluster_order <- cluster_stats[[1]]
  # Now, create a row for each redness/greenness value
  cluster_stats <- cluster_stats %>% pivot_longer(redness:greenness)
  # Re-instate the cluster order
  cluster_stats[[1]] <- factor(cluster_stats[[1]], levels=cluster_order)
  # Make new IDs a factor, otherwise R will complain
  cluster_stats$new_id <- as.factor(cluster_stats$new_id)
  cluster_stats$percent_value <- percent(cluster_stats$value, accuracy = 1)
  # Extract cluster ids
  cluster_ids <- cluster_stats[[1]]
  
  #View(cluster_stats)
  
  ggplot(cluster_stats,
         aes(
           fill = name,
           y = value,
           x = new_id,
           linetype = !t.test,
           label = percent_value
         )) +
    geom_bar(position = "fill",
             stat = "identity",
             color = "black") +
    geom_text(
      aes(
        label = percent_value,
        # x = cluster_ids,
        y = ifelse(name == "redness", .05, .9),
        fontface = ifelse(name == "redness", "bold", "plain")
      ),
      show.legend = F,
      position = position_stack(reverse=T),
      #y=ifelse(cluster_stats$name == "redness", 0.05, 0.95) 
    ) +
    scale_linetype_manual(labels=c("yes", "no"), values=c("solid", "dashed")) +
    scale_fill_manual(labels=c("green verbs", "red verbs"), values = c("green", "red")) +
    scale_color_manual(values = c("black", "white")) +
    xlab("Cluster ID") +
    ylab("Percentage") +
    scale_y_continuous(labels = percent) +
    guides(linetype = guide_legend(title="Dominant colour"), fill= guide_legend(title="Verb preferences"))
}

cluster_stats("non_zero.kmeans.full") %>% lm(greenness ~ valence + cognitive, data=.) %>% summary

cluster_stats("non_zero.kmeans.full") %>% cor.test(.$greenness, .$valence, data=.)
cluster_stats("non_zero.kmeans.full") %>% cor.test(.$greenness, .$cognitive, data=.)

#cluster_stats("non_zero.kmeans.full") 

### FULL

#cluster_stats("all.kmeans.full") %>% cluster_plot()
#cluster_stats("non_zero.kmeans.full") %>% cluster_plot()
#cluster_stats("outside_sd.kmeans.full")

# Frequency counts
#table(df$all.kmeans)



