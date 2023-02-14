# In this script, we'll build some clusters

# Use DistributionalSemanticsR clustering script
source("DistributionalSemanticsR/Clustering.R")

# Read coefficients output
df <- read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")

# Define the clustering function
do_clustering <- function(df,
                          clustering_algorithm="kmeans",
                          base,
                          mode="all") {
  df_copy <- df
  
  # Mode can be "all", "zero", "non-zero"

  # Needed to keep the ordering
  df$id  <- 1:nrow(df) 
  
  if (mode == "non-zero") {
    # Remove all zero coefficient features
    df_copy <- df_copy[df_copy["coefficient"] != 0,]
  } else if (mode == "zero") {
    # Keep only zero coefficient features
    df_copy <- df_copy[df_copy["coefficient"] == 0,]
  }
  # Else, just keep all
  
  # Remove coefficient column
  df_copy$coefficient <- NULL
  
  # Only keep the required coordinates
  pass_coords <- df_copy[, c(paste0(base, ".x"), paste0(base, ".y")), drop=FALSE]
  
  return_dataframe <- data.frame(feature = df_copy$feature)
  
  # Only give coordinates to the clustering algorithm
  clustering_ <- clustering(base, pass_coords)
  
  if (clustering_algorithm == "kmeans") {
    clusters <- clustering_$do_k_means_batch(1:20)
  } else if (clustering_algorithm == "dbscan") {
    clusters <- clustering_$do_dbscan_batch(2:10)
  }
  
  k_index = 1
  for (k_value in clusters$k_range) {
    assigned_cluster <- clusters$k_results[[k_index]]
    return_dataframe[[paste0(clusters$k_names[[k_index]], "_", mode)]] <- 
      ifelse(is.na(assigned_cluster), NA, paste0("cluster ", assigned_cluster))
    
    k_index = k_index + 1
  }
  
  coefficients_go <- merge(df, return_dataframe,
                           by="feature", all.x=TRUE)
  
  # Reinstate the ordering
  coefficients_go <- coefficients_go[order(coefficients_go$id), ]
  # Throw away id column
  coefficients_go$id <- NULL
  
  return(coefficients_go)  
}

for (mode in c("all", "non-zero", "zero")) {
  # - kmeans
  df <- do_clustering(df, "kmeans", "mds", mode)
  df <- do_clustering(df, "kmeans", "tsne", mode)
  df <- do_clustering(df, "kmeans", "umap", mode)
  
  # - dbscan
  df <- do_clustering(df, "dbscan", "mds", mode)
  df <- do_clustering(df, "dbscan", "tsne", mode)
  df <- do_clustering(df, "dbscan", "umap", mode)
}

write.csv(df, "output/RoodGroenAnthe_coefficients_infused_vectors_clusters.csv", 
          row.names=FALSE)

