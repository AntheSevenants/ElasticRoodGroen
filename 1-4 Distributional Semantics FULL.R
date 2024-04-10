# In this script, we add distributional vectors
library(magrittr)
library(useful)

options(scipen=999)

# Use DistributionalSemanticsR scripts
source("DistributionalSemanticsR/VectorSpace.R")

# Read coefficients output
df <- read.csv("output/RoodGroenAnthe_coefficients_infused.csv")

#
# Getting distributional vectors
#

# Load a vector space
embeddings <- as.matrix(read.table('data/embeddings_lemma.txt',
                                   sep=' ', row.names=1, skip = 1))

# Check for leading space NAs
if (all(is.na(embeddings[,ncol(embeddings)]))) {
  embeddings <- embeddings[,-ncol(embeddings)]
}

# Define vector space
vector_space_ <- vector_space(space=embeddings)

# Get the distributional coordinates
distributional_coords <- 
  vector_space_$get_distributional_values(df$lemma)

rownames(distributional_coords) <- df$feature

# Stop! Not all features will have distributional values
# Especially considering extra features like region etc.
# We check where the coordinates are all zeroes
non_vector_bin <- sapply(1:nrow(distributional_coords), function(i) {
  all(distributional_coords[i,] == 0)
})
no_vector_features <- df[non_vector_bin,]$feature
no_vector_indices <- which(non_vector_bin)

# Then, check which features have a zero coefficient
zero_features <- df[df$coefficient == 0,]$feature
zero_indices <- which(df$coefficient == 0)
# Combine with no vector indices
zero_indices <- unique(c(zero_indices, no_vector_indices)) 

# Then, check which features fall within the standard deviation
within_sd_features <- df[abs(df$coefficient) < sd(df$coefficient),]$feature
within_sd_indices <- which(abs(df$coefficient) < sd(df$coefficient))
# Combine with no vector indices
within_sd_indices <- unique(c(within_sd_indices, no_vector_indices)) 

# This set of distributional coordinates has ALL words
distributional_coords_all <-
  distributional_coords[-no_vector_indices,]
# This set of distributional coordinates does not have zero-coefficient words
distributional_coords_non_zero <-
  distributional_coords[-zero_indices,]

# This set of distributional coordinates does not have words with coefficients
# inside the standard deviation
distributional_coords_outside_sd <-
  distributional_coords[-within_sd_indices,]

get_color <- function(df, mat) {
  origin <- data.frame(feature = rownames(mat))
  
  merged_df <- merge(origin, df, by="feature", all.x=TRUE)
  
  merged_df$order <- ifelse(merged_df$coefficient < 0, "green",
                     ifelse(merged_df$coefficient > 0, "red", "grey"))
  
  return(merged_df$order)
}

do_plot <- function(mat, colors=NA) {
  if (is.na(colors)) {
    colors <- get_color(df, mat)
  }
  
  plot(mat, col = colors)
}

get_coords <- function(mode) {
  # Mode can be "all", "non_zero", "outside_sd"

  if (mode == "non_zero") {
    coords <- distributional_coords_non_zero
  } else if (mode == "outside_sd") {
    # Keep only zero coefficient features
    coords <- distributional_coords_outside_sd
  } else if (mode == "all") {
    # Else, just keep all
    coords <- distributional_coords_all
  }
  
  return(coords)
}

n_cluster_search <- FitKMeans(get_coords("all"), max.clusters=20,
                              nstart=25, seed=2204355, iter.max=100)
n_cluster_search

# We look for the cluster count where adding a cluster doesn't improve
# the model anymore
# So, I filter for the first non-increasing cluster (AddCluster = FALSE)
n_cluster <- n_cluster_search %>% filter(!AddCluster) %>% first %>% .$Cluster

PlotHartigan(n_cluster_search)

# Define the clustering function
do_clustering <- function(df,
                          coords,
                          clustering_algorithm="kmeans",
                          mode="all",
                          extra="") {
  # Needed to keep the ordering
  df$id  <- 1:nrow(df)
  
  if (clustering_algorithm == "kmeans") {
    # "all" mode should cluster into three categories, other modes two
    #k <- ifelse(mode == "all", 3, 2)
    k <- n_cluster
    clustering <- kmeans(coords, centers=k, iter.max=100)
    
    # Create a dataframe for merging
    return_dataframe <- data.frame(feature = names(clustering$cluster))
    return_dataframe[[paste0(mode, ".", clustering_algorithm, ".", extra)]] = 
      as.numeric(unname(clustering$cluster))
    
  } else if (clustering_algorithm == "dbscan") {
    print("TODO")
  }
  
  coefficients_go <- merge(df, return_dataframe,
                           by="feature", all.x=TRUE)
  
  # Reinstate the ordering
  coefficients_go <- coefficients_go[order(coefficients_go$id), ]
  # Throw away id column
  coefficients_go$id <- NULL
  
  return(coefficients_go)  
}

df <- do_clustering(df, get_coords("all"), "kmeans", "all", "full")
df <- do_clustering(df, get_coords("non_zero"), "kmeans", "non_zero", "full")
df <- do_clustering(df, get_coords("outside_sd"), "kmeans", "outside_sd", "full")

write.csv(df, "output/RoodGroenAnthe_coefficients_infused_vectors.csv", 
          row.names=FALSE)
