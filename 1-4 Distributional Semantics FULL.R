# In this script, we add distributional vectors
library(magrittr)

options(scipen=999)

# Use DistributionalSemanticsR scripts
source("DistributionalSemanticsR/VectorSpace.R")
source("DistributionalSemanticsR/DimensionReductionFactory.R")
source("ElasticToolsR/MetaFile.R")

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

dist_mat <- function(coords) {
  mat <- dist(coords, method="euclidean", diag=T, upper=T)
  mat <- as.matrix(mat)
  return(mat)
}

do_pca <- function(coords, dims=2) {
  clustering <- prcomp(coords, scale = FALSE)$x[,1:dims]
  
  return(clustering)
}

do_mds <- function(coords, dims=2) {
  
}

do_tsne <- function(coords, dims=2) {
  clustering <- Rtsne(coords, is_distance=TRUE, dims=dims)$Y %>% as.matrix()
  rownames(clustering) <- rownames(coords)
  
  return(clustering)
}

do_umap <- function(coords, dims=2) {
  umap(coords, n_components=dims)$layout
}

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

do_3d_plot <- function(mat, name) {
  colors <- get_color(df, mat)
  
  plot3d(mat[,1], mat[,2], mat[,3], col=colors)
  
  htmlwidgets::saveWidget(rglwidget(width = 520, height = 520), 
                          file = paste0("widgets/", name, ".html"),
                          libdir = "libs",
                          selfcontained = FALSE
  )
}

get_coords("non_zero") %>% dist_mat() %>% do_pca(3) %>% do_3d_plot("full_pca_non_zero")
get_coords("non_zero") %>% dist_mat() %>% do_tsne(3) %>% do_3d_plot("full_tsne_non_zero")
get_coords("non_zero") %>% do_umap(3) %>% do_3d_plot("full_umap_non_zero")

get_coords("all") %>% do_pca(2) %>% do_plot
get_coords("non_zero") %>% do_pca(2) %>% do_plot
get_coords("outside_sd") %>% do_pca(2) %>% do_plot

get_coords("all") %>% dist_mat() %>% do_tsne(2) %>% do_plot
get_coords("non_zero") %>% dist_mat() %>% do_tsne(2) %>% do_plot
get_coords("outside_sd") %>% dist_mat() %>% do_tsne(2) %>% do_plot

get_coords("all") %>% do_umap(2) %>% do_plot
get_coords("non_zero") %>% do_umap(2) %>% do_plot
get_coords("outside_sd") %>% do_umap(2) %>% do_plot



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
    k <- 13
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

df <- do_clustering(df, get_coords("all") %>% dist_mat() %>% do_pca(2), "kmeans", "all", "pca2d")
df <- do_clustering(df, get_coords("non_zero") %>% dist_mat() %>% do_pca(2), "kmeans", "non_zero", "pca2d")
df <- do_clustering(df, get_coords("outside_sd") %>% dist_mat() %>% do_pca(2), "kmeans", "outside_sd", "pca2d")

df <- do_clustering(df, get_coords("all") %>% dist_mat() %>% do_pca(10), "kmeans", "all", "pca10d")
df <- do_clustering(df, get_coords("non_zero") %>% dist_mat() %>% do_pca(10), "kmeans", "non_zero", "pca10d")
df <- do_clustering(df, get_coords("outside_sd") %>% dist_mat() %>% do_pca(10), "kmeans", "outside_sd", "pca10d")

df <- do_clustering(df, get_coords("all") %>% dist_mat() %>% do_tsne(2), "kmeans", "all", "tsne2d")
df <- do_clustering(df, get_coords("non_zero") %>% dist_mat() %>% do_tsne(2), "kmeans", "non_zero", "tsne2d")
df <- do_clustering(df, get_coords("outside_sd") %>% dist_mat() %>% do_tsne(2), "kmeans", "outside_sd", "tsne2d")

df <- do_clustering(df, get_coords("all") %>% dist_mat() %>% do_tsne(3), "kmeans", "all", "tsne3d")
df <- do_clustering(df, get_coords("non_zero") %>% dist_mat() %>% do_tsne(3), "kmeans", "non_zero", "tsne3d")
df <- do_clustering(df, get_coords("outside_sd") %>% dist_mat() %>% do_tsne(3), "kmeans", "outside_sd", "tsne3d")



# Correlation analysis
do_correlation_analysis <- function(df, exclude_features, coords) {
  df <- df[!(df$feature %in% exclude_features),]
  
  dimensions <- dim(coords)[2]
  
  print(df$feature %>% length)
  print(coords %>% rownames %>% length)
  
  p_values <- double()
  correlations <- double()
  
  for (index in 1:dimensions) {
    test <- cor.test(coords[,index], df$coefficient)
    
    p_values <- append(p_values, test$p.value)
    correlations <- append(correlations, test$estimate)
  }
  
  return_df <- data.frame(p = p_values,
                          correlation = correlations)
  
  return (return_df)
}

do_bonferroni <- function(df) {
  df$bon_sig <- df$p <= 0.05 / dim(df)[1]
  
  return(df)
}

# Trying to find correlation between dimensions and red-green coefficients
# I'm even applying Bonferroni correction, but too it still looks like fishing
# for spurious correlations (which is not in the scientific spirit)

correlation_all_df <- 
  do_correlation_analysis(df, no_vector_features, distributional_coords_all) %>%
  do_bonferroni()

correlation_non_zero_df <-
  do_correlation_analysis(df, c(zero_features, no_vector_features),
                          distributional_coords_non_zero) %>%
  do_bonferroni()

correlation_outside_sd_df <-
  do_correlation_analysis(df, c(within_sd_features, no_vector_features),
                          distributional_coords_outside_sd) %>%
  do_bonferroni()

# Add vector dimensions to the dataframe
dimensions <- dim(distributional_coords)[2]
# df$id <- 1:nrow(df)
for (index in 1:dimensions) {
  df[[paste0("dim_", index)]] <- distributional_coords[,index]
  
  # merger_df <- distributional_coords_all[,index] %>% as.data.frame()
  # features <- rownames(merger_df)
  # merger_df$feature <- features
  # colnames(merger_df) <- c(paste0("dim_", index), "feature")
  # 
  # df <- merge(df, merger_df, by="feature", all.x=TRUE)
}


# Reinstate the ordering
df <- df[order(df$id), ]
# Throw away id column
df$id <- NULL

dim(distributional_coords_all)
distributional_coords_all[,1]

write.csv(df, "output/RoodGroenAnthe_coefficients_infused_vectors.csv", 
          row.names=FALSE)
