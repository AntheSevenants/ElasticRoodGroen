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
vector_space_ <- vector_space(space=as.matrix(
                  read.table('data/embeddings.txt', 
                             sep=' ', row.names=1, skip = 1)))

# Get the distributional coordinates
distributional_coords <- 
  vector_space_$get_distributional_values(df$feature)

# Stop! Not all features will have distributional values
# Especially considering extra features like region etc.
# We check where the coordinates are all zeroes
no_vector_features <- df[distributional_coords[,11] == 0.0,]$feature
no_vector_indices <- which(distributional_coords[,11] == 0.0)

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

# Define the clustering function
do_clustering <- function(df,
                          clustering_algorithm="kmeans",
                          mode="all") {
  # Mode can be "all", "non_zero", "outside_sd"
  
  # Needed to keep the ordering
  df$id  <- 1:nrow(df)
  
  if (mode == "non_zero") {
    coords <- distributional_coords_non_zero
  } else if (mode == "outside_sd") {
    # Keep only zero coefficient features
    coords <- distributional_coords_outside_sd
  } else if (mode == "all") {
    # Else, just keep all
    coords <- distributional_coords_all
  }
  
  if (clustering_algorithm == "kmeans") {
    # "all" mode should cluster into three categories, other modes two
    k <- ifelse(mode == "all", 3, 2)
    clustering <- kmeans(coords, centers=k, iter.max=25)
    
    # Create a dataframe for merging
    return_dataframe <- data.frame(feature = names(clustering$cluster))
    return_dataframe[[paste0(mode, ".", clustering_algorithm)]] = 
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

df <- do_clustering(df, "kmeans", "all")
df <- do_clustering(df, "kmeans", "non_zero")
df <- do_clustering(df, "kmeans", "outside_sd")

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

correlation_all_df <- 
  do_correlation_analysis(df, no_vector_features, distributional_coords_all)

correlation_non_zero_df <-
  do_correlation_analysis(df, c(zero_features, no_vector_features),
                          distributional_coords_non_zero)

correlation_outside_sd_df <-
  do_correlation_analysis(df, c(within_sd_features, no_vector_features),
                          distributional_coords_outside_sd)

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
