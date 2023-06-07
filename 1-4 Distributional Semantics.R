# In this script, we add distributional vectors

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
                  read.table(gzfile('data/dutch.gz'), 
                             sep=' ', row.names=1, skip = 3)))

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
  

# 
# Dimensionality reduction
# 


# Apply dimensionality reduction
# We apply dimension reduction using three techniques:
# MDS, TSNE and UMAP

# Dimension reduction on ALL coordinates
dim_reduce_all <- dimension_reduction_factory(distributional_coords_all)
coords_all.mds <- dim_reduce_all$do_mds()
coords_all.tsne <- dim_reduce_all$do_tsne()
coords_all.umap <- dim_reduce_all$do_umap()

# Dimension reduction on coordinates of words with non-zero coefficients
dim_reduce_non_zero <- 
  dimension_reduction_factory(distributional_coords_non_zero)
coords_non_zero.mds <- dim_reduce_non_zero$do_mds()
coords_non_zero.tsne <- dim_reduce_non_zero$do_tsne()
coords_non_zero.umap <- dim_reduce_non_zero$do_umap()

# Dimension reduction on coordinates of words with coefficients outside
# the standard deviation
dim_reduce_outside_sd <- 
  dimension_reduction_factory(distributional_coords_outside_sd)
coords_outside_sd.mds <- dim_reduce_outside_sd$do_mds()
coords_outside_sd.tsne <- dim_reduce_outside_sd$do_tsne()
coords_outside_sd.umap <- dim_reduce_outside_sd$do_umap()

# To re-insert the no-vector columns (they will get NA coordinates)
# adapted to avoid NA insertions
# https://stackoverflow.com/a/11562428
insert_row <- function(existingDF, newrow, r) {
  if (r == nrow(existingDF) + 1) {
    existingDF[nrow(existingDF) + 1,] <- newrow
    return(existingDF)
  }
  
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

# Save the coordinates to the coefficients output
save_coordinates <- function(coords, technique, suffix, output, excluded_indices) {
  # We go over each index in the no vector vector (confusing nomenclature)
  for (index in sort(excluded_indices)) {
    coords <- insert_row(coords, list(NA, NA), index)
  }
  
  # Then, once the empty coordinates have been inserted,
  # we can attach the coordinates back to the original data frame
  output[paste0(technique, ".", suffix, ".x")] <- coords[paste0(technique, "_x")]
  output[paste0(technique, ".", suffix, ".y")] <- coords[paste0(technique, "_y")]
  
  return(output)
}

# Saving: all words
df <- save_coordinates(coords_all.mds, "mds", "all", df, no_vector_indices)
df <- save_coordinates(coords_all.tsne, "tsne", "all", df, no_vector_indices)
df <- save_coordinates(coords_all.umap, "umap", "all", df, no_vector_indices)

# Saving: non-zero words
df <- save_coordinates(coords_non_zero.mds, "mds", "non_zero", df, zero_indices)
df <- save_coordinates(coords_non_zero.tsne, "tsne", "non_zero", df, zero_indices)
df <- save_coordinates(coords_non_zero.umap, "umap", "non_zero", df, zero_indices)

# Saving: outside sd words
df <- save_coordinates(coords_outside_sd.mds, "mds", "outside_sd", df, within_sd_indices)
df <- save_coordinates(coords_outside_sd.tsne, "tsne", "outside_sd", df, within_sd_indices)
df <- save_coordinates(coords_outside_sd.umap, "umap", "outside_sd", df, within_sd_indices)

# Export the model meta again
write.csv(model_meta$as.data.frame(), "output/model_meta.csv", row.names=FALSE)

write.csv(df, "output/RoodGroenAnthe_coefficients_infused_vectors.csv", 
          row.names=FALSE)
