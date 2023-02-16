library(stringdist)

# In this script, we will work with the Levenshtein distance
# No distributional vectors, but dimension reduction is still necessary

source("DistributionalSemanticsR/DimensionReductionFactory.R")

# Read coefficients output
df <- read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")

distance_matrix <- stringdistmatrix(df$feature, df$feature, method="lv")

# Apply dimensionality reduction
# We apply dimension reduction using three techniques:
# MDS, TSNE and UMAP

dim_reduce <- dimension_reduction_factory(NULL, distance_matrix)
coords.mds <- dim_reduce$do_mds()
coords.tsne <- dim_reduce$do_tsne()

# Save the coordinates to the coefficients output
save_coordinates <- function(coords, technique, output) {
  output[paste0(technique, ".levenshtein.x")] <- coords[paste0(technique, "_x")]
  output[paste0(technique, ".levenshtein.y")] <- coords[paste0(technique, "_y")]
  
  return(output)
}

df <- save_coordinates(coords.mds, "mds", df)
df <- save_coordinates(coords.tsne, "tsne", df)

write.csv(df, "output/RoodGroenAnthe_coefficients_infused_vectors_levenshtein.csv", 
          row.names=FALSE)
