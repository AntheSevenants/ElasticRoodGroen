# In this script, we add distributional vectors

# Use DistributionalSemanticsR scripts
source("DistributionalSemanticsR/VectorSpace.R")
source("DistributionalSemanticsR/DimensionReductionFactory.R")

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
distributional_coords_filtered <-
  distributional_coords[distributional_coords[,11] != 0.0,]

# 
# Dimensionality reduction
# 


# Apply dimensionality reduction
# We apply dimension reduction using three techniques:
# MDS, TSNE and UMAP

dim_reduce <- dimension_reduction_factory(distributional_coords_filtered)
coords.mds <- dim_reduce$do_mds()
coords.tsne <- dim_reduce$do_tsne()
coords.umap <- dim_reduce$do_umap()

# To re-insert the no-vector columns (they will get NA coordinates)
# https://stackoverflow.com/a/11562428
insert_row <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}


# Save the coordinates to the coefficients output
save_coordinates <- function(coords, technique, output) {
  # We go over each index in the no vector vector (confusing nomenclature)
  for (index in sort(no_vector_indices)) {
    coords <- insert_row(coords, list(NA, NA), index)
  }
  
  # Then, once the empty coordinates have been inserted,
  # we can attach the coordinates back to the original data frame
  output[paste0(technique, ".x")] <- coords[paste0(technique, "_x")]
  output[paste0(technique, ".y")] <- coords[paste0(technique, "_y")]
  
  return(output)
}

df <- save_coordinates(coords.mds, "mds", df)
df <- save_coordinates(coords.tsne, "tsne", df)
df <- save_coordinates(coords.umap, "umap", df)

#
# Coordinate regression
#

fill_coefficient <- function(df, model, coefficient_name) {
  df[[paste0(coefficient_name, "_coeff")]] <- 
    model$coefficients[[coefficient_name]]
  return(df)
}

fill_significance <- function(df, model, coefficient_name) {
  df[[paste0(coefficient_name, "_sig")]] <- 
    coef(summary(model))[coefficient_name, 4] <= 0.05
  return(df)
}

fill_regression_data <- function(df, model, coefficient_name) {
  df <- fill_coefficient(df, model, coefficient_name)
  df <- fill_significance(df, model, coefficient_name)
  
  return(df)
}

add_coordinate_regression_columns <- function(df, technique) {
  df_filtered <- df[df$coefficient != 0,]
  df_filtered$class <-
    as.factor(ifelse(df_filtered$coefficient < 0, "green", "red"))
  
  model <- glm(as.formula(paste0("class ~ ", technique, ".x + ", technique, ".y")),
               family=binomial(link='logit'),
               data=df_filtered)
  
  df <- fill_regression_data(df, model, paste0(technique, ".x"))
  df <- fill_regression_data(df, model, paste0(technique, ".y"))
  
  return(df)
}

df <- add_coordinate_regression_columns(df, "mds")
df <- add_coordinate_regression_columns(df, "tsne")
df <- add_coordinate_regression_columns(df, "umap")

write.csv(df, "output/RoodGroenAnthe_coefficients_infused_vectors.csv", 
          row.names=FALSE)
