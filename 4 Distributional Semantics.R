# In this script, we add distributional vectors

# Use DistributionalSemanticsR scripts
source("DistributionalSemanticsR/VectorSpace.R")
source("DistributionalSemanticsR/DimensionReductionFactory.R")
source("DistributionalSemanticsR/Clustering.R")

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

# 
# Dimensionality reduction
# 


# Apply dimensionality reduction
# We apply dimension reduction using three techniques:
# MDS, TSNE and UMAP

dim_reduce <- dimension_reduction_factory(distributional_coords)
coords.mds <- dim_reduce$do_mds()
coords.tsne <- dim_reduce$do_tsne()
coords.umap <- dim_reduce$do_umap()

# Save the coordinates to the coefficients output
save_coordinates <- function(coords, technique, output) {
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

df_filtered <- df[df$coefficient != 0,]
df_filtered$class <-
  as.factor(ifelse(df_filtered$coefficient < 0, "green", "red"))

# TODO: this needs tidying
model_mds <- glm(class ~ mds.x + mds.y,
                 family=binomial(link='logit'),
                 data=df_filtered)
model_tsne <- glm(class ~ tsne.x + tsne.y,
                  family=binomial(link='logit'),
                  data=df_filtered)
model_umap <- glm(class ~ umap.x + umap.y,
                  family=binomial(link='logit'),
                  data=df_filtered)

df <- fill_regression_data(df, model_mds, "mds.x")
df <- fill_regression_data(df, model_mds, "mds.y")
df <- fill_regression_data(df, model_tsne, "tsne.x")
df <- fill_regression_data(df, model_tsne, "tsne.y")
df <- fill_regression_data(df, model_umap, "umap.x")
df <- fill_regression_data(df, model_umap, "umap.y")

# TODO: clustering (for another time)

write.csv(df, "output/RoodGroenAnthe_coefficients_infused_vectors.csv", 
          row.names=FALSE)
