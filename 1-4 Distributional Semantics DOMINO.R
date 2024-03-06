# In this script, we add distributional vectors
library(magrittr)

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

get_color <- function(df, mat) {
  origin <- data.frame(feature = rownames(mat))
  
  merged_df <- merge(origin, df, by="feature", all.x=TRUE)
  
  merged_df$order <- ifelse(merged_df$coefficient < 0, "green",
                            ifelse(merged_df$coefficient > 0, "red", "grey"))
  
  return(merged_df$order)
}

set.seed(2204355)

plot.kmeans <- function (x, data = NULL, class = NULL, size = 2, legend.position = c("right", 
                                                                      "bottom", "left", "top", "none"), title = "K-Means Results", 
          xlab = "Principal Component 1", ylab = "Principal Component 2", 
          ...) 
{
  toPlot <- fortify(model = x, data = data)
  legend.position <- match.arg(legend.position)
  if (!is.null(class)) 
    class <- factor(class)
  ggplot(toPlot, aes(x = .x, y = .y)) + 
    geom_point(aes(color = class), size = size) + 
    geom_mark_hull(aes(group = .Cluster), expand=0.01, radius=0.01, concavity=0.1) +
    scale_color_manual(values = c("green", "red")) + 
    theme(legend.position = legend.position) + 
    labs(title = title, x = xlab, y = ylab)
}

library(useful)

wineTrain <- get_coords("non_zero")
classes <- get_color(df, wineTrain)
wineK3 <- kmeans(x=wineTrain, centers=2, nstart=25)
wineK3$size

plot.kmeans(wineK3, data=wineTrain, class=classes)

wineBest <- FitKMeans(wineTrain, max.clusters=20, nstart=25, seed=2204355)
wineBest

PlotHartigan(wineBest)

table(classes, wineK3$cluster)

plot(table(classes, wineK3$cluster),
     main="Confusion matrix for red-green clustering",
     xlab="Order", ylab="Cluster")

library(cluster)

source("snippets/clusgap.R")

theGap <- clusGap(wineTrain, FUNcluster=pam, K.max=20, do_parallel=TRUE)
gapDF <- as.data.frame(theGap$Tab)
gapDF

