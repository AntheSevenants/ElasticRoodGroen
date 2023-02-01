# That's so random!
set.seed(2204355)

# ElasticTools imports
# Make sure the git submodules have been pulled!
source("ElasticToolsR/Dataset.R")
source("ElasticToolsR/ElasticNet.R")

# Read sampled dataframe
df <- read.csv("output/RoodGroenAnthe_sampled.csv")

# Coerce the response variable column to a factor
df$order <- factor(df$order, levels=c("green", "red"))
# Coerce the lexical influence column to a factor
df$participle <- as.factor(df$participle)

# Create an ElasticTools dataset
ds <- dataset(df=df,
              response_variable_column="order",
              to_binary_column="participle")

# Convert the dataset into a feature matrix
feature_matrix <- ds$as_matrix()

# Get the list of features
# In our case, this is the list of participles
feature_list <- ds$as_feature_list()

# Elastic Net regression itself
net <- elastic_net(ds=ds,
                   feature_matrix=feature_matrix)
output <- net$do_cross_validation(k=10)

output$results

# Get the lowest loss from the results
lowest_loss_row <- output$results[which.min(output$results$loss),]
lowest_loss_row

# Extract the coefficients from the model with the lowest loss
# Attach features to the coefficients
coefficients_with_labels <- net$attach_coefficients(
  output$fits[[lowest_loss_row[["X_id"]]]])

# Export
write.csv(coefficients_with_labels, "output/RoodGroenAnthe_coefficients.csv")
