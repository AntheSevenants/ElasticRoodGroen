# That's so random!
set.seed(2204355)

# ElasticTools imports
# Make sure the git submodules have been pulled!
source("ElasticToolsR/Dataset.R")
source("ElasticToolsR/ElasticNet.R")

# Read sampled dataframe
df <- read.csv("RoodGroenAnthe_sampled.csv")

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

# Test: ridge regression
fit <- net$do_ridge_regression()

# Attach features to the coefficients
coefficients_with_labels <- net$attach_coefficients(fit)

# Export
write.csv(coefficients_with_labels, "RoodGroenAnthe_ridge.csv")
