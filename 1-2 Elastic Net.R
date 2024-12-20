# That's so random!
set.seed(2204355)

# ElasticTools imports
# Make sure the git submodules have been pulled!
source("ElasticToolsR/Dataset.R")
source("ElasticToolsR/ElasticNet.R")
source("ElasticToolsR/MetaFile.R")

# Read sampled dataframe
df <- read.csv("output/RoodGroenAnthe_sampled.csv")

# Coerce the response variable column to a factor
df$order <- factor(df$order, levels=c("green", "red"))
# Coerce the lexical influence column to a factor
df$participle <- as.factor(df$participle)

# Coerce the country column to a factor
df$country <- as.factor(df$country)

# Make sure auxiliary lemma is recognised as a non-verbal variable
df$auxiliary_lemma <- paste0("_is_aux_", df$auxiliary_lemma)
# Coerce the auxiliary lemma column to a factor
df$auxiliary_lemma <- as.factor(df$auxiliary_lemma)

# Create an ElasticTools dataset
ds <- dataset(df=df,
              response_variable_column="order",
              to_binary_columns=c("participle", "auxiliary_lemma"),
              other_columns=c("country", "separable", "adjectiveness", 
                              "priming_rate", "logfreq", "edited",
                              "log_middle_field_length"))

# Get the list of features
# In our case, this is the list of participles
feature_list <- ds$as_feature_list()

# Elastic Net regression itself
net <- elastic_net(ds=ds)
gc()
output <- net$do_elastic_net_regression_auto_alpha(k=5)

output$results

# Get the lowest loss from the results
lowest_loss_row <- output$results[which.min(output$results$loss),]
lowest_loss_row

# Extract the coefficients from the model with the lowest loss
# Attach features to the coefficients
coefficients_with_labels <- net$attach_coefficients(
  output$fits[[lowest_loss_row[["X_id"]]]])

# Re-introduce lemma information
coefficients_with_labels$lemma <- 
  net$get_coupled_information(coefficients_with_labels, "participle", "participle_lemma")

# Export
write.csv(coefficients_with_labels, "output/RoodGroenAnthe_coefficients.csv",
          row.names=FALSE)

#
# Export model information
#

model_meta <- meta.file()

for (attribute in colnames(lowest_loss_row)) {
  if (attribute == "X_id") {
    next
  }
  
  model_meta$add_model_information(attribute, lowest_loss_row[[attribute]])
}

write.csv(model_meta$as.data.frame(), "output/model_meta.csv", row.names=FALSE)

