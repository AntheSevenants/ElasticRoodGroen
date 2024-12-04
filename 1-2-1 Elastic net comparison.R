# That's so random!
set.seed(2204355)

# ElasticTools imports
# Make sure the git submodules have been pulled!
source("ElasticToolsR/Dataset.R")
source("ElasticToolsR/ElasticNet.R")
source("ElasticToolsR/MetaFile.R")

library(pROC)

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
data_matrix <- ds$as_matrix()

# Elastic Net regression itself
net <- elastic_net(ds=ds)
gc()
fit <- net$do_lasso_regression()

predicted <- predict(fit, type="response", data_matrix)
auc_with_lexicon <- auc(df$order, predicted)
auc_with_lexicon <- as.numeric(auc_with_lexicon)

# Create an ElasticTools dataset, WITHOUT PARTICIPLE
ds_no_lexicon <- dataset(df=df,
              response_variable_column="order",
              to_binary_columns=c("auxiliary_lemma"),
              other_columns=c("country", "separable", "adjectiveness", 
                              "priming_rate", "logfreq", "edited",
                              "log_middle_field_length"))
data_matrix_no_lexicon <- ds_no_lexicon$as_matrix()

# Elastic Net regression itself, WITHOUT PARTICIPLE
net_no_lexicon <- elastic_net(ds=ds_no_lexicon)
gc()
fit_no_lexicon <- net_no_lexicon$do_lasso_regression()

predicted_no_lexicon <- predict(fit_no_lexicon, type="response", data_matrix_no_lexicon)
auc_no_lexicon <- auc(df$order, predicted_no_lexicon)
auc_no_lexicon <- as.numeric(auc_no_lexicon)

auc_df <- data.frame(model_type = c("with_lexicon", "without_lexicon"),
                     auc = c(auc_with_lexicon, auc_no_lexicon))
write.csv(auc_df, "auc.csv", row.names=F)
