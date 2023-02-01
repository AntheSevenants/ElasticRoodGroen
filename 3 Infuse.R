# In this script, we add extra information to the coefficients output
# E.g. frequency information and so on

# Read coefficients output
df <- read.csv("output/RoodGroenAnthe_coefficients.csv")

# First, let's handle our separable verbs
# In the Alpino corpus, the separable parts are split using '_'
df$lemma_original <- df$lemma # Save original column
df$lemma <- gsub("_", "", df$lemma) # Replace _
# Check using _ whether verb is separable or not
df$separable <- grepl("_", df$lemma_original, fixed=TRUE)

# Brysbaert, M., Warriner, A.B., & Kuperman, V. (2014)
# Dataset containing concreteness information
df_concreteness <- read.csv("data/Brysbaert_concreteness.csv", sep=";")
df_concreteness$Concrete_m <- 
  as.numeric(gsub(",", ".", df_concreteness$Concrete_m))

# Brysbaert, M., Stevens, M., De Deyne, S., Voorspoels, W., & Storms, G. (2014). 
# Norms of age of acquisition and concreteness for 30,000 Dutch words. 
# Acta Psychologica, 150, 80-84. 
# Dataset containing age of acquisition information
df_acquisition <- read.csv("data/Brysbaert_age_of_acquisition.csv", sep=";")
df_acquisition$aoa <- as.numeric(gsub(",", ".", df_acquisition$aoa))

# SUBTLEX-NL
# Keuleers, E., Brysbaert, M. & New, B. (2010). 
# SUBTLEX-NL: A new frequency measure for Dutch words based on film subtitles. 
# Behavior Research Methods, 42(3), 643-650.
df_subtlex <- read.delim("data/SUBTLEX.txt", sep="\t")

# Function to merge data between dataframes
get_info <- function(dest_df, source_df, by_column, info_column,
                     source_column="lemma") {
  apply(dest_df, 1, function(row) {
    row <- source_df[source_df[by_column] == row[[source_column]],][1,]
    return(as.numeric(row[[info_column]]))
  })
}

# Introduce Brysbaert et al. data into our dataset
df$concreteness <- get_info(df, df_concreteness, "stimulus", "Concrete_m")
df$aoa <- get_info(df, df_acquisition, "word", "aoa")
df$freq <- get_info(df, df_subtlex, "Word", "FREQcount", source_column="feature")
df$logfreq <- log10(df$freq)

# TODO: adjectiveness?

write.csv(df, "output/RoodGroenAnthe_coefficients_infused.csv", row.names=FALSE)
