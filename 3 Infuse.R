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
# Aggregate over duplicates (sigh)
df_concreteness <- aggregate(df_concreteness$Concrete_m,
                             by=list(stimulus=df_concreteness$stimulus),
                             data=df_concreteness,
                             FUN=mean)
# Restore column names
colnames(df_concreteness) <- c("stimulus", "Concrete_m")

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

# Moorset al. (2013).
# Norms of Valence, Arousal, Dominance, and Age of Acquisition for 4,300 Dutch Words.
# Behavior Research Methods 45 (1): 169–77.
df_norms <- read.csv("data/ValenceArousalDominance.csv")

# Adjectiveness
# Sevenants, A. Adjectiveness dataset for past participles in Dutch
df_adjectiveness <- read.csv("data/adjectiveness.csv")

#
# Introduce Brysbaert et al. data into our dataset
#

# Concreteness
df <- merge(x=df, y=df_concreteness[,c("stimulus", "Concrete_m")],
            by.x="lemma", by.y="stimulus", all.x=TRUE)
names(df)[names(df) == 'Concrete_m'] <- 'concreteness'

# Age of acquisition
df <- merge(x=df, y=df_acquisition[,c("word", "aoa")],
            by.x="lemma", by.y="word", all.x=TRUE)

# Subtlex frequency
df <- merge(x=df, y=df_subtlex[,c("Word", "FREQcount")],
            by.x="lemma", by.y="Word", all.x=TRUE)
names(df)[names(df) == 'FREQcount'] <- 'freq'
# Also log-transform
df$logfreq <- log10(df$freq)

# Norms
df <- merge(x=df, y=df_norms[,c("valence", "arousal", "dominance", "word")],
            by.x="lemma", by.y="word", all.x=TRUE)

# Adjectiveness
df <- merge(x=df, y=df_adjectiveness[,c("participle", "adjectiveness")],
            by.x="feature", by.y="participle", all.x=TRUE)


write.csv(df, "output/RoodGroenAnthe_coefficients_infused.csv", row.names=FALSE)
