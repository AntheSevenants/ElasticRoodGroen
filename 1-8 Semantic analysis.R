# Load dataset with dominant sense for each feature
semcor <-
  read.csv("output/RoodGroenAnthe_coefficients_semantics.csv")
# Load information for each of those dominant senses from Cornetto
cornetto <- read.csv("output/cornetto_info.csv")
# Load the Speed & Brysbaert (2023) dataset for polarity judgements
speed <- read.csv("data/SpeedBrysbaertEmotionNorms.tsv", sep = "\t")
speed$ValenceCategory <-
  factor(speed$ValenceCategory,
         levels = c("neutral", "positive", "negative"))

# Add Cornetto information to original dataset
df <-
  merge(semcor, cornetto, by.x = "dominant_sense", by.y = "sense")
# Also add the Speed info
df <- merge(df, speed, by.x = "lemma", by.y = "Word")
# Also the Cornetto dataset is really wonky,
# there is missing data even though it can just be inferred
df$transitivity <-
  ifelse(df$valency == "mono", "intransitive", "transitive")

write.csv(df, "output/RoodGroenAnthe_coefficients_semantics_full.csv",
          row.names = FALSE)

# Not all words in Cornetto have a semantic feature set assigned to them
# So I have to filter them out
df_semset <- df %>% filter(semantic_feature_set != "")

lm(
  coefficient ~ valency + control + attributive + spatial + cognitive + dynamic + ValenceCategory,
  data = df_semset
) %>% summary

# But we can do a regression with just the semantic type as well for the Pardoen
# hypothesis
df_semtype <- df %>% filter(semantic_type != "")

lm(
  coefficient ~ valency + ValenceVsNeutral + semantic_type, data=df_semtype
) %>% summary
