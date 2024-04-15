semcor <- read.csv("output/RoodGroenAnthe_coefficients_semantics.csv")
cornetto <- read.csv("output/cornetto_info.csv")
speed <- read.csv("data/SpeedBrysbaertEmotionNorms.tsv", sep="\t")
speed$ValenceCategory <- factor(speed$ValenceCategory, levels=c("neutral", "positive", "negative"))

df <- merge(semcor, cornetto, by.x = "dominant_sense", by.y = "sense")
df <- merge(df, speed, by.x="lemma", by.y="Word")

df <- df %>% filter(semantic_feature_set != "")
df$transitivity <- ifelse(df$valency == "mono", "intransitive", "transitive")

lm(coefficient ~ valency + control + attributive + spatial + cognitive + dynamic + ValenceCategory, data=df) %>% summary