semcor <- read.csv("output/RoodGroenAnthe_coefficients_semantics.csv")
cornetto <- read.csv("output/cornetto_info.csv")

df <- merge(semcor, cornetto, by.x = "dominant_sense", by.y = "sense")

cornetto$dominant_sense[duplicated(cornetto$dominant_sense)]
semcor$dominant_sense[duplicated(semcor$dominant_sense)]
semcor$lemma[duplicated(semcor$lemma)]
df$lemma[duplicated(df$lemma)]

lm(coefficient ~ morpho_type + conj_type + valency + transitivity + sclass + reflexive + subject + sem_type, data=df) %>% summary
