df_net <- read.csv("output/RoodGroenAnthe_coefficients_infused.csv")
df_llr <- read.csv("data/DeSutterLLR.csv")
df_oddsr <- read.csv("data/BloemOddsHebben.csv")

# De Sutter
df_ds <- merge(x=df_net, y=df_llr,
            by.x="feature", by.y="participle", all.y=TRUE)
# Zero coefficients in Elastic Net, 
df_ds[!is.na(df_ds$coefficient) & df_ds$coefficient == 0,]
df_ds[is.na(df_ds$coefficient),]

# TODO: make plot better (read: add red and green colours)
plot(df_ds$llr, df_ds$coefficient)
cor.test(df_ds$coefficient, df_ds$llr) # of course, the correlation is true

# Bloem
df_bloem <- merge(x=df_net, y=df_oddsr, by.x="lemma", by.y="lemma", all.y=T)

plot(df_bloem$oddsr, df_bloem$coefficient)

cor.test(df_bloem$coefficient, df_bloem$oddsr)

