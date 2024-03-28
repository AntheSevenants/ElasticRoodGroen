df_net <- read.csv("output/RoodGroenAnthe_coefficients.csv")
df_llr <- read.csv("data/DeSutterLLR.csv")

df_comp <- merge(x=df_net, y=df_llr,
            by.x="feature", by.y="participle", all.y=TRUE)
# Zero coefficients in Elastic Net, 
df_comp[!is.na(df_comp$coefficient) & df_comp$coefficient == 0,]
df_comp[is.na(df_comp$coefficient),]

# TODO: make plot better (read: add red and green colours)
plot(df_comp$llr, df_comp$coefficient)
cor.test(df_comp$coefficient, df_comp$llr) # of course, the correlation is true