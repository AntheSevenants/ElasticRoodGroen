df_net <- read.csv("output/RoodGroenAnthe_coefficients_infused.csv")
df_llr <- read.csv("data/DeSutterLLR.csv")

# Read Bloem's values (asc), then convert to logit
df_oddsr <- read.csv("data/collostruct7_wiki_asc.txt")
df_oddsr$logit <- log(df_oddsr$Unconditional.MLE)


# De Sutter
df_ds <- merge(x=df_net, y=df_llr,
            by.x="feature", by.y="participle", all.y=TRUE)
# Zero coefficients in Elastic Net, 
ds_zero <- df_ds[!is.na(df_ds$coefficient) & df_ds$coefficient == 0,]

ds_na <- df_ds[is.na(df_ds$coefficient),]

# TODO: make plot better (read: add red and green colours)
quadrant_plot <- function(x, y, xlab=NA, ylab=NA, goffset=0, roffset=0) {
  par(mar=c(4, 4, 1, 1))
  
  plot(x, y, xlab=xlab, ylab=ylab)
  lines(x=c(0,0), y=c(120, 0), col="red")
  lines(x=c(0,0), y=c(0, -120), col="green")
  lines(x=c(-120,0), y=c(0,0), col="green")
  lines(x=c(0,120), y=c(0,0), col="red")
}

#quadrant_plot(df_ds$llr, df_ds$coefficient, "Log likelihood ratio", "Elastic net coefficient", -1, -1.7)

ds_cor <- cor.test(df_ds$coefficient, df_ds$llr) # of course, the correlation is true

# Bloem
df_bloem <- merge(x=df_net, y=df_oddsr, by.x="lemma", by.y="Verb", all.y=T)
df_bloem <- df_bloem[df_bloem$lemma != "zeggen",]

bloem_zero <- df_bloem[!is.na(df_bloem$coefficient) & df_bloem$coefficient == 0,]
bloem_na <- df_bloem[is.na(df_bloem$coefficient),]

df_bloem_nna <- df_bloem[!is.na(df_bloem$coefficient) & df_bloem$logit != Inf,]
bloem_cor <- cor.test(df_bloem_nna$coefficient, df_bloem_nna$logit)

#quadrant_plot(df_bloem_nna$logit, df_bloem_nna$coefficient, "Odds ratio", "Elastic net coefficient", -1, -1.7)

