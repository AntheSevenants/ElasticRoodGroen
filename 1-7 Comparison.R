df_net <- read.csv("output/RoodGroenAnthe_coefficients_infused.csv")
df_llr <- read.csv("data/DeSutterLLR.csv")
df_oddsr <- read.csv("data/BloemOddsHebben.csv")

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

quadrant_plot(df_ds$llr, df_ds$coefficient, "Log likelihood ratio", "Elastic net coefficient", -1, -1.7)

ds_cor <- cor.test(df_ds$coefficient, df_ds$llr) # of course, the correlation is true

# Bloem
df_bloem <- merge(x=df_net, y=df_oddsr, by.x="lemma", by.y="lemma", all.y=T)

plot(df_bloem$oddsr, df_bloem$coefficient)

cor.test(df_bloem$coefficient, df_bloem$oddsr)

