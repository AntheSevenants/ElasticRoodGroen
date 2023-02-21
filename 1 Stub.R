# Read dataframe
df <- read.csv("RoodGroenAnthe.csv")

write.csv(df, "output/RoodGroenAnthe_sampled.csv", row.names = FALSE)
