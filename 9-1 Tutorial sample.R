set.seed(20230703)

df <- read.csv("output/RoodGroenAnthe_sampled.csv")

sample_row_indices <- sample(nrow(df),
                             size = 2500,
                             replace = FALSE)
df_sample <- df[sample_row_indices, c("participle", "order", "country", "edited")]

write.csv(df_sample, "output/RoodGroenAnthe_tutorial.csv",
          row.names=FALSE)
