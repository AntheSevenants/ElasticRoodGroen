# Read dataframe
df <- read.csv("RoodGroenAnthe.csv")

# Find the original items count
original_items_count <- nrow(df)

# Build a frequency table so we know the participle counts
participle_counts <- table(df$participle)

# Remove all hapaxes
df <- subset(df,
        participle %in% 
        names(participle_counts[participle_counts > 1]))

# Compute the difference
difference <- original_items_count - nrow(df)
print(difference)

write.csv(df, "output/RoodGroenAnthe_sampled.csv", row.names = FALSE)
