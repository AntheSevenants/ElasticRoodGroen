# Import the dt fixing library
source("naive-dt-fix/naive-dt-fix.R")

# Read dataframe
df <- read.csv("RoodGroenAnthe.csv")

# Find the original items count
original_items_count <- nrow(df)

# Fix tokenisation errors
df$participle <- gsub("[.»«]", "", df$participle)
df$participle <- tolower(df$participle)

# Fix spelling errors
df$participle <- gsub("gedomicileerd", "gedomicilieerd", df$participle)
df$participle <- gsub("gedouched", "gedoucht", df$participle)

# Automatic dt fixing
df <- fix_participle_dt(df, "participle")

# Fix random things
df$participle <- gsub("gedefiniëerd", "gedefinieerd", df$participle)

# Remove all participles ending in 'e'
# These participles are declensed, and are thus ADJECTIVES!
get_last_char <- function(value) {
  return(substr(value, nchar(value), nchar(value)) )
}
get_last_char <- Vectorize(get_last_char)

df <- df[get_last_char(df$participle) != "e", ]

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
