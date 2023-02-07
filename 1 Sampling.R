# That's so random!
set.seed(2204355)

# Read dataframe
df <- read.csv("RoodGroenAnthe.csv")

# Extract the SoNaR components from the filenames
get_component <- function(filename) {
  component <- gsub("(.*?)_.*", "\\1", filename)
  return(component)
}
get_component <- Vectorize(get_component) 
df$component <- get_component(df$file)

# Participles must appear at least ten times in order to be eligible
MINIMUM_PARTICIPLE_THRESHOLD = 10

crosstable <- as.table(xtabs(~ order + component, df))

filtered <- c()
# Filter out all items which do not fit minimum requirements
for (component in dimnames(crosstable)$component) {
  for (order in dimnames(crosstable)$order) {
    # Filter the data frame for that component and order
    df_component <- df[df$component == component & df$order == order,]
    
    # Build a frequency table so we know the participle counts
    participle_counts <- table(df_component$participle)
    
    # Filter the data frame for participles having n > threshold
    df_component <- subset(df_component,
                           participle %in% names(participle_counts[participle_counts >= MINIMUM_PARTICIPLE_THRESHOLD]))
    
    # Append to list of filtered dataframes
    filtered <- append(filtered, list(df_component))
  }
}
df_filtered <- do.call("rbind", filtered)

# Which components are eligible for use?
crosstable <- as.table(xtabs(~ order + component, df_filtered))
crosstable

# How many samples do we want per component and per order, at least?
SAMPLE_PER_COMPONENT = 1000

# We keep a record of which components are eligible to be used
component_whitelist <- c()
# Loop over each component
for (component in dimnames(crosstable)$component) {
  # Keep the frequencies of each component and each order
  frequencies <- c()
  for (order in dimnames(crosstable)$order) {
    frequencies <- append(frequencies, crosstable[order, component])
  }
  
  # If the minimum frequency is enough, add the component to our whitelist
  if (min(frequencies) >= SAMPLE_PER_COMPONENT) {
    component_whitelist <- append(component_whitelist, component)
  }
}

component_whitelist

# Sampling time!
samples <- c()
# Loop over each component and order
for (component in component_whitelist) {
  for (order in dimnames(crosstable)$order) {
    # Filter the data frame for that component and order
    df_component <- df_filtered[df_filtered$component == component & df_filtered$order == order,]
    # Sample the required rows
    df_component_sample <- df_component[sample(nrow(df_component),
                                               size = SAMPLE_PER_COMPONENT,
                                               replace = FALSE),]
    # Append to list of samples
    samples <- append(samples, list(df_component_sample))
  }
}

df_sample <- do.call("rbind", samples)

write.csv(df_sample, "output/RoodGroenAnthe_sampled.csv", row.names = FALSE)
