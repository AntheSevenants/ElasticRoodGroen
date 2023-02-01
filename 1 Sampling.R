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

# Which components are eligible for use?
crosstable <- as.table(xtabs(~ order + component, df))
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

# Sampling time!
samples <- c()
# Loop over each component and order
for (component in component_whitelist) {
  for (order in dimnames(crosstable)$order) {
    # Filter the data frame for that component and order
    df_component <- df[df$component == component & df$order == order,]
    # Sample the required rows
    df_component_sample <- df_component[sample(nrow(df_component),
                                               size = SAMPLE_PER_COMPONENT,
                                               replace = FALSE),]
    # Append to list of samples
    samples <- append(samples, list(df_component_sample))
  }
}

df_sample <- do.call("rbind", samples)

write.csv(df_sample, "RoodGroenAnthe_sampled.csv", row.names = FALSE)
