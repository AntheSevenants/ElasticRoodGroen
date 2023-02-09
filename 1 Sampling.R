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
# We now know each participle has enough attestations, but are there enough
# participles in the first place?
component_whitelist <- c()
# Loop over each component
for (component in dimnames(crosstable)$component) {
  # For each order, check how many participles are actually there
  frequencies <- c()
  for (order in dimnames(crosstable)$order) {
    # Filter the data frame for that component and order
    df_component <- df_filtered[df_filtered$component == component &
                                df_filtered$order == order,]
    
    # Get all unique participles
    eligible_participles <- unique(df_component$participle)
    
    frequencies <- append(frequencies, length(eligible_participles))
  }
  
  # Find the required number of participles
  n_participles_to_sample <- SAMPLE_PER_COMPONENT / MINIMUM_PARTICIPLE_THRESHOLD
  
  # If the minimum frequency is enough, add the component to our whitelist
  if (min(frequencies) >= n_participles_to_sample) {
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
    
    # Get all unique participles
    eligible_participles <- unique(df_component$participle)
    
    # Find the required number of participles
    n_participles_to_sample <- SAMPLE_PER_COMPONENT / MINIMUM_PARTICIPLE_THRESHOLD
    
    # Actually sample the participles
    sampled_participles <- sample(eligible_participles,
                                  n_participles_to_sample, replace=FALSE)
    
    # This list will hold all sampled items from the different participles
    participle_samples <- c()
    
    # For each participle, sample the minimum participle threshold
    for (participle in sampled_participles) {
      # Only get the rows corresponding to this participle
      df_participle <- df_component[df_component$participle == participle,]
      
      #print(participle)
      #print(nrow(df_participle))
      #flush.console()
      
      # Sample the required rows
      df_participle_sample <- df_participle[sample(nrow(df_participle),
                                           size = MINIMUM_PARTICIPLE_THRESHOLD,
                                           replace = FALSE),]
      
      participle_samples <- append(participle_samples, list(df_participle_sample))
    }
    
    # Combine all participle samples
    df_component_sample <- do.call("rbind", participle_samples)
  
    # Append to list of component samples
    samples <- append(samples, list(df_component_sample))
  }
}

df_sample <- do.call("rbind", samples)

write.csv(df_sample, "output/RoodGroenAnthe_sampled.csv", row.names = FALSE)
