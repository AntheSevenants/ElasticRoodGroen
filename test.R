# Read dataframe
df <- read.csv("RoodGroenAnthe.csv")

get_component <- function(filename) {
  component <- gsub("(.*?)_.*", "\\1", filename)
  return(component)
}
get_component <- Vectorize(get_component) 

df$component <- get_component(df$file)

xtabs(~ order + component, df)