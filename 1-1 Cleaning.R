library(stringr)
library(parallel)
library(data.table)
library(slider)

# Import the dt fixing library
source("naive-dt-fix/naive-dt-fix.R")

# Read dataframe
df <- read.csv("RoodGroenAnthe.csv")

# Find the original items count
original_items_count <- nrow(df)

# Fix tokenisation errors
df$participle <- gsub("[().»«?-]", "", df$participle)
df$participle <- tolower(df$participle)

# Remove duplicates (still needed)
df <- df[!duplicated(df[ , c("sentence", "participle_index", "auxiliary_index")]),]

# Automatic dt fixing
df <- fix_participle_dt(df, "participle")

# Fix spelling errors and other random things
replacements <- c(
  "gedomicileerd" = "gedomicilieerd",
  "gedouched" = "gedoucht",
  "dt$" = "d",
  "(\\w)\\1{2}" = "\\1\\1",
  "gedefiniëerd" = "gedefinieerd",
  "gewéést" = "geweest",
  "gezién" = "gezien",
  "gevráágd" = "gevraagd",
  "gevreëen" = "gevreeën",
  "geišnformeerd" = "geïnformeerd",
  "geeindigd" = "geëindigd",
  "gehád" = "gehad",
  "gezégd" = "gezegd",
  "gedáán" = "gedaan",
  "geinformeerd" = "geïnformeerd",
  "geiritteerd" = "geïrriteerd",
  "gesmst" = "ge-sms't",
  "geïnstaleerd" = "geïnstalleerd",
  "geinspireerd" = "geïnspireerd",
  "terrechtgekomen" = "terechtgekomen",
  "gefitnesst" = "gefitnest",
  "geplaats$" = "geplaatst",
  "(geïntresseerd|geintresseerd|geinteresseerd|geïnteresserd|geinteresseerd|geinterseerd)" = "geïnteresseerd",
  "doogeschoten$" = "doodgeschoten",
  "gepushed$" = "gepusht",
  "gefinancieerd" = "gefinancierd",
  "gesubsidiëerd" = "gesubsidieerd",
  "gedefineerd" = "gedefinieerd",
  "geintegreerd" = "geïntegreerd",
  "enzgepost" = "gepost",
  "gestummuleerd" = "gestimuleerd",
  "geaprecieerd" = "geapprecieerd",
  "vollediggewerkt" = "gewerkt",
  "geschokeerd" = "geshockeerd",
  "(gebasserd|gebasseerd)" = "gebaseerd",
  "gecurreteerd" = "gecuretteerd",
  "gefilleerd" = "gefileerd",
  "getuterd" = "getoeterd",
  "gecontrolleerd" = "gecontroleerd",
  "getruckeerd" = "getrukeerd",
  "gelerd" = "geleerd"
)
df$participle <- stringr::str_replace_all(df$participle, replacements)


df <- df[!(df$participle %in% c("zgn", "gemiddeld", "gelijkstand")), ]
df <- df[df$auxiliary_lemma %in% c("zijn", "hebben", "worden"), ]

# Remove all participles ending in 'e'
# These participles are declensed, and are thus ADJECTIVES!
get_last_char <- function(value) {
  return(substr(value, nchar(value), nchar(value)) )
}
get_last_char <- Vectorize(get_last_char)

df <- df[get_last_char(df$participle) != "e", ]

# Handle separable verbs
# In the Alpino corpus, the separable parts are split using '_'
# We can use this to encode whether a verb is separable or not
df$separable <- grepl("_", df$participle_lemma, fixed=TRUE)

# Build a frequency table so we know the participle counts
participle_counts <- table(df$participle)

# Add minimum frequency
MINIMUM_FREQUENCY = 100
df <- subset(df,
        participle %in% 
        names(participle_counts[participle_counts >= MINIMUM_FREQUENCY]))

# Some attestations have short, weird or no sentence ID
# We need the ID to compute priming, so we throw them away
# It's only a couple hundred, doesn't really matter
df <- df[df$sentence_id %>% nchar > 6, ]

# Compute the difference
second_stage_count <- nrow(df)
difference_second_stage <- original_items_count - second_stage_count

print(paste("Removed", difference_second_stage, "items"))

# Extract the SoNaR components from the sentence IDs
get_component <- function(filename) {
  component <- gsub("(.*?)\\..*", "\\1", filename)
  return(component)
}
get_component <- Vectorize(get_component) 
df$component <- get_component(df$sentence_id)

# Extract the SoNaR subcorpus from the file IDs
get_subcorpus <- function(filename) {
  subcorpus <- gsub("([a-z]{2}-[a-z]{1}-[a-z]{1}-[a-z]{1}).*", "\\1",
                    filename, ignore.case=T)
  return(subcorpus)
}
get_subcorpus <- Vectorize(get_subcorpus) 
df$subcorpus <- get_subcorpus(df$file)

# Decide whether edited or not
df$edited <- ifelse(df$subcorpus %in% 
                    c("WR-P-E-A", "WR-P-E-L", "WR-U-E-A", "WR-U-E-D"), F, T)

# Load Lassy Groot meta data and do a left join
lassy_meta <- read.csv("data/SonarMeta.csv")
df <- merge(x=df, y=lassy_meta,
            by.x="component", by.y="document")

# Load polarity data and do a left join
polarity_df <- read.csv("data/RoodGroenAnthePolarity.csv")
df <- merge(x=df, y=polarity_df,
            by="sentence_id",
            all.x=TRUE)

# Load adjectivity data and do a left join
adjectiveness_df <- read.csv("data/adjectiveness.csv")
df <- merge(x=df, y=adjectiveness_df,
                          by="participle",
                          all.x=TRUE)

# Get rid of all for which adjectiveness is too high (mistaggings)
MAXIMUM_ADJECTIVENESS = 0.9
df <- subset(df, adjectiveness < MAXIMUM_ADJECTIVENESS)

# Load frequency data and do a left join
frequency_df <- read.delim("data/SUBTLEX.txt", quote = "")
frequency_df <- frequency_df[,c("Word", "FREQcount")]
frequency_df$logfreq <- log10(frequency_df$FREQcount)
df <- merge(x=df, y=frequency_df,
                  by.x="participle",
                  by.y="Word",
                  all.x=TRUE)

# Remove records without defined adjectiveness value
# I checked them manually and they are all *not* participles
na_indices <- which(is.na(df$adjectiveness))
df[na_indices,] # Inspection
df <- df[!is.na(df$adjectiveness),]

# Remove all participles without frequency counts
# They are few (don't worry)
df[is.na(df$FREQcount),]$participle %>% unique
df <- df[!is.na(df$FREQcount),]

# Remove all UNK data
df <- df[df$country %in% c("BE", "NL"),]

# Compute the difference
third_stage_count <- nrow(df)
difference_third_stage <- second_stage_count - third_stage_count

print(paste("Removed", difference_third_stage, "items"))

# Priming information
# WARNING: very intensive process!
# (new code should take around 10 minutes to complete)

df$paragraph_no <- gsub("(.*\\.)(\\d+)(\\.s\\.)(\\d+)", "\\2", df$sentence_id, ignore.case=T) %>% as.numeric()
df$sentence_no <- gsub("(.*\\.)(\\d+)(\\.s\\.)(\\d+)", "\\4", df$sentence_id, ignore.case=T) %>% as.numeric()

PRIMING_PARAGRAPHS_NO = 1

start.time <- Sys.time()

### Paragraph level priming

grouped_df <- df %>%
  group_by(component, paragraph_no) %>%
  arrange(component, paragraph_no, sentence_no) %>%
  mutate(red_count = cumsum(order == "red"),
         green_count = cumsum(order == "green")) %>%
  summarize(red_counts = last(red_count),
            green_counts = last(green_count)) %>%
  ungroup()

grouped_df <- grouped_df %>% 
  group_by(component) %>%
  arrange(paragraph_no) %>%
  mutate(red_paragraph_primes = slide_index_sum(red_counts, paragraph_no, before = 1, after = -1, complete = FALSE),
         green_paragraph_primes = slide_index_sum(green_counts, paragraph_no, before = 1, after = -1, complete = FALSE))

df <- merge(df,
            grouped_df[, c("component", "paragraph_no", "red_paragraph_primes", "green_paragraph_primes")],
            by = c("component", "paragraph_no"), all.x = TRUE)

### Sentence level priming

grouped_df <- df %>%
  group_by(component, paragraph_no) %>%
  arrange(component, paragraph_no, sentence_no) %>%
  mutate(red_sentence_primes = lag(cumsum(order == "red"), default=0),
         green_sentence_primes = lag(cumsum(order == "green"), default=0)) %>%
  ungroup()

df <- merge(df,
            grouped_df[, c("component", "paragraph_no", "sentence_no", "red_sentence_primes", "green_sentence_primes")],
            by = c("component", "paragraph_no", "sentence_no"), all.x = TRUE)

df$red_primes <- df$red_paragraph_primes + df$red_sentence_primes
df$green_primes <- df$green_paragraph_primes + df$green_sentence_primes

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

INCREMENT <- 0.001

df$priming_rate <-
  log((df$red_primes + INCREMENT) / (df$green_primes + INCREMENT))

#
# Middle field column
#
df$middle_field_length <- pmin(df$participle_index, df$auxiliary_index) - df$clause_start_index
# Issue: for some middle fields, the length is zero (thank you online corpus)
# I just replace it with 1 to get a reasonable log
df[df$middle_field_length == 0,]$middle_field_length <- 1
df$log_middle_field_length <- log10(df$middle_field_length)

# For inspection
write.csv(data.frame(participle=unique(df$participle)), "unique.csv",
          row.names=FALSE)

# For the paper
write.csv(data.frame(name=c("first_removal", "second_removal"),
                     no_items=c(difference_second_stage, difference_third_stage)),
          "output/filtering_numbers.csv", row.names=FALSE)

# Write to file
write.csv(df, "output/RoodGroenAnthe_sampled.csv", row.names = FALSE)
