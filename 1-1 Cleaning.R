library(stringr)
library(parallel)
library(data.table)

# Import the dt fixing library
source("naive-dt-fix/naive-dt-fix.R")

# Read dataframe
df <- read.csv("RoodGroenAnthe.csv")

# Find the original items count
original_items_count <- nrow(df)

# Fix tokenisation errors
df$participle <- gsub("[().»«?-]", "", df$participle)
df$participle <- tolower(df$participle)

# Remove duplicates
df <- df[!duplicated(df[ , c("sentence", "participle")]),]

# Automatic dt fixing
df <- fix_participle_dt(df, "participle")

# Fix spelling errors and other random things
df$participle <- gsub("gedomicileerd", "gedomicilieerd", df$participle)
df$participle <- gsub("gedouched", "gedoucht", df$participle)
df$participle <- gsub("dt$", "d", df$participle) # dt$ -> d$
df$participle <- gsub("(\\w)\\1{2}", "\\1\\1", df$participle) # 3x -> 2x
df$participle <- gsub("gedefiniëerd", "gedefinieerd", df$participle)
df$participle <- gsub("gewéést", "geweest", df$participle)
df$participle <- gsub("gezién", "gezien", df$participle)
df$participle <- gsub("gevráágd", "gevraagd", df$participle)
df$participle <- gsub("gevreëen", "gevreeën", df$participle)
df$participle <- gsub("geišnformeerd", "geïnformeerd", df$participle)
df$participle <- gsub("geeindigd", "geëindigd", df$participle)
df$participle <- gsub("gehád", "gehad", df$participle)
df$participle <- gsub("gezégd", "gezegd", df$participle)
df$participle <- gsub("gedáán", "gedaan", df$participle)
df$participle <- gsub("geinformeerd", "geïnformeerd", df$participle)
df$participle <- gsub("geiritteerd", "geïrriteerd", df$participle)
df$participle <- gsub("gesmst", "ge-sms't", df$participle)
df$participle <- gsub("geïnstaleerd", "geïnstalleerd", df$participle)
df$participle <- gsub("geinspireerd", "geïnspireerd", df$participle)
df$participle <- gsub("terrechtgekomen", "terechtgekomen", df$participle)
df$participle <- gsub("gefitnesst", "gefitnest", df$participle)
df$participle <- gsub("geplaats$", "geplaatst", df$participle)
df$participle <- 
  gsub("(geïntresseerd|geintresseerd|geinteresseerd|geïnteresserd|geinteresseerd|geinterseerd)",
       "geïnteresseerd", df$participle)
df$participle <- gsub("doogeschoten$", "doodgeschoten", df$participle)
df$participle <- gsub("gepushed$", "gepusht", df$participle)
df$participle <- gsub("gefinancieerd", "gefinancierd", df$participle)
df$participle <- gsub("gesubsidiëerd", "gesubsidieerd", df$participle)
df$participle <- gsub("gedefineerd", "gedefinieerd", df$participle)
df$participle <- gsub("geintegreerd", "geïntegreerd", df$participle)
df$participle <- gsub("enzgepost", "gepost", df$participle)
df$participle <- gsub("gestummuleerd", "gestimuleerd", df$participle)
df$participle <- gsub("geaprecieerd", "geapprecieerd", df$participle)
df$participle <- gsub("vollediggewerkt", "gewerkt", df$participle)
df$participle <- gsub("geschokeerd", "geshockeerd", df$participle)
df$participle <- gsub("(gebasserd|gebasseerd)", "gebaseerd", df$participle)
df$participle <- gsub("gecurreteerd", "gecuretteerd", df$participle)
df$participle <- gsub("gefilleerd", "gefileerd", df$participle)
df$participle <- gsub("getuterd", "getoeterd", df$participle)
df$participle <- gsub("gecontrolleerd", "gecontroleerd", df$participle)
df$participle <- gsub("getruckeerd", "getrukeerd", df$participle)
df$participle <- gsub("gelerd", "geleerd", df$participle)


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
MINIMUM_FREQUENCY = 10
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
dt <- as.data.table(df)

PRIMING_PARAGRAPHS_NO = 1
paragraph_information <- str_match_all(df$sentence_id,
                                       "(.*p\\.)(\\d+)(\\.s\\.)(\\d+)")

start.time <- Sys.time()
priming_info <- mclapply(paragraph_information, function(paragraph_tuple) {
  prefix <- paragraph_tuple[, 2]
  paragraph_no <- as.numeric(paragraph_tuple[, 3])
  infix <- paragraph_tuple[,4]
  sentence_no <- as.numeric(paragraph_tuple[,5])
  
  priming_paragraph_start <-
    max(1, paragraph_no - PRIMING_PARAGRAPHS_NO)
  priming_paragraph_end <- max(1, paragraph_no - 1)
  priming_range <- priming_paragraph_start:priming_paragraph_end
  
  primers <- list("red" = 0, "green" = 0)
  
  for (primer_index in priming_range) {
    needle <- paste0(prefix, primer_index)
    primer_rows <- dt[startsWith(dt$sentence_id, needle),]

    if (dim(primer_rows)[1] > 0) {
      for (i in 1:nrow(primer_rows)) {
        row <- primer_rows[i,]
        
        primers[[row$order]] = primers[[row$order]] + 1
      }
    }
  }
  
  priming_sentence_start <- 1
  priming_sentence_end <- max(1, sentence_no - 1)
  priming_range <- priming_sentence_start:priming_sentence_end

  for (primer_index in priming_range) {
    needle <- paste0(prefix, paragraph_no, infix, primer_index)
    #print(needle)
    primer_rows <- dt[startsWith(dt$sentence_id, needle),]

    if (dim(primer_rows)[1] > 0) {
      for (i in 1:nrow(primer_rows)) {
        row <- primer_rows[i,]

        primers[[row$order]] = primers[[row$order]] + 1
      }
    }
  }
  
  rm(primer_rows)
  #gc()
  
  return(primers)
}
, mc.cores = min(detectCores(), 8), mc.preschedule=TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

priming_df <- as.data.frame(do.call(rbind, priming_info))
colnames(priming_df) <- c("red_primes", "green_primes")

df$red_primes <- as.numeric(priming_df$red_primes)
df$green_primes <- as.numeric(priming_df$green_primes)

INCREMENT <- 0.001

df$priming_rate <-
  log((df$red_primes + INCREMENT) / (df$green_primes + INCREMENT))

# For inspection
write.csv(data.frame(participle=unique(df$participle)), "unique.csv",
          row.names=FALSE)

# For the paper
write.csv(data.frame(name=c("first_removal", "second_removal"),
                     no_items=c(difference_second_stage, difference_third_stage)),
          "output/filtering_numbers.csv", row.names=FALSE)

# Write to file
write.csv(df, "output/RoodGroenAnthe_sampled.csv", row.names = FALSE)
