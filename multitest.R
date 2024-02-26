library(dplyr)
library(zoo)
library(slider)

row_count <- 100
PRIMING_PARAGRAPHS_NO <- 1

adf <- data.frame(
  component = "A",
  paragraph_no = c(rep(1, row_count / 5), rep(2, row_count / 5), rep(3, row_count / 5), rep(4, row_count / 5), rep(5, row_count / 5)),
  sentence_no = rep(1:(row_count / 5), 5),
  order=sample(c("red", "green"), row_count, replace=TRUE)
)

# adf <- adf[adf$paragraph_no != 3,]

# indices <- sample(1:nrow(adf), 29)
# adf <- adf[-indices,]

grouped_df <- adf %>%
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

adf <- merge(adf,
      grouped_df[, c("component", "paragraph_no", "red_paragraph_primes", "green_paragraph_primes")],
      by = c("component", "paragraph_no"), all.x = TRUE)

### Sentence level

grouped_df <- adf %>%
  group_by(component, paragraph_no) %>%
  arrange(component, paragraph_no, sentence_no) %>%
  mutate(red_sentence_primes = lag(cumsum(order == "red"), default=0),
         green_sentence_primes = lag(cumsum(order == "green"), default=0)) %>%
  ungroup()

adf <- merge(adf,
             grouped_df[, c("component", "paragraph_no", "sentence_no", "red_sentence_primes", "green_sentence_primes")],
             by = c("component", "paragraph_no", "sentence_no"), all.x = TRUE)

counts <- mapply(count_preceding, 
                 adf$component, 
                 adf$paragraph_no, 
                 adf$order)

aggregate(order ~ component + paragraph_no, data = adf, 
          FUN = function(x) { sum(x) })


count_preceding_utterances <- function(df, PRIMING_PARAGRAPHS_NO) {
  # Create a vector to store the counts
  red_counts <- numeric(nrow(df))
  green_counts <- numeric(nrow(df))
  
  # Iterate over each row of the dataframe
  for (i in 1:nrow(df)) {
    # Extract relevant information for the current row
    current_component <- df$component[i]
    current_paragraph <- df$paragraph_no[i]
    current_order <- df$order[i]
    
    # Find the paragraphs preceding the current paragraph
    preceding_paragraphs <- unique(df$paragraph_no[df$component == current_component & 
                                                     df$paragraph_no < current_paragraph])
    
    # Take the last PRIMING_PARAGRAPHS_NO paragraphs
    preceding_paragraphs <- tail(preceding_paragraphs, PRIMING_PARAGRAPHS_NO)
    
    # Count the number of "red" and "green" utterances in the preceding paragraphs
    for (paragraph in preceding_paragraphs) {
      red_counts[i] <- red_counts[i] + sum(df$order[df$component == current_component & 
                                                      df$paragraph_no == paragraph] == "red")
      green_counts[i] <- green_counts[i] + sum(df$order[df$component == current_component & 
                                                          df$paragraph_no == paragraph] == "green")
    }
  }
  
  # Return the counts as a list
  return(list(red_counts = red_counts, green_counts = green_counts))
}

get_primes_v <- function(df, component_, paragraph_no_, sentence_no_) {
  # Initialize red_primes and green_primes vectors
  red_primes <- rep(0, nrow(component_))
  green_primes <- rep(0, nrow(component_))
  
  # Calculate paragraph_primes
  paragraph_primes <- aggregate(order ~ component + paragraph_no, data = df)
  
  # priming_paragraph_start <- pmax(1, paragraph_no_ - PRIMING_PARAGRAPHS_NO)
  # priming_paragraph_end <- pmax(1, paragraph_no_ - 1)
  # 
  # print(paste0("Start: ", priming_paragraph_start))
  # print(paste0("End: ", priming_paragraph_end))
  
  return(list(red_primes, green_primes))
}

get_primes_v(adf, adf$component, adf$paragraph_no, adf$sentence_no)
