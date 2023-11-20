output <- squares_test_df(df, "mds", "non_zero", -1.8, 1.7, 3.2, 3, 0.1)
output <- squares_test_df(df, "tsne", "non_zero", -5, 5, 10, 3, 0.1)
output <- squares_test_df(df, "umap", "non_zero", -2.8, 2.6, 5.25, 3, 0.1)

pairwise_t <- output[["pairwise_t"]]
squares_df <- output[["squares_df"]]
pairwise_t <- output[["pairwise_t"]]

for (index in squares_df$index) {
  print(index)
}

# Bonferroni k
# I count the number of the non-NA values in the table
bonferroni_k <- sum(!is.na(pairwise_t$p.value))

source_c <- c()
dest_c <- c()
sig_c <- c()
bonf_c <- c()
lcoef_c <- c()
rcoef_c <- c()
conc_c <- c()

# We go over each table row
for (i in 1:dim(pairwise_t$p.value)[1]) {
  # We go over each column in the roiw
  for (j in 1:length(pairwise_t$p.value[i,])) {
    # Get the value at this position
    value <- pairwise_t$p.value[i,j]
    # If value is NA, go to the next value
    if (is.na(value)) {
      next
    }
    
    sig <- FALSE
    
    # If not significant, also skip
    if (value <= 0.05) {
      sig = TRUE
    }
    
    # Significant after Bonferroni correction?
    bonf_sig <- value <= 0.05 / bonferroni_k
    
    print(paste0(i + 1, " -> ", j))
    print(sig)
    print(bonf_sig)
    
    left_adjectiveness <- squares_df[i + 1,][["mean_adjectiveness"]]
    left_coefficient <- squares_df[i + 1,][["mean_coefficient"]]
    
    right_adjectiveness <- squares_df[j,][["mean_adjectiveness"]]
    right_coefficient <- squares_df[j,][["mean_coefficient"]]
    
    max_adj_idx <- which.max(c(left_adjectiveness, right_adjectiveness))
    max_coeff_idx <- which.max(c(left_coefficient, right_coefficient))
    
    equal <- base::all.equal(left_coefficient, right_coefficient,
                             tolerance=0.1)
    equal <- typeof(equal) == "logical"
    concordant <- max_adj_idx == max_coeff_idx
    
    concordant_out <- NULL
    
    if (equal) {
      concordant_out <- "tie"
    } else {
      concordant_out <- ifelse(concordant, "concordant", "discordant")
    }
    
    source_c <- append(source_c, i + 1)
    dest_c <- append(dest_c, j)
    sig_c <- append(sig_c, sig)
    bonf_c <- append(bonf_c, bonf_sig)
    lcoef_c <- append(lcoef_c, left_coefficient)
    rcoef_c <- append(rcoef_c, right_coefficient)
    conc_c <- append(conc_c, concordant_out)
  }
}

output_df <- data.frame("source" = source_c,
                     "dest" = dest_c,
                     "sig" = sig_c,
                     "bonf_sig" = bonf_c,
                     "left_coefficient" = lcoef_c,
                     "right_coefficient" = rcoef_c,
                     "concordant" = conc_c)


concordancy_plot <- function(output_df) {
  all_plot <- ggplot(data = output_df) +
    geom_bar(aes(x = fct_infreq(concordant))) +
    labs(x = "concordancy")
}



conc_plot <- output_df %>% concordancy_plot()
sig_plot <- output_df %>% dplyr::filter(sig == TRUE) %>% concordancy_plot()
bonf_sig_plot <- output_df %>% dplyr::filter(bonf_sig == TRUE) %>% concordancy_plot()

cowplot::plot_grid(conc_plot, sig_plot, bonf_sig_plot, ncol=3, nrow=1,
                   labels=c("ALL", "SIGNIFICANT", "BONFERRONI SIGNIFICANT"))
