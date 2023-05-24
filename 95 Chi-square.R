df_chi_square <- read.csv("output/RoodGroenAnthe_coefficients.csv")
df_chi_square$removed <- ifelse(df_chi_square$coefficient == 0, TRUE, FALSE)
df_chi_square_non_zero <- df_chi_square[!df_chi_square$removed,]
df_chi_square_non_zero$order <- 
  ifelse(df_chi_square_non_zero$coefficient > 0, "red", "green")


chi_square_data <- xtabs(~ order, data=df_chi_square_non_zero)

chi_square_test <- chisq.test(chi_square_data)
