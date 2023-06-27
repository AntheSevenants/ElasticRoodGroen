library(dplyr)

source("2-4 GAM.R")

data.frames <- list()
for (technique in c("mds", "tsne", "umap")) {
  for (kind in c("all", "non_zero", "outside_sd")) {
    target_col <- paste0(technique, ".", kind)
    fit <- build_gam(df, technique, kind)
    pred_df <- get_predictions_df(df, fit, technique, kind, 0.1)
    names(pred_df)[match('fit', names(pred_df))] <- target_col
    
    data.frames <- append(data.frames, list(pred_df))
  }
}

pred_df <- bind_rows(data.frames)

write.csv(pred_df, "output/heatmap.csv", row.names = FALSE)
