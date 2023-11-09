library(DescTools)

source("2-4 GAM squares.R")

squares_test(df, "mds", "non_zero", -1.8, 1.7, 3.2, 3, 0.1)
squares_test(df, "tsne", "non_zero", -5, 5, 10, 3, 0.1)
squares_test(df, "umap", "non_zero", -2.8, 2.6, 5.25, 3, 0.1)

scatter_pairs(df, "mds", "non_zero", -1.8, 1.7, 3.2, 3, 0.1)
conc_disc_pairs(df, "mds", "non_zero", -1.8, 1.7, 3.2, 3, 0.1)

scatter_pairs(df, "tsne", "non_zero", -5, 5, 10, 3, 0.1)
scatter_pairs(df, "umap", "non_zero", -2.8, 2.6, 5.25, 3, 0.1)
