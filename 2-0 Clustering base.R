library(dbscan) # for dbscan
library(magrittr) # %>%
library(DescTools) # Cstat
library(parallel) # gotta go fast
library(semvar) # Speelman & Heylen (2014)
library(cluster) # pam etc
library(nonnest2) # Vuong test


df <- read.csv("output/RoodGroenAnthe_coefficients_infused_vectors.csv")

df_copy <- df
df_copy <- df_copy[df_copy$coefficient != 0, ]