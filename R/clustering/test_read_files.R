# Test: reading CSV files in R

library(readr) # this library creates a tibble - pretty fast 

# TCGA = The Cancer Genome Atlas

tcga_kidney_unnormalized_TOP10 <- read_csv("data/TCGA_kidney_unnormalized_TOP10.csv")
tcga_kidney_unnormalized <- read_csv("data/TCGA_kidney_unnormalized.csv")

