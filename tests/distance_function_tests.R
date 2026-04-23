# Testing: Distance functions

source("R/clustering/distance_matrix.R")
library(microbenchmark)
library(stats)

df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

labels <- df[c(11),]
df <- df[-c(11),]

df_log <- log2(df + 1)
df_normalized <- t(scale(t(df_log))) 

# canberra_test <- dist_cpp(df_normalized, method = "canberra")

# we benchmark with a smaller distance matrix
microbenchmark(
  base = basic_dist(df_normalized, method = "euclidean"), 
  stats = stats::dist(df_normalized, method = "euclidean"),
  cpp = dist_cpp(df_normalized, method = "euclidean")
)

# dist_cpp(df_normalized, "minkowski", 0)

# results
# Unit: microseconds
#   expr    min      lq     mean  median     uq     max neval
#   base 1364.4 1383.05 2130.921 1425.95 1852.7 14920.2   100
#  stats  201.8  206.40  233.314  220.45  249.6   344.9   100
#    cpp  168.1  169.50  196.110  171.80  176.4  2043.1   100
