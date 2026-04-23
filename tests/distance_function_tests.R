# Testing: Distance functions

source("R/clustering/distance_matrix.R")
library(microbenchmark)
library(stats)

df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

labels <- df[c(11),]
df <- df[-c(11),]

df_log <- log2(df + 1)
df_normalized <- t(scale(t(df_log)))

df <- t(df_normalized)

gc()
# we benchmark with a smaller distance matrix
microbenchmark(
  cpp = dist_cpp(df, method = "euclidean"),
  base = basic_dist(df, method = "euclidean"), 
  stats = as.matrix(stats::dist(df, method = "euclidean")),
  times = 10
)

# dist_cpp(df_normalized, "minkowski", 0)
# dist_cpp(df_normalized, method = "canberra")

# results
# Unit: milliseconds
#  expr       min        lq       mean     median        uq       max neval
#   cpp   10.3661   10.8389   11.75134   11.04550   12.0544   16.7512    10
#  base 1769.5834 1928.4384 1983.67655 1973.07135 2028.4863 2213.9856    10
# stats   29.3805   29.7974   33.33071   33.07225   34.7202   41.3070    10
