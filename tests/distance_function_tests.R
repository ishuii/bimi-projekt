# Testing: Distance functions

library(distRcpp)    # devtools::install_github("ishuii/distRcpp")
library(microbenchmark)
library(stats)

df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

labels <- df[c(11),]
df <- df[-c(11),]

df_log <- log2(df + 1)
df_normalized <- t(scale(t(df_log)))

df <- t(df_normalized)

# we benchmark with a smaller distance matrix

# ------------- euclidean --------------------
microbenchmark(
  cpp = dist_cpp(df, method = "euclidean"),
  stats = as.matrix(stats::dist(df, method = "euclidean")),
  times = 100
)

# results
# Unit: milliseconds
#  expr     min      lq     mean   median       uq      max neval
#   cpp  9.5679  9.8237 12.06345 10.10760 15.09475  21.0146   100
# stats 28.5136 29.6940 35.44438 34.33175 35.38550 105.9304   100

# ------------ manhattan ----------------------
microbenchmark(
  cpp = dist_cpp(df, method = "manhattan"),
  stats = as.matrix(stats::dist(df, method = "manhattan")),
  times = 100
)

# Unit: milliseconds
#  expr     min      lq     mean   median       uq      max neval
#   cpp  9.2108  9.6334 14.85194 10.28100 17.19515 183.9987   100
# stats 28.7071 30.4709 36.78002 36.52335 38.83420 114.0772   100

# --------------- canberra ------------------
microbenchmark(
  cpp = dist_cpp(df, method = "canberra"),
  stats = as.matrix(stats::dist(df, method = "canberra")),
  times = 100
)

# Unit: milliseconds
#  expr     min      lq     mean   median       uq      max neval
#   cpp 12.2828 12.9125 15.78399 13.33065 19.87285  34.0939   100
# stats 30.3296 31.5905 37.86309 36.68875 40.46630 141.6207   100

# -------------- minkowski -------------------
microbenchmark(
  cpp = dist_cpp(df, method = "minkowski", p = 3),
  stats = as.matrix(stats::dist(df, method = "minkowski", p = 3)),
  times = 100
)

# Unit: milliseconds
#  expr      min       lq     mean   median       uq      max neval
#   cpp 158.6603 164.5358 173.0038 169.2507 176.3634 236.6788   100
# stats 703.9692 720.3147 731.2079 731.0529 740.1032 824.0864   100

# --------------
# no comparison for pearson and angular