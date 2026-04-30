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
  stats = stats::dist(df, method = "euclidean"),
  times = 100
)

# results
# Unit: milliseconds
#  expr       min      lq     mean   median       uq      max neval
#   cpp  9.655701 10.02240 13.06204 10.9106 14.86445  36.7332   100
# stats 22.637501 22.96015 25.77228 23.3134 25.05680 109.6400   100

# ------------ manhattan ----------------------
microbenchmark(
  cpp = dist_cpp(df, method = "manhattan"),
  stats = stats::dist(df, method = "manhattan"),
  times = 100
)

# Unit: milliseconds
#  expr     min       lq     mean   median       uq     max neval
#   cpp  9.1722  9.68820 11.77226 10.21165 11.43685 26.6401   100
# stats 22.6297 23.16325 24.83491 23.54075 24.33770 38.0660   100

# --------------- canberra ------------------
microbenchmark(
  cpp = dist_cpp(df, method = "canberra"),
  stats = stats::dist(df, method = "canberra"),
  times = 100
)

# Unit: milliseconds
#  expr     min       lq     mean  median       uq     max neval
#   cpp 12.3061 12.95555 16.16518 13.5529 17.83705 30.9185   100
# stats 24.1258 24.96220 26.69668 25.5618 27.44805 38.0805   100

# -------------- minkowski -------------------
microbenchmark(
  cpp = dist_cpp(df, method = "minkowski", p = 3),
  stats = stats::dist(df, method = "minkowski", p = 3),
  times = 100
)

# Unit: milliseconds
#  expr      min       lq     mean   median       uq      max neval
#   cpp 158.4086 162.1364 170.1709 166.7419 174.4418 217.3223   100
# stats 696.6187 706.6745 714.1992 713.1131 720.1876 758.3541   100

# --------------
# no comparison for pearson and angular