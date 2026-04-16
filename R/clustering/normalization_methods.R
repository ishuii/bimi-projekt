
# !!!These functions are not yet tested!!!

# data preparation
# goal: remove labels row, save it in list if needed later
# we only normalize and cluster the rest of the data

prepare_data <- function(df) {
  # Labels = last row
  labels <- as.numeric(df[nrow(df), ])
  
  # remove labels on lowest row
  df <- df[-nrow(df), ]
  
  # check if everything is numerical
  df <- as.matrix(df)     
  mode(df) <- "numeric"
  
  # we return matrix to make sure it stays numeric (no switch back to dataframe)
  
  return(list(data = df, labels = labels))
}

################################################################################
# (1) standard: log + z-score
# -> the first one we used, and a pretty good start
normalize_log_zscore <- function(df) {
  df_log <- log2(df + 1)
  df_norm <- t(scale(t(df_log)))
  return(df_norm)
}

# note Wiki: should df be overriden with normalization? or keep "old" form?
# i.e. in code: (1) same variable: df <- normalize_log_zscore(df)
#          or   (2) new variable:  df_norm <- normalize_log_zscore(df)

# what it does:
# Log reduces outliers
# Z-Score → same scaling per gene

# best choice for:
# Clustering
# Heatmaps

################################################################################
# (2) CPM+log+z-score 

normalize_cpm_log_zscore <- function(df) {
  col_sums <- colSums(df)
  
  # stop to prevent division by 0
  if (any(col_sums == 0)) {
    stop("Some columns have sum = 0 (cannot compute CPM)")
  }
  
  df_cpm <- t(t(df) / col_sums) * 1e6
  df_log <- log2(df_cpm + 1)
  df_norm <- t(scale(t(df_log)))
  return(df_norm)
}

# what it does:
# corrects sequencing depth (CPM = counts per million)
# afterwards like the standard

# best for:
# true RNA-Seq Counts (like TCGA)

################################################################################

# (3) just Log (if absolut differences are important)
normalize_log_only <- function(df) {
  return(log2(df + 1))
}

# only does transformation
# disadvantage: genes with high variation dominate

################################################################################

# (4) correlation based method (very good for clustering)
get_correlation_distance <- function(df) {
  df_log <- log2(df + 1)
  dist_mat <- as.dist(1 - cor(df_log))
  return(dist_mat)
}

# what it does:  measures similarities of patterns
# very good for: Gene-expression-clustering, often better than euclidian


# ------------------------------ TESTING ------------------------------------------

df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

##### Test 1 - prepare_data() ----------------------------------------------------
prepared_data_list <- prepare_data(df)

# first list entry is a matrix
prepared_data <- prepared_data_list[[1]]

##### Test 2 - normalize_log_zscore() ---------------------------------------------
log_zscore_df <- normalize_log_zscore(prepared_data)   # returns a matrix

# margin = 1 if looking at rows, if at cols margin = 2
test_zscore <- function(x, z_func, margin = 1, tol = 1e-6) {
  y <- z_func(x)
  
  if (margin == 1) {
    means <- rowMeans(y)    # means should be close to 0 (< 1e-6)
    sds <- apply(y, 1, sd)  # sd should be close to 1 ( (sd-1) < 1e-6 )
  } else {
    means <- colMeans(y)
    sds <- apply(y, 2, sd)
  }
  
  if (!all(abs(means) < tol)) {
    stop("Z-score test failed: means not ~0")
  }
  
  if (!all(abs(sds - 1) < tol)) {
    stop("Z-score test failed: sds not ~1")
  }
  
  message("Z-score test passed")
}

test_zscore(prepared_data, normalize_log_zscore, margin = 1)

# seems to work!

##### Test 3 - normalize_cpm_log_zscore -------------------------------------------

# log and zscore parts have already been tested
# here, we test only the cpm section: df_cpm <- t(t(df) / col_sums) * 1e6

cpm_data <- t(t(prepared_data) / colSums(prepared_data)) * 1e6

test_cpm <- function(x, tol = 1e-6) {
  col_sums <- colSums(x)
  
  if (!all(abs(col_sums - 1e6) < tol)) {
    stop("CPM test failed: column sums are not ~1e6")
  }
  
  message("CPM test passed")
}

test_cpm(cpm_data)
# CPM section works

##### Test 4 - normalize_log_only ------------------------------------------------

test_log <- function(x, log_func) {
  y <- log_func(x)
  
  # Check finite values
  if (any(!is.finite(y))) {
    stop("Log test failed: non-finite values present")
  }
  
  # Check monotonicity (flattened)
  if (!all(order(x) == order(y))) {
    stop("Log test failed: not monotonic")
  }
  
  message("Log test passed")
}

test_log(prepared_data, normalize_log_only)
# Log test passed

##### Test 5 - get_correlation_distance() ----------------------------------------
corr_df <- get_correlation_distance(prepared_data)

# returns a dist object
# should this step skip the distance function??







