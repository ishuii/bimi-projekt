# general formula
# d_(A u B), C = alpha_a * d_ac + alpha_b * d_bc + beta * d_ab + gamma * abs(d_ac - d_bc)

lance_williams <- function(d_ac, d_bc, d_ab, alpha_a, alpha_b, beta, gamma) {
  alpha_a * d_ac + alpha_b * d_bc + beta * d_ab + gamma * abs(d_ac - d_bc)
}

# single linkage
# min(d_ac, d_bc)
# alpha_a = alpha_b = 0.5
# beta = 0
# gamma = -0.5

single_linkage <- function(size_a, size_b) {
  list(alpha_a = 0.5, alpha_b = 0.5, beta = 0, gamma = -0.5)
}

# complete linkage
# max(d_ac, d_bc)
# alpha_a = alpha_b = 0.5
# beta = 0
# gamma = 0.5

complete_linkage <- function(size_a, size_b) {
  list(alpha_a = 0.5, alpha_b = 0.5, beta = 0, gamma = 0.5)
}

# average linkage
# alpha_a = |A| / (|A| + |B|)
# alpha_b = |B| / (|A| + |B|)
# beta = 0
# gamma = 0

average_linkage <- function(size_a, size_b) {
  total <- size_a + size_b
  list(alpha_a = size_a/total, alpha_b = size_b/total, beta = 0, gamma = 0)
}

# custom input function
custom_linkage <- function(alpha_a, alpha_b, beta, gamma) {
  function(size_a, size_b) {
    create_param_list(alpha_a, alpha_b, beta, gamma)
  }
}

create_param_list <- function(alpha_a, alpha_b, beta, gamma) {
  return(list(alpha_a = alpha_a, alpha_b = alpha_b, beta = beta, gamma = gamma))
}

# complete function ------------------------------------------------------------
hierarchical_clustering <- function(d_mat, method, custom_params = NULL) {
  if (method == "single") {
    coeff_function <- single_linkage
  } else if (method == "complete") {
    coeff_function <- complete_linkage
  } else if (method == "average") {
    coeff_function <- average_linkage
  } else if (method == "custom") {
    coeff_function <- custom_linkage(custom_params$alpha_a, custom_params$alpha_b, custom_params$beta, custom_params$gamma)
  } else {
    stop("Unbekannte Methode")
  }
  
  n <- nrow(d_mat)
  diag(d_mat) <- Inf # we don't want to have 0 as the min distance
  
  # initialize the variables we will return later
  matched_at <- numeric(length = n-1)
  merge <- matrix(0, (n-1), 2)
  
  cluster_id <- -seq_len(n)  # negative for the merge matrix: the original clusters
  
  # for average linkage: initially all clusters have the size 1
  cluster_size <- rep(1, n)
  
  # overview over active clusters (without NA)
  active <- rep(TRUE, n)
  
  for (k in 1:(n-1)) {
    # we only consider active clusters
    idx <- which(active)
    sub <- d_mat[idx, idx]
    
    # select the two clusters with the minimal distance
    d <- which(sub == min(sub), arr.ind = T)[1,]
    i <- idx[d[1]]
    j <- idx[d[2]]
    
    # store height and the clusters that where matched
    matched_at[k] <- d_mat[i,j]
    merge[k,] <- sort(c(cluster_id[i], cluster_id[j]), decreasing = TRUE)
    
    # update the cluster id for the newly matched cluster
    cluster_id[i] <- k
    
    # get coefficients based on function used
    coeff <- coeff_function(size_a = cluster_size[i], size_b = cluster_size[j])
    alpha_a <- coeff$alpha_a
    alpha_b <- coeff$alpha_b
    beta    <- coeff$beta
    gamma   <- coeff$gamma
    
    # new distance for the new cluster with Lance-Williams
    new_dist <- rep(Inf, n)
    new_dist[idx] <- lance_williams(d_mat[i,idx], d_mat[j,idx], d_mat[i,j], alpha_a, alpha_b, beta, gamma)
    new_dist[i] <- Inf
    new_dist[j] <- Inf
    
    # update distance matrix
    # remove old clusters and add new cluster values (new_dist)
    # update cluster i
    d_mat[i, ] <- new_dist
    d_mat[, i] <- new_dist
    d_mat[i, i] <- Inf
    
    # deactivate cluster j
    d_mat[j, ] <- Inf # we don't remove it to keep the matrix size the same
    d_mat[, j] <- Inf
    cluster_id[j] <- NA
    active[j] <- FALSE
    
    cluster_size[i] <- cluster_size[i] + cluster_size[j]
    cluster_size[j] <- NA
  }
  
  return(list(matched_at = matched_at, merge = merge))
}