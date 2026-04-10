# in case we are not allowed to use the dist function from stats

dist <- function(data_set, method = "euclidean") {
  n <- nrow(data_set)
  mat <- matrix(0, n, n) # create empty matrix
  
  for (i in 1:n) {
    for (j in i:n) {
      diff <- data_set[i,] - data_set[j,] # row-wise comparison 
      
      if (method == "euclidean") {
        d <- sqrt(sum(diff^2))
        
      } else if (method == "manhattan") {
        d <- sum(abs(diff))
        
      } else {
        stop("Unbekannte Methode")  # we can always add more methods here
      }
      
      mat[i, j] <- d
      mat[j, i] <- d  # symmetrical matrix
    }
  }
  return(mat)
}


# Testing ----------------------------------------------------------

df <- read.csv("data/TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

labels <- df[c(11),]
df <- df[-c(11),]

df_log <- log2(df + 1)
df_normalized <- t(scale(t(df_log)))


dist_mat <- dist(t(df_normalized), method = "euclidean")
