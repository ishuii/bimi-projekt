# Implementation of average-linkage

##### Get data and normalize it -----------------------------------------------------

# data will be read in by team GUI and received from it
# here we do this step directly just for the first implementation

# first column is treated as row names
df <- read.csv("C://Users//rosin//Downloads//TCGA_kidney_unnormalized_TOP10.csv", row.names = 1)

df <- df[, 1:20]  # only first 20 cols OPTIONAL

# extract labels and remove them from the dataset
labels <- df[c(11),]
df <- df[-c(11),]

# data is unnormalized --> difficult to compare
# for clustering, Z-score normalization is best
df_log <- log2(df + 1)
df_normalized <- t(scale(t(df_log)))

# ------------------------------------------------------------------------------------
##### distance matrix

# clustering ... (1) genes --> rows, ... (2) patients --> transpose
# information whether (1) or (2) will come from team GUI (selection by operator)

# requires a distance matrix for the dendrogram
# are we allowed to use the existing dist method?

library(stats) # for the dist function --> otherwise use distance_matrix.R


dist_object <- dist(t(df_normalized), method = "euclidean")
dist_mat <- as.matrix(dist_object)

diag(dist_mat) <- Inf
d <- which(dist_mat == min(dist_mat), arr.ind = T)[1,]
max(d)


######################################
# Average Linkage

average_linkage <- function(d_mat) {
  n <- nrow(d_mat)
  diag(d_mat) <- Inf
  
  matched_at <- numeric(length = n-1)
  merge <- matrix(0, (n-1), 2)
  
  cluster_id <- -seq_len(n)
  cluster_size <- rep(1, n)  # <-- NEU: jedes Objekt startet mit Größe 1
  
  for (k in 1:(n-1)) {
    d <- which(d_mat == min(d_mat), arr.ind = TRUE)[1,]
    i <- d[1]
    j <- d[2]
    
    matched_at[k] <- d_mat[i, j]
    merge[k, ] <- sort(c(cluster_id[i], cluster_id[j]), decreasing = TRUE)
    
    # neue Clustergröße
    new_size <- cluster_size[i] + cluster_size[j]
    
    # gewichtetes Mittel berechnen
    new_dist <- (cluster_size[i] * d_mat[i, ] +
                   cluster_size[j] * d_mat[j, ]) / new_size
    
    # Update Matrix
    d_mat[i, ] <- new_dist
    d_mat[, i] <- new_dist
    d_mat[i, i] <- Inf
    
    d_mat[j, ] <- Inf
    d_mat[, j] <- Inf
    
    # Updates
    cluster_id[i] <- k
    cluster_id[j] <- NA
    
    cluster_size[i] <- new_size
    cluster_size[j] <- 0
  }
  
  return(list(matched_at = matched_at, merge = merge))
}
################################################################################

cluster_res <- average_linkage(dist_mat)


hc <- hclust(dist_object, method = "average")
#test mit anderen cluster codes


# numerischer Vergleich der Höhen (best practice)
all.equal(cluster_res$matched_at, hc$height)


plot(cluster_res$matched_at, hc$height,
     xlab = "Dein Complete Linkage",
     ylab = "hclust Complete Linkage",
     main = "Vergleich der Clusterhöhen",
     pch = 19, col = "blue")

abline(0, 1, col = "red", lwd = 2)
#--------------------------------------------------------

plot(cluster_res$matched_at - hc$height,
     type = "b",
     pch = 19,
     col = "purple",
     ylab = "Differenz",
     xlab = "Merge-Schritt",
     main = "Differenz der Höhen")

abline(h = 0, col = "red", lwd = 2)
#----------------------------------------------------------

par(mfrow = c(1, 2))

plot(hc, main = "hclust (complete)", xlab = "", sub = "")

# eigenes Objekt in hclust-Format bringen
hc_custom <- list(
  merge = cluster_res$merge,
  height = cluster_res$matched_at,
  order = hc$order,   # gleiche Blattreihenfolge für Vergleich
  labels = colnames(df_normalized),
  method = "complete"
)

class(hc_custom) <- "hclust"

plot(hc_custom, main = "eigene Implementierung", xlab = "", sub = "")



