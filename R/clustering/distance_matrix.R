library(Rcpp)

cppFunction('
  #include <cmath>
  using namespace Rcpp;
  
  // [[Rcpp::export]]
  NumericMatrix dist_cpp(NumericMatrix mat, std::string method) {
    int n = mat.nrow();
    int p = mat.ncol();
    NumericMatrix dmat(n, n);
      
    for (int i = 0; i < n; i++) {
        for (int j = i; j < n; j++) {
            double d = 0.0;
              
            for (int k = 0; k < p; k++) {
                double diff = mat(i,k) - mat(j,k);
                  
                if (method == "euclidean") {
                  d += diff*diff;
                } else if (method == "manhattan") {
                  d += std::abs(diff); 
                }
            }
            
            if (method == "euclidean") {
              d = std::sqrt(d);
            }
            
            dmat(i,j) = d;
            dmat(j,i) = d;
        }
    }
    return dmat;
  }')

# old version, just here for testing
basic_dist <- function(df, method = "euclidean") {
  n <- nrow(df)
  mat <- matrix(0, n, n) # create empty matrix
  
  for (i in 1:n) {
    for (j in i:n) {
      diff <- df[i,] - df[j,] # row-wise comparison 
      
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