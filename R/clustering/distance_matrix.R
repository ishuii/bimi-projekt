library(Rcpp)

cppFunction('
  #include <cmath>
  using namespace Rcpp;
  
  // [[Rcpp::export]]
  NumericMatrix dist_cpp(NumericMatrix mat, std::string method, int p = 2) {
    int nr = mat.nrow();
    int nc = mat.ncol();
    NumericMatrix dmat(nr, nr);
    
    int method_id;
    
    if (method == "euclidean") method_id = 0;
    else if (method == "manhattan") method_id = 1;
    else if (method == "minkowski") {
      if (p <= 0) stop("p muss größer als 0 sein.");
      else if (p == 1) method_id = 1;
      else if (p == 2) method_id = 0;
      else method_id = 2;
    }
      
    for (int i = 0; i < nr; i++) {
        for (int j = i; j < nr; j++) {
            double d = 0.0;
              
            for (int k = 0; k < nc; k++) {
                double diff = mat(i,k) - mat(j,k);

                if (method_id == 0) {
                  d += diff*diff;
                } else if (method_id == 1) {
                  d += std::abs(diff); 
                } else if (method_id == 2) {
                  d += std::pow(std::abs(diff), p);
                }
            }
            
            if (method_id == 0) {
              d = std::sqrt(d);
            } else if (method_id == 2) {
              d = std::pow(d, 1.0/p);
            }
            
            dmat(i,j) = d;
            dmat(j,i) = d;
        }
    }
    return dmat;
  }')

# TODO 
# add canberra
# add pearson
# add angular
# include GUI selection: do we calculate over patients or genes?

# clustering ... (1) genes --> rows, ... (2) patients --> transpose
# information whether (1) or (2) will come from team GUI (selection by operator)

#-------------------------------------------------------------------------------

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
