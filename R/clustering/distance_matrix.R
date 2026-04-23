library(Rcpp)

cppFunction('
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix dist_cpp(NumericMatrix mat, std::string method = "euclidean", int p = 2) {
  int nr = mat.nrow();
  int nc = mat.ncol();
  NumericMatrix dmat(nr, nr);

  int method_id;

  if (method == "euclidean") method_id = 0;
  else if (method == "manhattan") method_id = 1;
  else if (method == "canberra") method_id = 2;
  else if (method == "pearson") method_id = 3;
  else if (method == "angular") method_id = 4;
  else if (method == "minkowski") {
    if (p <= 0) stop("p muss größer als 0 sein.");
    else if (p == 1) method_id = 1;
    else if (p == 2) method_id = 0;
    else method_id = 5;
  } else {
    stop("Unbekannte Methode");
  }

// helper functions for pearson and angular
  
  NumericVector means, norms, centered_norms;

  if (method_id == 3) {
    means = NumericVector(nr);
    centered_norms = NumericVector(nr);

    for (int i = 0; i < nr; i++) {
      double sum = 0.0;
      for (int k = 0; k < nc; k++) sum += mat(i,k);
      means[i] = sum / nc;

      double s = 0.0;
      for (int k = 0; k < nc; k++) {
        double xc = mat(i,k) - means[i];
        s += xc * xc;
      }
      centered_norms[i] = std::sqrt(s);
    }
  }

  if (method_id == 4) { 
    norms = NumericVector(nr);

    for (int i = 0; i < nr; i++) {
      double s = 0.0;
      for (int k = 0; k < nc; k++) {
        double v = mat(i,k);
        s += v * v;
      }
      norms[i] = std::sqrt(s);
    }
  }


// main function

  for (int i = 0; i < nr; i++) {
    dmat(i,i) = 0.0;

    for (int j = i + 1; j < nr; j++) {
      double d = 0.0;

      // euclidean
      if (method_id == 0) {
        double sum = 0.0;
        for (int k = 0; k < nc; k++) {
          double diff = mat(i,k) - mat(j,k);
          sum += diff * diff;
        }
        d = std::sqrt(sum);
      }

      // manhattan
      else if (method_id == 1) {
        double sum = 0.0;
        for (int k = 0; k < nc; k++) {
          sum += std::abs(mat(i,k) - mat(j,k));
        }
        d = sum;
      }

      // canberra
      else if (method_id == 2) {
        double sum = 0.0;
        for (int k = 0; k < nc; k++) {
          double xi = mat(i,k);
          double yj = mat(j,k);
          double denom = std::abs(xi) + std::abs(yj);
          if (denom > 0)
            sum += std::abs(xi - yj) / denom;
        }
        d = sum;
      }

      // pearson
      else if (method_id == 3) {
        double num = 0.0;

        for (int k = 0; k < nc; k++) {
          double xc = mat(i,k) - means[i];
          double yc = mat(j,k) - means[j];
          num += xc * yc;
        }

        double denom = centered_norms[i] * centered_norms[j];
        double corr = 0.0;

        if (denom > 0)
          corr = num / denom;

        d = 1.0 - corr;
      }

      // angular
      else if (method_id == 4) {
        double dot = 0.0;

        for (int k = 0; k < nc; k++) {
          dot += mat(i,k) * mat(j,k);
        }

        double denom = norms[i] * norms[j];
        double cos_sim = 0.0;

        if (denom > 0) {
          cos_sim = dot / denom;

          // clamp for numerical stability
          if (cos_sim > 1.0) cos_sim = 1.0;
          if (cos_sim < -1.0) cos_sim = -1.0;
        }

        d = std::acos(cos_sim) / M_PI;
      }

      // minkowski
      else if (method_id == 5) {
        double sum = 0.0;
        for (int k = 0; k < nc; k++) {
          sum += std::pow(std::abs(mat(i,k) - mat(j,k)), p);
        }
        d = std::pow(sum, 1.0 / p);
      }

      dmat(i,j) = d;
      dmat(j,i) = d;
    }
  }

  return dmat;
}
')
# TODO 
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
