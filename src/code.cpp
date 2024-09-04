#include <Rcpp.h>
using namespace Rcpp;


//' Smoothing using exponential weighted moving average
//'
//' @inheritParams ewma_cc_vec
//' @return `vector<numeric`: The smoothed series
//'
//' @import Rcpp
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericVector ewma_smoothing(Rcpp::NumericVector x, double lambda){
    int n = x.length();
    NumericVector z(n);
    z[0] = x[0]*lambda;
    if (n > 1) {
     for (int i = 1; i < n; i++) {
       z[i] = lambda * x[i] + (1 - lambda) * z[i-1];
     }
    }
    return z;
}


