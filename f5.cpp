#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector f5(NumericVector x, NumericVector y) {
  int n = std::max(x.size(), y.size());
  NumericVector x1 = rep_len(x, n);
  NumericVector y1 = rep_len(y, n);
  
  NumericVector out(n);
  
  for(int i = 0; i < n; ++i) {
    out[i] = std::min(x1[i], y1[i]);
  }
  return out;
}