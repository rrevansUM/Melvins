#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector f2(NumericVector x) {
  int n = x.size();
  NumericVector out(n);
  
  out[0] = x[0];
  for(int i = 1; i < n; i++) {
    out[i] = out[i - 1] + x[i];
  }
  return out;
}
