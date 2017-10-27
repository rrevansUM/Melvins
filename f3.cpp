#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool f3(LogicalVector x) {
  int n = x.size();
  
  for (int i = 0; i < n; ++i) {
    if (x[i]) return true;
  }
  return false;
}
