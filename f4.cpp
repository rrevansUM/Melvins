#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int f4(Function pred, List x) {
  int n = x.size();
  
  for(int i = 0; i < n; ++i) {
    LogicalVector res = pred(x[i]);
    if (res[0]) return i + 1;
  }
  return 0;
}
