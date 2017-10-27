############################################
# Rcpp Tutorial                            #
# Authors: Romain Francois, Hadley Wickham #
############################################

library(Rcpp)
sourceCpp("hello.cpp") # see file "C:\Users\rrevans\Documents\hello.cpp"
hello()

# The sourceCpp function parses a C++ file and looks for functions marked with the Rcpp::export
# attribute. A shared library is then built and its exported functions are made available as R functions
# in the specified environment

# Including C++ Inline

# it's also possible to do inline declaration and execution of C++ code.
# There are several ways to accomplish this, including passing a code string to sourceCpp or using
# the shorter-form cppFunction or evalCpp functions

library(Rcpp)
cppFunction('int add(int x, int y, int z){
            int sum = x + y + z;
            return sum;
            }')
add(1,2,3)

# The Rcpp Interface

# Data Structures

# In the example shown in 3.2 the function returns an int (a scalar integer). Scalars and vectors are
# different. In R we need vectors for maximum functionality to be enabled. For that reason, Rcpp's
# functionality is provided through a set of C++ classes that wrap R data structures. A few of them
# are:

# Basic vector classes: NumericVector, IntegerVector, CharacterVector, and LogicalVector
# Scalar equivalents: int, double, bool, String
# Matrix Equivalents: IntegerMatrix, NumericMatrix, LogicalMatrix, CharacterMatrix
# List, DataFrame

# There are also classes for many more specialised language objects: Environment, ComplexVector,
# RawVector, DottedPair, Language, Promise, Symbol, WeakReference, and so on.
# Memory management is handled automatically by the class constructors and destructors

# Examples

# Row Maximum Function: Suppose we want to compute the maximum element of each row in a matrix. To achieve this, we
# loop over each row of the matrix and use the sugar routine max

cppFunction('NumericVector row_max(NumericMatrix m){

            int nrow = m.nrow();
            NumericVector max(nrow);

            for(int i = 0; i < nrow; i++){
              max[i] = Rcpp::max(m(i, _));
            }
            return max;
            }')
mat <- matrix(c(1:10000), nrow = 5, ncol = 2000)
row_max(mat)

# Element Sum

sumR <- function(x){
  total <- 0
  for(i in seq_along(x)){
    total <- total + x[i]
  }
  total
}

cppFunction('double sumC(NumericVector x){

              int n = x.size();
              double total = 0;

              for(int i = 0; i < n; ++i){
                total += x[i];
              }
              return total;
            }')

library(microbenchmark)

x <- runif(1e6)

microbenchmark(sum(x),
               sumC(x),
               sumR(x))

# Mean

# We will implement mean in C++ and then compares it to the built-in mean() and the purpose of
# this example is to also demonstrate how you can embed R code in special C++ comment blocks.
# This is really convenient if you want to run some test code:

cppFunction('double meanC(NumericVector x){
              
              int n = x.size();
              double total = 0;
              
              for(int i=0; i<n; ++i){
                total += x[i];
              }
              return total/n;
            }')

microbenchmark(mean(x), meanC(x))

# Fibonacci: F_n = F_n-1 + F_n -2
# Initial values: F_0 = 0 and F_1 = 1

fibR <- function(n) {
  if (n == 0) return(0)
  if (n == 1) return(1) 
  return(fibR(n - 1) + fibR(n - 2))
}

fibR(0); fibR(1); fibR(10)

cppFunction('int fibonacci(const int x) {
              if(x == 0) { return(0); }
              if(x == 1) { return(1); }
            return(fibonacci(x - 1)) + fibonacci(x - 2);
            }')
fibonacci(10)

#========================= Rcpp by Hadley =========================#

##### Scalar output, no input

one <- function() 1L

# in Rcpp:

cppFunction('int one() {
  return 1;
}')

one()

##### Scalar input, scalar output

signR <- function(x) {
  if (x > 0) 1
  else if (x == 0) 0
  else -1
}

cppFunction('
int signC(int x) {
  if (x > 0) {
    return 1;
  } else if (x == 1) {
    return 0;
  } else {
    return -1;
  }
}
')

signC(-10)

##### vector input, scalar output

sumR <- function(x) {
  total <- 0
  for (i in 1:length(x)) {
    total <- total + x[i]
  }
  total
}

x <- rgamma(10000, 0.5)

sumR(x)

cppFunction('
  double sumC(NumericVector x) {
    int n = x.size();
    double total = 0;
    for(int i = 0; i < n; ++i) {
      total += x[i];
    }
    return total;
  }
')

sumC(x)

##### vector input, vector output

pdistR <- function(x, ys) {
  sqrt((x - ys) ^ 2)
}

cppFunction('
  NumericVector pdistC(double x, NumericVector ys) {
    int n = ys.size();
    NumericVector out(n);

    for(int i = 0; i < n; ++i) {
      out[i] = sqrt(pow(ys[i] - x, 2.0));
    }
  return out;
  }
')

plot(density(pdistC(mean(x), x)))

##### Matrix input, vector output

cppFunction('
  NumericVector rowSumsC(NumericMatrix x) {
    int nrow = x.nrow(), ncol = x.ncol();
    NumericVector out(nrow);
    
    for(int i = 0; i < nrow; ++i) {
      double total = 0;
      for (int j = 0; j < ncol; ++j) {
        total += x(i, j);
      }
      out[i] = total;
    }
    return out;
  }
')

x <- matrix(sample(100), 100, 10)
dim(x)

rowSumsC(x)

sourceCpp("C:/Users/rrevans/Desktop/R code/meanC.cpp")
x <- runif(1e6)
meanC(x); mean(x)

# just a different way to compute mean
sourceCpp("C:/Users/rrevans/Desktop/R code/f1.cpp")
y <- runif(1e6)
f1(y)

# produces a sequence that adds the last result to the current result:
# 1 + 2 = 3, 3 + 3 = 6, 4 + 6 = 10, 5 + 10 = 15, ..., 10 + 45 = 55
sourceCpp("C:/Users/rrevans/Desktop/R code/f2.cpp")
y <- seq(1, 10, 1)
f2(y)

# if any element is TRUE, return TRUE
sourceCpp("C:/Users/rrevans/Desktop/R code/f3.cpp")
y <- sample(c(TRUE, FALSE), 10, replace = TRUE)
f3(y)

x <- runif(100)
y <- runif(50)
sourceCpp("C:/Users/rrevans/Desktop/R code/f5.cpp")
f5(x, y)

