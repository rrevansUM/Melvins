#==================================#
#     doParallel and foreach       # 
#==================================#

library(doParallel)
library(foreach)

registerDoParallel(cores = 4)

system.time(foreach(i = 1:10000) %do% sum(tanh(1:i)))

# user  system elapsed 
# 2.63    0.00    2.67

system.time(foreach(i = 1:10000) %dopar% sum(tanh(1:i)))

# user  system elapsed 
# 2.52    0.07    2.67

reps <- 1000
a <- matrix(NA, nrow = reps, ncol = 1)

for (i in 1:reps) {
  a[i] <- mean(rnorm(100))
}

a <- foreach(i = icount(reps), .combine = 'c') %dopar% {
  mean(rnorm(100))
}

a <- foreach(n = rep(reps / 8, 8), .combine = 'c') %dopar% {
  a <- numeric(n)
  for (i in seq_len(n)) {
    a[i] <- mean(rnorm(100))
  }
  a
}

# It's fine to make assignments to a inside the "foreach" loop because it's a 
# local variable. When the "for" loop finishes, a is returned, and finally all 
# of the a vectors are combined with the "c" function by the master.

#==== nesting calls ====#

# Let's say that we want to perform a Monte Carlo simulation using a function 
# called sim. The sim function takes two arguments, and we want to call it with 
# all combinations of the values that are stored in the vectors avec and bvec. 
# The following doubly-nested for loop does that. For testing purposes, the sim 
# function is defined to return 10 * a + b

# In this case, it makes sense to store the results in a matrix, 
# so we create one of the proper size called x, 
# and assign the return value of sim to the appropriate element of x each time
# through the inner loop.

sim <- function(a, b) 10 * a + b

avec <- 1:2
bvec <- 1:4

x <- matrix(0, length(avec), length(bvec))
for (j in 1:length(bvec)) {
  for (i in 1:length(avec)) {
    x[i, j] <- sim(avec[i], bvec[j])
  }
}
x

# When using foreach(), we don't create a matrix and assign values into it. 
# Instead, the inner loop returns the columns of the result matrix as vectors,
# which are combined in the outer loop into a matrix. 
# Here's how to do that using the %:% operator:

x <- foreach(b = bvec, .combine = 'cbind') %:%
        foreach(a = avec, .combine = 'c') %dopar% {
          sim(a, b)
        }
x

# This is structured very much like the nested for loop.
# The outer foreach is iterating over the values in "bvec", 
# passing them to the inner foreach, 
# which iterates over the values in "avec" for each value of "bvec". 
# Thus, the "sim" function is called in the same way in both cases.

# %:% turns multiple foreach loops into a single loop
