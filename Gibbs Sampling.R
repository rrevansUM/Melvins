
# There is a number of individuals, say n=12, who take a pass/fail test 15 times. 
# For each individual we have recorded the number of passes, which can go from 0 to 15. 
# Because of confidentiality issues, 
# we are presented with rounded-to-the-closest-multiple-of-3 data. 
# We are interested in estimating theta of the Binomial distribution behind the data.
# 
# Rounding is probabilistic,
# with probability 2/3 if you are one count away from a multiple of 3 
# and probability 1/3 if the count is you are two counts away. Multiples of 3 are not rounded.
#
# We can use Gibbs sampling to alternate between sampling the posterior for the unrounded Y and theta. 
# 
# In the case of Y I used:

possible <- function(rounded){
  if(rounded == 0){
    options <- c(0, 1, 2)
  } else {
    options <- c(rounded - 2,
                 rounded - 1,
                 rounded,
                 rounded + 1,
                 rounded + 2)
  }
  return(options)
}

# probability mass function (pmf) of numbers rounding to R given theta
prior_y <- function(options, theta){
  p <- dbinom(options, 15, prob = theta)
  return(p)
}

# likelihood of rounding
like_round3 <- function(options){
  if(length(options) == 3){
    like <- c(1, 2/3, 1/3)
  }
  else{
    like <- c(1/3, 2/3, 1, 2/3, 1/3)
  }
}

# estimate posterior mass function, draw 1 value from it
post_sample_y <- function(R, theta){
  po <- possible(R)
  pr <- prior_y(po, theta)
  li <- like_round3(po)
  post <- li * pr / sum(li * pr)
  samp <- sample(po, 1, prob=post)
  return(samp)
}

# While for theta we are assuming a vague Beta(alpha=1, beta=1), as prior density function for theta, 
# so the posterior density is a Beta(alpha + sum Y_i, beta + 12*15 - sum Y_i).

# function to sample from posterior P(theta|Y, R)
post_sample_theta <- function(alpha, beta, Y){
  theta <- rbeta(1, alpha + sum(Y), beta + 12 * 15 - sum(Y))
  return(theta)
}

# implementation
R <- c(0, 0, 3, 9, 3, 0, 6, 3, 0, 6, 0, 3)
nsim <- 10000
burnin <- 1000
alpha <- 1
beta <- 1
store <- matrix(0, nrow = nsim, ncol = length(R) + 1)

starting.values <- c(R, 0.1)

# sampling

store[1, ] <- starting.values
for(i in 2:nsim){
  current <- store[i - 1, ]
  for(j in 1:length(R)){
    y <- post_sample_y(R[j], current[length(R) + 1])
    # Jump or not still missing
    current[j] <- y
  }
  theta <- post_sample_theta(alpha, beta, current[1:length(R)])
  # jump or not still missing
  current[length(R) + 1] <- theta
  store[i, ] <- current
}

plot((burnin + 1):nsim, store[(burnin + 1):nsim, 13], type = "l")

data.frame(theta = store[(burnin + 1):nsim, 13]) %>%
  ggplot(aes(x = theta)) %>%
  add(geom_density(fill = "blue", alpha = 0.5))

multiple_plot <- data.frame(Y = matrix(store[(burnin+1):nsim, 1:12], 
                                       nrow = (nsim - burnin)*12,
                                       ncol = 1))
multiple_plot$obs <- factor(rep(1:12, each = (nsim - burnin)))

ggplot(multiple_plot, aes(x = Y)) %>%
  add(geom_histogram()) %>%
  add(facet_grid(~obs))

