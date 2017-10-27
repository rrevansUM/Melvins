####  Using Monte Carlo approximation to compute posterior distribution of functions of one parameter
####  Sampling model: Bernouilli independent random variables
####  Prior for the probability of success: Beta distribution
####  We are looking at the log odds

nsim <- 5000
a <- 0.1
b <- 5

n <- 20
sumy <- 0
post.a <- a+sumy
post.b <- b+n

theta.val.post <- rep(0,nsim)
gamma.val.post <- rep(0,nsim)
theta.val.prior <- rep(0,nsim)
gamma.val.prior <- rep(0,nsim)


for(i in 1:nsim){
    theta.val.prior[i] <- rbeta(1, a, b)
    gamma.val.prior[i] <- log(theta.val.prior[i] / (1 - theta.val.prior[i]))
    theta.val.post[i] <- rbeta(1,post.a,post.b)
    gamma.val.post[i] <- log(theta.val.post[i] / (1 - theta.val.post[i]))
    print(i)
}


plot(seq(0, 1, by = 0.01),
	 dbeta(seq(0, 1, by = 0.01), a, b),
	 type = "l",
	 lty = 1, 
	 lwd = 3,
	 xlab = "Theta",
	 ylab = "",
	 main = "Prior and posterior distributions for theta",
	 col = "grey",
	 cex = 1.5)
lines(seq(0, 1, by = 0.01),
	  dbeta(seq(0, 1, by = 0.01), post.a, post.b),
	  type = "l",lty = 1,lwd = 3,col = "red")
abline(v = a / (a + b), col = "grey", lty = 3, lwd = 3)
abline(v = post.a / (post.a + post.b), col = "red", lty = 3, lwd = 3)
legend(0.4, 6, 
	   col = c("grey","red"),
	   c("Prior distribution", "Posterior distribution"),
	   lwd = rep(3, 2), 
	   lty = rep(1, 2))


hist(theta.val.post,
	col = "grey",
	breaks = 50,
	freq = FALSE,
	xlab = "Theta",
	ylab = "p(theta | y_1,.., y_n)",
	main = "Density estimate and exact posterior distribution for theta")
lines(density(theta.val.post), col = "black", lwd = 3, lty = 1)
lines(seq(0, 1, by = 0.01),
	  dbeta(seq(0, 1, by = 0.01), post.a, post.b),
	  col = "red", lwd = 3, lty = 1)
legend(0.02, 400,
	  col = c("black","red"),
	  lwd = rep(3, 2),
	  lty = rep(1, 2),
	  c("Density estimate of posterior distribution", "Beta(0.1,25.1)"))

hist(gamma.val.post,
	 col = "grey", 
	 breaks = 50,
	 freq = FALSE,
	 xlab = "gamma",
	 ylab = "p(gamma | y_1,.., y_n)",
	 main = "Monte Carlo estimate of posterior distribution \n of log-odds")
lines(density(gamma.val.post), col = "black", lwd = 3, lty = 1)

hist(gamma.val.prior,
	 col = "grey",
	 breaks = 50,
	 freq = FALSE,
	 xlab = "gamma",
	 ylab = "p(gamma)",
	 main = "Monte Carlo estimate of prior distribution \n of log-odds")
lines(density(gamma.val.prior), col = "black", lwd = 3, lty= 1 )

plot(density(gamma.val.post),
	 type = "l",
	 col = "red",
	 xlab = "gamma",
	 ylab = "",
	 lwd = 3,
	 lty = 1,
	 main = "Monte Carlo estimate of prior and posterior distribution \n of log-odds")
lines(density(gamma.val.prior), col ="grey", lwd = 3, lty= 1)
legend(-80, 0.06,
	   col = c("grey","red"),
	   c("Prior distribution", "Posterior distribution"),
	   lwd = rep(3, 2),
	   lty = rep(1,2))

## Monte Carlo estimates

## Monte Carlo estimate of the prior mean of theta
mean(theta.val.prior)

## Monte Carlo estimate of the posterior mean of theta
mean(theta.val.post)

## Monte Carlo estimate of the prior mean of the log-odds
mean(gamma.val.prior)

## Monte Carlo estimate of the posterior mean of the log-odds
mean(gamma.val.post)

## Monte Carlo estimate of the prior variance of theta
var(theta.val.post)

## Monte Carlo estimate of the posterior variance of the log-odds
var(gamma.val.post)

## Monte Carlo standard error
sqrt(var(gamma.val.post) / 5000)


## Monte Carlo estimate of the lower bound of a 95% credible interval for theta
quantile(theta.val.post, 0.025)
## Monte Carlo estimate of the upper bound of a 95% credible interval for theta
quantile(theta.val.post, 0.975)

## Monte Carlo estimate of the lower bound of a 95% credible interval for the log-odds
quantile(gamma.val.post, 0.025)
## Monte Carlo estimate of the upper bound of a 95% credible interval for the log-odds
quantile(gamma.val.post, 0.975)

## Lower bound of a 95% confidence interval for the posterior mean of the log-odds using the Central Limit Theorem
low.95.gamma.post <- mean(gamma.val.post) - 1.96 * (sd(gamma.val.post) / sqrt(5000))
low.95.gamma.post

## Upper bound of a 95% confidence interval for the posterior mean of the log-odds using the Central Limit Theorem
upp.95.gamma.post <- mean(gamma.val.post) + 1.96 * (sd(gamma.val.post) / sqrt(5000))
upp.95.gamma.post



### Exact values

## Exact lower bound of a 95% credible interval for theta (from the Beta distribution)
qbeta(0.025, post.a, post.b)
## Exact upper bound of a 95% credible interval for theta (from the Beta distribution)
qbeta(0.975, post.a, post.b)

## Exact value of the posterior mean (from the Beta distribution)
exact.post.mean <- post.a / (post.a + post.b)
exact.post.mean

## Exact value of the posterior variance (from the Beta distribution)
exact.post.var <- (post.a * post.b) / ((post.a + post.b + 1) * ((post.a + post.b) ^ 2))
exact.post.var



###  Monte Carlo approximation to compute posterior probability of two parameters
###  Sampling model: Poisson; Prior: gamma
###  Two populations with two mean parameters, for which we assume an independent prior and for
###  we assume the data are conditionally independent (i.e. the data for population 1 is conditionally independent on theta2 given theta1
###  and same for population 2)

a <- 2
b <- 1

n1 <- 111
sum.y1 <- 217

n2 <- 44
sum.y2 <- 66

post.a1 <- a + sum.y1
post.b1 <- b + n1

post.a2 <- a + sum.y2
post.b2 <- b + n2


nsim <- 5000
theta1.post <- rep(0, nsim)
theta2.post <- rep(0, nsim)

for(i in 1:nsim){
    theta1.post[i] <- rgamma(1, post.a1, post.b1)
    theta2.post[i] <- rgamma(1, post.a2, post.b2)
}

### Monte Carlo estimate of the posterior probability that theta1 is larger than theta2
sum(theta1.post > theta2.post) / nsim
