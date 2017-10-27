### Monte Carlo approximation
### Computing posterior mean, posterior variance, posterior median, and 95% credible interval
### Sampling model: Poisson; prior: Gamma

a <-2
b <- 1

n <- 44
sum.y <- 66

post.a <- a+sum.y
post.b <- b+n

no.iter <- seq(50,10000,by=50)

post.mean <- rep(0,length(no.iter))
post.var <- rep(0,length(no.iter))
post.median <- rep(0,length(no.iter))
post.l95 <- rep(0,length(no.iter))
post.u95 <- rep(0,length(no.iter))

for(i in 1:length(no.iter)){
	
	sample.theta <- rgamma(no.iter[i],post.a,post.b)
	post.mean[i] <- mean(sample.theta)
	post.var[i] <- (1 / (no.iter[i] - 1)) * sum((sample.theta - post.mean[i]) ^ 2)
	post.median[i] <- quantile(sample.theta,0.5)
	post.l95[i] <- quantile(sample.theta,0.025)
	post.u95[i] <- quantile(sample.theta,0.975)
	
	print(i)
}

plot(no.iter, post.mean,
	type = "l",
	col = "black",
	xlab = "No of sample values, B",
	ylab = "Posterior mean",
	main = "",
	cex.lab = 1.5,
	axes = FALSE,
	frame = TRUE,
	lty = 1,
	lwd = 3)
points(no.iter, post.mean, pch = 20, col = "red")
abline(h = post.a / post.b, col = "red", lty = 1, lwd = 3)
axis(1, cex.axis = 1.5)
axis(2, cex.axis = 1.5)

plot(no.iter, post.var,
	type="l"
	,col="black",
	xlab="No of sample values, B",
	ylab="Posterior variance",
	main="",
	cex.lab=1.5,
	axes=FALSE,
	frame=TRUE,
	lty=1,
	lwd=3)
points(no.iter,post.var, pch = 20, col = "red")
abline(h = post.a / (post.b ^ 2), col = "red", lty = 1, lwd = 3)
axis(1, cex.axis = 1.5)
axis(2, cex.axis = 1.5)


plot(no.iter, post.median, 
	type = "l", 
	col = "black",
	xlab = "No of sample values, B",
	ylab = "Posterior median",
	main = "",
	cex.lab = 1.5, 
	axes = FALSE,
	frame = TRUE,
	lty = 1,
	lwd = 3)
points(no.iter, post.median, pch = 20, col = "red")
abline(h = qgamma(0.5, post.a, post.b), col = "red", lty = 1, lwd = 3)
axis(1, cex.axis = 1.5)
axis(2, cex.axis = 1.5)

sample.theta.100 <- rgamma(100, post.a, post.b)
sample.theta.5000 <- rgamma(1000, post.a, post.b)

hist(sample.theta.100,
	col = "grey",
	breaks = 50,
	freq = FALSE,
	xlab = "Theta",
	ylab = "p(theta | y_1,.., y_n)",
	main = "B = 100")
lines(density(sample.theta.100), col = "black", lwd = 3, lty = 1)
lines(seq(0, 2, by = 0.01),
	dgamma(seq(0, 2, by = 0.01), post.a, post.b),
	col = "red", lwd = 3, lty = 1)
legend(0.72, 8, 
	col = c("black","red"),
	lwd = rep(3, 2),
	lty = rep(1, 2), 
	c("Density estimate","Gamma(68,45)"))


hist(sample.theta.5000,
	col = "grey",
	breaks = 50,
	freq = FALSE,
	xlab = "Theta",
	ylab = "p(theta | y_1,.., y_n)",
	main = "B=5000")
lines(density(sample.theta.5000),
	col = "black", lwd = 3, lty = 1)
lines(seq(0, 2, by = 0.01),
	dgamma(seq(0, 2, by = 0.01), post.a, post.b),
	col = "red",
	lwd = 3,
	lty = 1)
legend(0.8, 4, 
	   col = c("black", "red"),
	   lwd = rep(3,2),
	   lty = rep(1,2),
	   c("Density estimate", "Gamma(68,45)"))


