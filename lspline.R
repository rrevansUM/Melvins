install.packages("lspline")
library(lspline)
library(ggplot2)
library(dplyr)
library(magrittr)

# coefficients are slopes of consecutive segments
# 
# coefficients capture slope change at consecutive knots
#
# Knot locations can be specified:
# 
#   manually (with lspline())
#   at breaks dividing the range of x into q equal-frequency intervals
#     qlspline()
#   at breaks dividing the range of x into n equal-width intervals
#     elspline()
#
# The implementation follows Greene (2003, chapter 7.5.2).

n <- 200
d <- data.frame(x = scales::rescale(rchisq(n, 6), c(0, 20)))
d$interval <- findInterval(d$x, c(5, 10), rightmost.closed = TRUE) + 1
d$slope <- c(2, -3, 0)[d$interval]      # I think this is just arbitrary
d$intercept <- c(0, 25, -5)[d$interval] # so is this
d$y <- with(d, intercept + slope * x + rnorm(n, 0, 1)) # add y = mx + randomness

head(d)

p <- d %>%
  ggplot(aes(x = x, y = y)) %>%
  add(geom_point(aes(color = as.character(slope)))) %>%
  add(scale_color_discrete(name = "Slope")) %>%
  add(theme_bw())

p

# Setting knot locations manually

# parameterizing the slopes of individual segments (marginal = FALSE)
m1 <- lm(y ~ lspline(x, c(5, 10), marginal = FALSE), data = d)
broom::tidy(m1)

# parameterizing with coeficients measuring change in slope (derivative)
m2 <- lm(y ~ lspline(x, c(5, 10), marginal = TRUE), data = d)
broom::tidy(m2)

#   term                                    estimate  std.error   statistic       p.value
# 1                            (Intercept) -0.2890142 0.24170444  -1.195734  2.332451e-01
# 2 lspline(x, c(5, 10), marginal = TRUE)1  2.0807546 0.06546599  31.783750  2.851693e-79
# 3 lspline(x, c(5, 10), marginal = TRUE)2 -5.0972988 0.10573678 -48.207432 1.194627e-110
# 4 lspline(x, c(5, 10), marginal = TRUE)3  2.9824455 0.09370027  31.829635  2.249739e-79

# -2.08 is the slope of the first segment
# -5.10 is the change in slope of the second segment beginning at x = 5,
# it changes from 2 to -3 (-2 + -3) = -5
# 2.98 is the change in slope at knot x = 10, changing from -3 to 0

all.equal(fitted(m1), fitted(m2)) # nearly identical

p %>%
  add(geom_smooth(method = "lm", formula = formula(m1), se = FALSE)) %>%
  add(geom_vline(xintercept = c(5, 10), linetype = 2))



