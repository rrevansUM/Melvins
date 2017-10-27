library(ggplot2)
library(sandwich)
library(msm)

# Examples of Poisson regression
# Example 1. The number of persons killed by mule or horse kicks in the Prussian army per year. 
# Ladislaus Bortkiewicz collected data from 20 volumes of Preussischen Statistik. 
# These data were collected on 10 corps of the Prussian army in the late 1800s over the course of 20 years.
# 
# Example 2. The number of people in line in front of you at the grocery store. 
# Predictors may include the number of items currently offered at a special discounted price and 
# whether a special event (e.g., a holiday, a big sporting event) is three or fewer days away.
# 
# Example 3. The number of awards earned by students at one high school. P
# redictors of the number of awards earned include the type of program in which the student was enrolled 
# (e.g., vocational, general or academic) and the score on their final exam in math.

# Description of the data
# For the purpose of illustration, we have simulated a data set for Example 3 above. 
# In this example, num_awards is the outcome variable and indicates the number of awards earned by 
# students at a high school in a year, math is a continuous predictor variable and represents students' 
# scores on their math final exam, and prog is a categorical predictor variable with three levels 
# indicating the type of program in which the students were enrolled. 
# It is coded as 1 = "General", 2 = "Academic" and 3 = "Vocational". 
# Let's start with loading the data and looking at some descriptive statistics.

p <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")
glimpse(p)
p <- within(p, {
  prog <- factor(prog, 
  	levels = 1:3, 
  	labels = c("General","Acedemic","Vocational"))
  id <- factor(id)
})
glimpse(p)
summary(p)

# Each variable has 200 valid observations and their distributions seem quite reasonable.
mean(p$num_awards)
var(p$num_awards)
sd(p$num_awards)
# The unconditional mean and variance of our outcome variable are not extremely different. 
# Our model assumes that these values, conditioned on the predictor variables, 
# will be equal (or at least roughly so).

# We can use the tapply function to display the summary statistics by program type. 
# The table below shows the average numbers of awards by program type and seems to suggest 
# that program type is a good candidate for predicting the number of awards, our outcome variable, 
# because the mean value of the outcome appears to vary by prog. 
# Additionally, the means and variances within each level of prog
# --the conditional means and variances--are similar.

with(p, tapply(num_awards, prog, function(x) {
	sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
	})
)

# A conditional histogram separated out by program type is plotted to show the distribution.

plot <- ggplot(p, aes(num_awards, fill = prog))
plot <- plot + geom_histogram(binwidth = 0.5, position = "dodge")
plot

model.1 <- glm(num_awards ~ prog + math, family = "poisson", data = p)
summary(model.1)

# Cameron and Trivedi (2009) recommended using robust standard errors for the parameter estimates 
# to control for mild violation of the distribution assumption that the variance equals the mean. 
# We use R package sandwich below to obtain the robust standard errors and calculated the p-values 
# accordingly. Together with the p-values, we have also calculated the 95% confidence interval using 
# the parameter estimates and their robust standard errors

cov.ml <- vcovHC(model.1, type = "HC0") # Heteroskedasticity-consistent estimation of the covariance matrix 
cov.ml
std.err <- sqrt(diag(cov.ml))
std.err
r.est <- cbind(Estimate=coef(model.1),
               model.se = summary(model.1)$coefficients[,2],
               "Robust SE" = std.err,
               "Pr(>|z|)" = 2*pnorm(abs(coef(model.1)/std.err), 
               	lower.tail = FALSE),
               LB = coef(model.1) - 1.96 * std.err,
               UB = coef(model.1) + 1.96 * std.err)
r.est

# We can also test the overall effect of prog by comparing the deviance of the full model with 
# the deviance of the model excluding prog. The two degree-of-freedom chi-square test indicates 
# that prog, taken together, is a statistically significant predictor of num_awards.

model.2 <- update(model.1, . ~ . - prog)
anova(model.2, model.1, test = "Chisq")

# Sometimes, we might want to present the regression results as incident rate ratios and their 
# standard errors, together with the confidence interval. 
# To compute the standard error for the incident rate ratios, we will use the Delta method. 
# To this end, we make use the function deltamethod implemented in R package msm.

# The delta method expands a differentiable function of a random variable about its mean, 
# usually with a first-order Taylor approximation, and then takes the variance

s <- deltamethod(list(~ exp(x1), ~ exp(x2), ~ exp(x3), ~ exp(x4)), coef(model.1), cov.ml)
rexp.est <- exp(r.est[,-4])
rexp.est[, "Robust SE"] <- s
rexp.est
list(r.est, rexp.est)

# Sometimes, we might want to look at the expected marginal means. 
# For example, what are the expected counts for each program type holding math score at its overall mean? 
# To answer this question, we can make use of the predict function. 
# First off, we will make a small data set to apply the predict function to it.

s1 <- data.frame(math = mean(p$math),
                 prog = factor(1:3, levels = 1:3, labels = levels(p$prog)))

predict(model.1, s1, type = "response", se.fit = TRUE)

# We can also graph the predicted number of events

### calculate and store predicted values
p$phat <- predict(model.1, type = "response")
### order by program and then by math
p <- p[with(p, order(prog, math)), ]
## create plot
plot <- ggplot(p, aes(x = math, y = phat, color = prog))
plot <- plot + geom_point(aes(y = num_awards), 
	alpha = 0.5, 
	position = position_jitter(h=0.2))
plot <- plot + geom_line(size = 1)
plot <- plot + labs(x  ="Math Score", y = "Expected Number of Awards")
plot

# The graph indicates that the most awards are predicted for those in the academic program (prog = 2), 
# especially if the student has a high math score. The lowest number of predicted awards is for those 
# students in the general program (prog = 1). 
# The graph overlays the lines of expected values onto the actual points, 
# although a small amount of random noise was added vertically to lessen overplotting.

###############################
# Italian Credit Card Example #
###############################

credit.dat <- read.table("/Users/richardcompy/Desktop/creditcards.txt")
glimpse(credit.dat)
colnames(credit.dat) <- c("income","number.cases","credit.cards")
summary(credit.dat)
credit.dat$lcases <- log(credit.dat$number.cases)
glimpse(credit.dat)

log.fit <- glm(credit.cards ~ income + offset(lcases), 
	family = poisson, 
	data = credit.dat)
log.fit2 <- glm(credit.cards ~ income,
 offset=lcases,
  family = poisson,
   data = credit.dat)
summary(log.fit)
summary(log.fit2)
# offset is the same either way

fitted(log.fit)

