#==============================#
#   STAN and BRMS examples     #
#==============================#

# Simple normal regression example
# 
# Fake data

y <- rnorm(n = 20, mean = 10, sd = 5)

library(rstan)

model_string <- "
  data {
    int <lower=0> N;
    real y[N];
  }
  
  parameters {
    real mu;
    real <lower=0> sigma;
  }
  model{
    y ~ normal(mu, sigma);
    mu ~ normal(0, 100);
    sigma ~ lognormal(0, 4);
  }
"

mcmc_samples <- stan(model_code = model_string, 
                     data = list(N = length(y), y = y),
                     pars = c("mu","sigma"),
                     chains = 3,
                     iter = 5000,
                     warmup = 1000)

traceplot(mcmc_samples)
plot(mcmc_samples)

mcmc_samples

#======================= BRMS =========================#
library(brms)
library(tidyverse)

# In the following, we use an example about the recurrence time of an infection
# in kidney patients initially published by McGilchrist and Aisbett (1991). 
# The data set consists of 76 entries of 7 variables:
  
data("kidney")
head(kidney)
head(arrange(kidney, patient))

# Variable time represents the recurrence time of the infection, 
# censored indicates if time is right censored (1) or not censored (0), 
# variable patient is the patient id, and recur indicates if it is the first or 
# second recurrence in that patient. Finally, variables age, sex, 
# and disease make up the predictors.
# 
# Suppose we want to predict the (possibly censored) recurrence time using a 
# log-normal model, in which the intercept as well as the effect of age is 
# nested within patients. Then, we may use the following code:
  
fit1 <- brm(time | cens(censored) ~ age * sex + disease + (1 + age | patient),
            data = kidney,
            family = lognormal(),
            prior = c(set_prior("normal(0,5)",class = "b"),
                      set_prior("cauchy(0,2)",class = "sd"),
                      set_prior("lkj(2)", class = "cor")),
            warmup = 1000, iter = 5000, chains = 3,
            control = list(adapt_delta = 0.98))

# When factors are used as predictors, parameter names will depend on the factor 
# levels. To get an overview of all parameters and parameter classes for which 
# priors can be specified, use function get_prior. For the present example, 

get_prior(time | cens(censored) ~ age * sex + disease + (1 + age|patient), 
          data = kidney, 
          family = lognormal())

# does the desired

# Could try horseshoe prior for shrinkage effects for population level effects

fit2 <- brm(time | cens(censored) ~ age * sex + disease + (1 + age|patient),
            data = kidney,
            family = lognormal(),
            prior = c(set_prior("horseshoe(1)")),
            warmup = 1000, iter = 2000, chains = 3,
            control = list(adapt_delta = 0.99))

# Analyzing the Results

stancode(fit2)
standata(fit2)
summary(fit2)

# The Eff.Sample value is an estimation of the effective sample size. 
# That is the number of independent samples from the posterior distribution 
# that would be expected to yield the same standard error of the posterior mean
# as is obtained from the dependent samples returned by the MCMC algorithm. 
# The Rhat value provides information on the convergence of the algorithm. 
# If Rhat is considerably greater than 1 (i.e., > 1.1), 
# the chains have not yet converged and it is necessary to run more iterations 
# and/or set stronger priors
# 
# With respect to the above summary, sexfemale seems to be the only
# population-level effect with considerable influence on the response. 
# Because the mean of sexfemale is positive, 
# the model predicts longer periods without an infection for females than for 
# males. Effects of population-level predictors can also be visualized with the
# marginal_effects method

plot(fit1)

# Effects of population-level predictors can also be visualized with the 
# marginal_effects method

marginal_effects(fit1)

# Looking at the group-level effects, the standard deviation parameter of age 
# is suspiciously small. To test whether it is smaller than the standard
# deviation parameter of Intercept, we apply the hypothesis method

hypothesis(fit1, 
           hypothesis = "Intercept - age > 0",
           class = "sd",
           group = "patient")

# The one-sided 95% credibility interval does not contain zero, 
# thus indicating that the standard
# deviations differ from each other in the expected direction. 
# In accordance with this finding, the Evid.Ratio shows that the hypothesis 
# being tested (i.e., Intercept - age > 0) is about 79 times more likely than the
# alternative hypothesis Intercept - age < 0. 
# It is important to note that this kind of comparison is not easily possible
# when applying frequentist methods, because in this case only point estimates 
# are available for group-level standard deviations and correlations.
# 
# When looking at the correlation between both group-level effects, 
# its distribution displayed in Figure 2 and the 95% credibility interval in the 
# summary output appear to be rather wide. 
# This indicates that there is not enough evidence in the data to reasonably 
# estimate the correlation. 
# Together, the small standard deviation of age and the uncertainty in the
# correlation raise the question if age should be modeled as a group specific 
# term at all. To answer this question, we fit another model without this term

fit3 <- update(fit1, formula =  ~ . - (1 + age|patient) + (1|patient))
summary(fit3)

# A good way to compare both models is leave-one-out cross-validation (LOO)

LOO(fit1, fit3)

# For the present example, it is immediately evident that both models have very 
# similar fit, indicating that there is little benefit in adding group specific
# coefficients for age
