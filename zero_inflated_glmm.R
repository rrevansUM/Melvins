#=========================================================
# Fitting count and zero-inflated count GLMMs with mgcv
#=========================================================

# install.packages(c("glmmTMB","ggstance"))
library(glmmTMB)    # fitting zero-inflated GLMMs
library(mgcv)       # GAM and GLMM
library(tidyverse)
library(ggstance)
library(magrittr)

# here are several ways in which mgcv allows GLMMs to be fitted, 
# but the way that interests me here is via gam() 
# and the random effect spline basis. 
# Penalised splines of the type provided in mgcv 
# can also be represented in mixed model form,
# such that GAMs can also be fitted using mixed effect modelling software. 
# The general idea is that the spline is decomposed into two parts:

#   the perfectly smooth parts of the basis, namely those functions, 
#   including constant and linear functions, 
#   in the penalty null space of the spline. 
#   These are added to the fixed effects model matrix, whilst,

#   the remaining wiggly parts of the basis are treated as random effects.

# Given this duality between splines and random effects, 
# you can reverse the idea and create a spline basis that is the equivalent 
# of a simple Gaussian i.i.d random effect,
# such that you can fit a GLMM or GAMM using GAM software like mgcv. 
# mgcv has the re basis for this,
# and I'll exploit that to fit the zero-inflated GLMMs to the two examples.

# In Brooks et al. (2017), two example data sets are used;
# 
# Salamanders - 
#   Seven combinations of different salamander species and life-stages
#   were sampled repeatedly sampled four times at 23 sites in Applachian streams 
#   (Price et al., 2016). 
#   Some of the streams were impacted by mountaintop removal and valley filling 
#   from coal mining. 
#   The data are available from Price et al. (2015), 
#   as well as the glmmTMB package.
#   
# Owls - the second example is a well-studied one in 
# mixed modelling papers and textbooks 
# (Zuur et al., 2009, Bolker et al. (2013)), 
# and relates to the begging behaviour of owl nestlings.
# The data were originally reported in Roulin and Bersier (2007).

# Salamanders

# Brooks et al. (2017) fit several count models to the Salamander data set, 
# including standard Poisson GLMMs, negative binomial GLMMs, 
# with theta estimated and modelled via a linear predictor,
# as well as zero-inflated Poisson (ZIP) and 
# zero-inflated negative binomial (ZINB) models. Of these,
# gam() can currently fit all but the negative binomial with theta modelled via a 
# linear predictor and the ZINB models.

data(Salamanders)
dim(Salamanders)

head(Salamanders)

pairs(Salamanders[,c("count","spp","mined","site")], upper.panel = NULL)

nbgam2 <- gam(count ~ spp * mined + s(site, bs = "re"),
              data = Salamanders,
              family = nb,
              method = "ML")

nbm2 <- glmmTMB(count ~ spp * mined + (1|site),
                data = Salamanders,
                family = nbinom2)

nbgam2_coef <- coef(nbgam2)[c(1:14)]
nbm2_coef <- coef(summary(nbm2))$cond[,"Estimate"]

nb2_coefs <- data.frame(estimate = c(nbgam2_coef, nbm2_coef),
                        model = rep(c("mgcv::gam","glmmTMB"), each = 14),
                        term = rep(names(coef(nbgam2)[c(1:14)]), 2))

nb2_coefs %>%
  ggplot(aes(x = estimate, y = term, colour = model, shape = model)) +
  geom_point(position = position_dodgev(height = 0.3),
             size = 3) +
  labs(y = NULL,
       x = "Regression estimate",
       title = "Comparing mgcv with glmmTMB",
       subtitle = "Salamander: Negative Binomial")

# Theys the same

# comparing the means or posterior modes for the 'site' random effect

nbgam2_r <- coef(nbgam2)[-c(1:14)]
nbm2_r   <- ranef(nbm2)$cond$site[,1]
nms <- sub("s\\(site\\)\\.", "Site ", names(nbgam2_r))
ranefs <- data.frame(ranef = c(unname(nbgam2_r), nbm2_r),
                     model = rep(c("glmmTMB", "mgcv::gam"), 
                                 each = length(nbgam2_r)),
                     site  = rep(nms, 2))
ranefs <- transform(ranefs, site = factor(site, nms[order(nbgam2_r)]))

ranefs %>%
  ggplot(aes(x = ranef, y = site, colour = model, shape = model)) +
  geom_point(position = position_dodgev(height = 0.5),
             size = 3) +
  labs(y = NULL,
       x = "Random effect",
       title = "Comparing mgcv with glmmTMB",
       subtitle = "Salamanders: Negative Binomial")

gam.vcomp(nbgam2)

# generate and plot fitted values from the model
# need to consider whether to and how to marginalise over or 
# condition on the random effects
# aim is to predict at the population mode by setting 
# the random effect component to 0

# ZIP model with linear predictors for both the mean 
# and the zero-inflation components of the model

zipm3 <- glmmTMB(count ~ spp * mined + (1|site),
                 zi = ~ spp * mined, # zero-inflation predictor
                 data = Salamanders,
                 family = poisson)

zipgam3 <- gam(list(count ~ spp * mined + s(site, bs = "re"), # for the mean
                    ~ spp * mined), # zero-inflation
               data = Salamanders,
               family = ziplss,
               method = "REML") # REML is the only option available

sal.uniq <- unique(Salamanders[,c("mined","spp")])
newd <- as.data.frame(cbind(sal.uniq, site = "R -1"))
newd0 <- newd

rownames(newd0) <- rownames(newd) <- NULL

# predict setting random effect for site to zero (exclude)
pred <- predict(zipgam3, newd, exclude = "s(site)", type = "link")

pred

# column 1 is predicted value of the response from the poisson part of the model
# on the scale of the linear predictor (log scale).
# column 2 is the predicted value from the zero-inflation component and is
# on the scale of the clog-log scale.
# these need to be back transformed to the respective response scales and
# then multiplied together

ilink <- binomial(link = "cloglog")$linkinv

newd$fitted <- exp(pred[,1]) * ilink(pred[,2])

newd %>%
  ggplot(aes(x = spp, y = fitted, color = mined)) %>%
  add(geom_point(size = 3))
