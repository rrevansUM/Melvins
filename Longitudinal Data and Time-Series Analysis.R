###############################
# Longitudinal Data Tutorials #
###############################

# Generate a longitudinal dataset and convert it into long format

library(MASS)
dat.tx.a <- mvrnorm(n=250, mu=c(30, 20, 28), 
                    Sigma=matrix(c(25.0, 17.5, 12.3, 
                                   17.5, 25.0, 17.5, 
                                   12.3, 17.5, 25.0), nrow=3, byrow=TRUE))
dat.tx.b <- mvrnorm(n=250, mu=c(30, 20, 22), 
                    Sigma=matrix(c(25.0, 17.5, 12.3, 
                                   17.5, 25.0, 17.5, 
                                   12.3, 17.5, 25.0), nrow=3, byrow=TRUE))

dat <- data.frame(rbind(dat.tx.a, dat.tx.b))

names(dat) = c('measure.1', 'measure.2', 'measure.3')

dat.wide <- data.frame(subject.id=factor(1:500), tx=rep(c('A', 'B'), each=250), dat)
glimpse(dat.wide)

rm(dat.tx.a, dat.tx.b)

dat.long <- reshape(dat.wide, 
                    varying=c('measure.1', 'measure.2', 'measure.3'), 
                    idvar='subject.id', 
                    direction='long')
glimpse(dat.long)
dat.long <- dplyr::arrange(dat.long, subject.id, time)
glimpse(dat.long)

# Multilevel Growth Models/Mixed Models/Hierarchical Models

# (1|subject.id) is the random effect by person, assuming a random intercept for each person
# Random effects are useful in helping to capture the variance without sacrificing too many degrees of freedom

# There are two types of random effects: random intercepts and random slopes. 
# In this particular example, we only used the random intercept, meaning that the average depression score 
# over time is varied by person but the depression trajectory (the slope) is assumed to be heterogeneous

# a random slope model would be specified by, (time|subject.id) instead of (1|subject.id)

# One of the most problematic limitations in linear models is it assumes the error terms to be independent of each other. 
# This could be true for simple random sampling in cross-sectional data but probably not in multilevel samples 
# (e.g. data from cluster random sampling scheme) and longitudinal data. 
# The failure in capturing the interpersonal differences would result in inflated Type I error. 
# In other word, every significant findings you get in the linear regression could be just wrong. 
# Therefore, it's often good to try multilevel regressions if you suspect the data-set is not 
# independently distributed.

library(lme4)
model1 <- lmer(measure ~ time + (1|subject.id), data = dat.long)
summary(model1)

# Linear mixed model fit by REML ['lmerMod'] 
# Formula: measure ~ time + (1 | subject.id)
# Data: dat.long
# 
# REML criterion at convergence: 9786.2
# 
# Scaled residuals: 
#   Min       1Q   Median       3Q      Max 
# -3.03798 -0.68157  0.03651  0.67748  3.00222 
# 
# Random effects:
#   Groups     Name        Variance Std.Dev.
# subject.id (Intercept) 12.55    3.543   
# Residual               30.50    5.522   
# Number of obs: 1500, groups:  subject.id, 500
#
# There are two sources of variance in this model: 
# the residual (the usual one as in linear models) 
# and the interpersonal difference (i.e. subject.id). 
# One common way to quantify the strength of interpersonal difference is intraclass correlation coefficient (ICC).
# It is possible to compute ICC from the multilevel model and it is just 12.55/(12.55 + 30.50) = 0.292,
# which means 29.2% of the variation in depression score could be explained by interpersonal difference.
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)  30.0275     0.4092   73.39
# time         -2.4800     0.1746  -14.20
#
# p-values are generally not reported here, but can be approximated
# 
# Correlation of Fixed Effects:
#   (Intr)
# time -0.854

# Multilevel growth models with approximate p-values

library(lmerTest)
model2 <- lmer(measure ~ time + (1|subject.id), data = dat.long)
summary(model2)

# Calculating 95% CI and PI

dat.new <- data.frame(time=1:3) # specify the time points that we want the average values
dat.new$measure <- predict(model2, dat.new, re.form=NA) # predict() function to get the average values from the model ignoring the conditional random effects (re.form=NA)
m.mat <- model.matrix(terms(model2), dat.new) # calculating the variances of the average values
dat.new$var <- diag(m.mat%*%vcov(model2)%*%t(m.mat)) + VarCorr(model2)$subject.id[1] # further calculating variances
#  It's basically the matrix cross-products plus the variance of the random intercept
dat.new$pvar  <- dat.new$var + sigma(model2)^2 # calculates the variances of a single observation, which is the variance of the average values plus the residual variance
dat.new$ci.lb <- with(dat.new, measure - 1.96*sqrt(var))
dat.new$ci.ub <- with(dat.new, measure + 1.96*sqrt(var))
dat.new$pi.lb <- with(dat.new, measure - 1.96*sqrt(pvar))
dat.new$pi.ub <- with(dat.new, measure + 1.96*sqrt(pvar))

# plot average values
library(ggplot2)
plot <- ggplot(data = dat.new, aes(x=time, y=measure))
plot <- plot + geom_line(data=dat.long, alpha = 0.02, aes(group=subject.id))
plot <- plot + geom_errorbar(width=0.02, color="red", aes(x=time-0.02, ymax=ci.ub, ymin=ci.lb))
plot <- plot + geom_line(color="red",linetype="dashed",aes(x=time-0.02))
plot <- plot + geom_point(size=3.5, color="red", fill="white", aes(x=time-0.02))
plot <- plot + geom_errorbar(width=0.02, color="blue",aes(x=time+0.02,ymax=pi.ub,ymin=pi.lb))
plot <- plot + geom_point(size=3.5,color="blue",fill="white",aes(x=time+0.02))
plot <- plot + theme_bw()
plot

# Apparently, the basic multilevel model is not quite enough to analyse our imaginary randomised controlled trial
# (RCT) data-set. 

# Incorporate treatment effect

model3 <- lmer(measure ~ time + tx + (1|subject.id), data = dat.long)
summary(model3)

# It is clear from the model summary that both time and tx effects are statistically significant 
# and participants in treatment B had a depression score 2.34 points lower than those in treatment A. 
# Should we conclude that treatment B is more effective? Definitely not. 
# The model is not a proper way to compare group difference in the context of longitudinal data

# The 'treatment effect' in m1 is in fact the the average treatment effect along time. 
# In other words, the coefficient -2.34 is the difference in depression score 
# between treatments A and B averaged in time 1, 2, and 3.

# We are actually not interested in knowing the average difference between the two treatments 
# but how the trajectories differ at time 2 and 3.

model4 <- lmer(measure ~ time*tx + (1|subject.id), data = dat.long)
summary(model4)

# The effect of interest is the time:txB interaction. 
# This term is a little bit tricky to interpret: 
# the coefficient value indicates the difference between the two treatments per unit time increment,
# conditional on average time and treatment effect

# For each unit time increase (e.g. from time 1 to time 2), 
# participants in treatment B could expect a 3.44 point lower depression score than those in treatment A 
# after accounting for the average treatment effect difference

# So at time 2, participants in treatment B could expect a -3.44(2) + 5.22 = -1.66 score difference 
# and at time 3, participants in treatment B could expect a -3.44(3) + 5.22 = -5.10

anova(model1, model3, model4)

p <- ggplot(data = dat.long, aes(x=time, y=measure, group=subject.id))
p <- p + geom_point()
p <- p + geom_line(alpha=0.1)
p <- p + facet_grid(.~tx)
p <- p + stat_summary(aes(group=1), geom="line", fun.y=mean, size=1, color='red')
p

q <- ggplot(data = dat.long, aes(x=jitter(time), y=measure, group=subject.id))
q <- q + geom_point()
q <- q + geom_line(alpha=0.1)
q <- q + facet_grid(.~tx)
q <- q + geom_smooth(aes(group=1))
q

r <- ggplot(data = dat.long, aes(x=time, y=measure, group=subject.id))
r <- r + geom_line(alpha=0.1)
r <- r + stat_smooth(aes(group=1), method="lm")
r <- r + stat_summary(aes(group=1), geom="point", fun.y=mean, shape=17,size=3, color='red')
r <- r + facet_grid(.~tx)
r

#####################################################################################################################
# R Textbook Examples
# Applied Longitudinal Data Analysis: Modeling Change and Event Occurrence
# By Judith D. Singer and John B. Willett
# Chapter 2: Exploring Longitudinal Data on Change
#####################################################################################################################
library(tibble)
library(dplyr)
tolerance <- read.csv("http://www.ats.ucla.edu/stat/r/examples/alda/data/tolerance1.txt")
glimpse(tolerance)
tbl_df(tolerance)

tolerance.pp <- read.csv("http://www.ats.ucla.edu/stat/r/examples/alda/data/tolerance1_pp.txt")
tbl_df(tolerance.pp)

# Bivariate correlation among tolerance scores assessed on five occassions

corred <- cor(tolerance[,c("tol11","tol12","tol13","tol14","tol15")])
library(corrplot)
corrplot.mixed(corred)

library(lattice)
xyplot(tolerance ~ age | id,
       data=tolerance.pp,
       prepanel = function(x, y){prepanel.loess(x, y, family="gaussian")},
       xlab = "Age", ylab = "Tolerance",
       panel = function(x, y) {
         panel.xyplot(x, y)
         panel.loess(x,y, family="gaussian") 
         },
       ylim=c(0, 4), 
       as.table=T
       )

# Fitting separate 'within person' OLS regressions

by(tolerance.pp, tolerance.pp$id, function(x){summary(lm(tolerance ~ time, data=x))})

# Stem plots for the intercepts

int <- by(tolerance.pp, tolerance.pp$id, function(x){coefficients(lm(tolerance ~ time, data = x))[[1]]})
int < unlist(int)
names(int) <- NULL
summary(int)
stem(int, scale=2)

# stem plot for fitted rate of change

rate <- by(tolerance.pp, tolerance.pp$id, function(x){coefficients(lm(tolerance ~ time, data = x))[[2]]})
rate < unlist(rate)
names(rate) <- NULL
summary(rate)
stem(rate, scale=2)

# stem plot for R^2

rsq <- by(tolerance.pp, tolerance.pp$id, function(x){summary(lm(tolerance ~ time, data = x))$r.squared})
rsq <- unlist(rsq)
names(rsq) <- NULL
summary(rsq)
stem(rsq, scale=2)

# fitted OLS models superimposed on empirical growth plots

xyplot(tolerance ~ age|id, 
       data = tolerance.pp,
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)
       },
       ylim=c(0,4),
       as.table=TRUE
       )

# plot of raw data

interaction.plot(tolerance.pp$age, tolerance.pp$id, tolerance.pp$tolerance)

p <- ggplot(data = tolerance.pp, aes(x=time, y=tolerance, group=id))
p <- p + geom_point()
p <- p + geom_line(alpha=0.2)
p <- p + facet_grid(.~male)
p <- p + stat_summary(aes(group=1), geom="line", fun.y=mean, size=1, color='red')
p

q <- ggplot(data = tolerance.pp, aes(x=time, y=tolerance, group=id))
q <- q + geom_point()
q <- q + geom_line(alpha=0.2)
q <- q + facet_grid(.~male)
q <- q + geom_smooth(group=1)
q

# fit linear model by id
fit <- by(tolerance.pp, tolerance.pp$id, function(x){fitted.values(lm(tolerance ~ time, data = x))})
fit <- unlist(fit)

interaction.plot(tolerance.pp$age, tolerance.pp$id, fit, xlab="age",ylab="tolerance")

# OLS estimates plotted against the predictors male and exposure

par(mfrow=c(1,2))
plot(tolerance$male, int, 
     xlab="Male", 
     ylab="Fitted initial status",
     xlim=c(0,1),ylim=c(0.5,2.5))

cor(tolerance$male, int)

plot(tolerance$exposure, int,
     xlab="Exposure",
     ylab="Fitted initial status",
     xlim=c(0.0, 2.5),
     ylim=c(0.5, 2.5))
par(mfrow=c(1,1))

######################################################################
# Chapter 4: Doing Data Analysis with the Multilevel Model for Change
######################################################################

alcohol1 <- read.table("http://www.ats.ucla.edu/stat/r/examples/alda/data/alcohol1_pp.txt", header=TRUE, sep=",")
glimpse(alcohol1)

xyplot(alcuse ~ age | id,
       data = alcohol1,
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)
       },
       ylim=c(-1,4),
       as.table=TRUE
       )

# Fitted OLS trajectories displayed separately by coa status and peer levels

r <- ggplot(data = alcohol1, aes(x=age, y=alcuse, group=id))
r <- r + geom_line(alpha=0.2)
r <- r + stat_smooth(aes(group=1), method="lm")
r <- r + stat_summary(aes(group=1), geom="point", fun.y=mean, shape=17,size=3, color='red')
r <- r + facet_grid(.~coa)
r

# Model A
library(nlme)
model.a <- lme(alcuse ~ 1, random=~1|id, data=alcohol1)
summary(model.a) # AIC=679.0049, BIC=689.5087, logLik=-336.5025

# Model B
model.b <- lme(alcuse ~ age_14, data=alcohol1, random = ~age_14|id, method="ML")
summary(model.b) # AIC=648.6111, BIC=669.6431, logLik=-318.3055

# Model C
model.c <- lme(alcuse ~ coa*age_14, data=alcohol1, random = ~age_14|id, method="ML")
summary(model.c) # AIC=637.2026, BIC=665.2453, logLik=-310.6013

# Model D
model.d <- lme(alcuse ~ coa*age_14 + peer*age_14 , data=alcohol1, random= ~ age_14 | id, method="ML")
summary(model.d) # AIC=608.6906, BIC=643.744, logLik=-294.3453

# Model E
model.e <- lme(alcuse ~ coa + peer*age_14 , data=alcohol1, random= ~ age_14 | id, method="ML")
summary(model.e) # AIC=606.7033, BIC=638.2513, logLik=-294.3516

# Model F
model.f <- lme(alcuse ~ coa + cpeer*age_14 , data=alcohol1, random= ~ age_14 | id, method="ML")
summary(model.f) # AIC=606.7033, BIC=638.2513, logLik=-294.3516

# Model G
model.g <- lme(alcuse ~ ccoa + cpeer*age_14 , data=alcohol1, random= ~ age_14 | id, method="ML")
summary(model.g) # AIC=606.7033, BIC=638.2513, logLik=-294.3516

#### Model B plotting and toying

fixef.b <- fixef(model.b)
fit.b <- fixef.b[[1]] + alcohol1$age_14[1:3]*fixef.b[[2]]

plot(alcohol1$age[1:3], fit.b, 
     ylim=c(0, 2), 
     type="b", 
     ylab="predicted alcuse", 
     xlab="age",
     main="Model B \n Unconditional growth model")

#### Model C plotting and toying

fixef.c <- fixef(model.c)
fit.c0 <- fixef.c[[1]] + alcohol1$age_14[1:3]*fixef.c[[3]]
fit.c1 <- fixef.c[[1]] + fixef.c[[2]] + alcohol1$age_14[1:3]*fixef.c[[3]] + alcohol1$age_14[1:3]*fixef.c[[4]]

plot(alcohol1$age[1:3], fit.c0,
     ylim=c(0, 2), 
     type="b", 
     ylab="predicted alcuse", 
     xlab="age",
     main="Model C \n Uncontrolled effects of COA")
lines(alcohol1$age[1:3], fit.c1, type="b", pch=17)
legend(14, 2, c("COA=0", "COA=1"))

#### Model E

fixef.e <- fixef(model.e)

fit.ec0p0 <- fixef.e[[1]] + 
  0.655*fixef.e[[3]] + 
  alcohol1$age_14[1:3]*fixef.e[[4]] + 
  0.655*alcohol1$age_14[1:3]*fixef.e[[5]]

fit.ec0p1 <- fixef.e[[1]] + 
  1.381*fixef.e[[3]] + 
  alcohol1$age_14[1:3]*fixef.e[[4]] + 
  1.381*alcohol1$age_14[1:3]*fixef.e[[5]] 

fit.ec1p0 <- fixef.e[[1]] + 
  fixef.e[[2]] + 
  0.655*fixef.e[[3]] + 
  alcohol1$age_14[1:3]*fixef.e[[4]] + 
  0.655*alcohol1$age_14[1:3]*fixef.e[[5]] 

fit.ec1p1 <- fixef.e[[1]] + 
  fixef.e[[2]] + 
  1.381*fixef.e[[3]] +
  alcohol1$age_14[1:3]*fixef.e[[4]] + 
  1.381*alcohol1$age_14[1:3]*fixef.e[[5]]

plot(alcohol1$age[1:3], fit.ec0p0,
     ylim=c(0, 2), 
     type="b", 
     ylab="predicted alcuse", 
     xlab="age",
     main="Model E \n *Final* model for the controlled effects of COA",
     pch=2)
lines(alcohol1$age[1:3], fit.ec0p1, type="b", pch=0)   
lines(alcohol1$age[1:3], fit.ec1p0, type="b", pch=17)   
lines(alcohol1$age[1:3], fit.ec1p1, type="b", pch=15) 
legend(14, 2, c("COA=0, low peer", "COA=0, high peer", 
                "COA=1, low peer", "COA=1, high peer"))

#########################################
# Chapter 5: Treating TIME More Flexibly
#########################################

reading <- read.table("http://www.ats.ucla.edu/stat/r/examples/alda/data/reading_pp.txt", header=T, sep=",")
glimpse(reading)
reading <- tbl_df(reading)
reading

xyplot(piat~age | id, 
       data=reading,
       panel=function(x,y, subscripts){
         panel.xyplot(x, y, pch=16)
         panel.lmline(x,y, lty=4)
         panel.xyplot(reading$agegrp[subscripts], y, pch=3)
         panel.lmline(reading$agegrp[subscripts], y) 
         }, 
       ylim=c(0, 80), 
       as.table=T, 
       subscripts=T)

reading %>% 
  mutate(agegrp.c=agegrp-6.5) %>%
  mutate(age.c=age-6.5) -> reading

r <- ggplot(data = reading, aes(x=agegrp.c, y=piat, group=id))
r <- r + geom_line(alpha=0.1)
r <- r + stat_smooth(aes(group=1), method="lm")
r <- r + stat_summary(aes(group=1), geom="point", fun.y=mean, shape=17,size=3, color='red')
r

q <- ggplot(data = reading, aes(x=agegrp.c, y=piat, group=id))
q <- q + geom_point()
q <- q + geom_line(alpha=0.1)
q <- q + geom_smooth(group=1)
q

#### modeling

lme.agegrp <- lme(piat ~ agegrp.c, random = ~agegrp|id, data = reading, method="ML")
summary(lme.agegrp) # AIC=1831.949, BIC=1853.473, logLik = -909.9746

#Using the age variable.
lme.age <- lme(piat ~ age.c, random= ~ age | id, data = reading, method="ML")
summary(lme.age) # AIC=1815.896, BIC=1837.419, logLik = -901.9478

#####################
# Non-linear Change # 
#####################

summary(model.e)
#obtaining the fixed effects parameters 
fixef.e <- fixef(model.e)
#obtaining the predicted values and squaring them
fit2.ec0p0 <- (fixef.e[[1]] + .655*fixef.e[[3]] +
                 alcohol1$age_14[1:3]*fixef.e[[4]] +
                 .655*alcohol1$age_14[1:3]*fixef.e[[5]])^2   
fit2.ec0p1 <- (fixef.e[[1]] + 1.381*fixef.e[[3]] +
                 alcohol1$age_14[1:3]*fixef.e[[4]] +
                 1.381*alcohol1$age_14[1:3]*fixef.e[[5]] )^2
fit2.ec1p0 <- (fixef.e[[1]] + fixef.e[[2]] + .655*fixef.e[[3]] +
                 alcohol1$age_14[1:3]*fixef.e[[4]] +
                 .655*alcohol1$age_14[1:3]*fixef.e[[5]] )^2
fit2.ec1p1 <- (fixef.e[[1]] + fixef.e[[2]] + 1.381*fixef.e[[3]] +
                 alcohol1$age_14[1:3]*fixef.e[[4]] +
                 1.381*alcohol1$age_14[1:3]*fixef.e[[5]])^2

plot(alcohol1$age[1:3], fit2.ec0p0, 
     ylim=c(0, 3), 
     type="n", 
     ylab="predicted alcuse squared", 
     xlab="age",
     main="Non-linear Change")
lines(spline(alcohol1$age[1:3], fit2.ec0p0, method="natural"), type="b", pch=2)
lines(spline(alcohol1$age[1:3], fit2.ec0p1, method="natural"), type="b", pch=0)   
lines(spline(alcohol1$age[1:3], fit2.ec1p0, method="natural"), type="b", pch=17)   
lines(spline(alcohol1$age[1:3], fit2.ec1p1, method="natural"), type="b", pch=15)
legend(14, 3, c("COA=0, low peer", "COA=0, high peer", 
                "COA=1, low peer", "COA=1, high peer"))

#### Fox and Geese game data

fg <- read.table("http://www.ats.ucla.edu/stat/r/examples/alda/data/foxngeese_pp.txt", header=T, sep=",")
tbl_df(fg)

# Empirical growth plots for 8 children in the fox and geese data.

xyplot(nmoves ~ game | id, 
       data=fg,
       ylim=c(0, 25), 
       as.table=T)

q <- ggplot(data = fg, aes(x=game, y=nmoves, group=id))
q <- q + geom_point()
q <- q + geom_line(alpha=0.1)
q <- q + geom_smooth(group=1)
q

model.a <- nlme(nmoves~ 1 + 19/ (1 + xmid*exp( -scal*game + u)),
                fixed=scal+xmid~1, random= scal+u~1 |id, 
                start=c(scal=.2, xmid=12), data=fg)

summary(model.a)

##########################################################################
# Chapter 7: Examining the multilevel model's error covariance structure #
##########################################################################

opposites <- read.table("http://www.ats.ucla.edu/stat/r/examples/alda/data/opposites_pp.txt", header=TRUE, sep=",")
tbl_df(opposites)

r <- ggplot(data = opposites, aes(x=time, y=opp, group=id))
r <- r + geom_line(alpha=0.1)
r <- r + stat_smooth(aes(group=1), method="lm")
r <- r + stat_summary(aes(group=1), geom="point", fun.y=mean, shape=17,size=3, color='red')
r

q <- ggplot(data = opposites, aes(x=time, y=opp, group=id))
q <- q + geom_point()
q <- q + geom_line(alpha=0.1)
q <- q + geom_smooth(group=1)
q

opp.reml <- lme(opp~time*ccog, data=opposites, random= ~time | id)
summary(opp.reml)

corandcov <- function(glsob,cov=TRUE,...){
  corm <- corMatrix(glsob$modelStruct$corStruct)[[5]]
  print(corm)
  varstruct <- print(glsob$modelStruct$varStruct)  
  varests <- coef(varstruct, uncons=F, allCoef=T)
  covm <- corm*glsob$sigma^2*t(t(varests))%*%t(varests)
  return(covm)
}

unstruct <- gls(opp ~ time*ccog, 
                data=opposites, 
                correlation=corSymm(form=~1|id),
                weights=varIdent(form=~1|wave),
                method="REML")
summary(unstruct)
corandcov(unstruct)

comsym <- gls(opp ~ time*ccog,
              data=opposites, 
              correlation=corCompSymm(form = ~ 1 |id), 
              method="REML")
cc <- corMatrix(comsym$modelStruct$corStruct)[[5]]
print(cc)

cc*comsym$sigma^2

heterocom <- gls(opp ~ time*ccog,
                data=opposites, 
                correlation=corCompSymm(form = ~ 1 |id),
                weights=varIdent(form = ~1|wave),
                method="REML")
corandcov(heterocom)

auto1 <- gls(opp ~ time*ccog,
             data=opposites, 
             correlation=corAR1(form = ~ 1 |id), 
             method="REML")
cc <- corMatrix(auto1$modelStruct$corStruct)[[5]]
print(cc)

cc * auto1$sigma^2

hauto1 <- gls(opp~time*ccog,
              data=opposites, 
              correlation=corAR1(form = ~ 1 |id), 
              weights=varIdent(form = ~1|wave), method="REML")
corandcov(hauto1)

toep <- gls(opp~time*ccog,
            data=opposites, 
            correlation=corARMA(form = ~ 1 |id, p=3, q=0), 
            method="REML")
cc <- corMatrix(toep$modelStruct$corStruct)[[5]]
print(cc)

cc*toep$sigma^2

anova(unstruct, comsym, heterocom, auto1, hauto1, toep)
anova(comsym, auto1, toep)

# Standard error covariance structure, conditional growth model
randSlope <- lme(opp ~ time*ccog, data=opposites, random =  ~ time | id)

# Conditional growth model: dropping random slope
randInt <- lme(opp ~ time*ccog, data=opposites, random =  ~ 1 | id)

# Conditional growth model: dropping random intercept
randSlope.noint <- lme(opp ~ time*ccog, data=opposites, random =  ~ 0 + time | id)

# Conditional growth model: dropping intercept-slope covariance
no.int.slope.covar <- lme(opp ~ time*ccog, data=opposites, random = list(id=pdDiag(~time)))

anova(randSlope, randInt, randSlope.noint, no.int.slope.covar)
# randSlope is best

mod1 <- lme(opp ~ time*ccog, 
            data = opposites, 
            random= ~ time|id,
            correlation=corAR1())

mod2 <- lme(opp ~ time*ccog, 
            data = opposites, 
            random= ~ time|id,
            weights=varIdent(form=~1|time),
            correlation=corAR1())

anova(mod1, mod2) # mod1 better

#Standard error covariance structure
summary(lme(opp ~ time * ccog,
            data=opposites, 
            random =  ~ time | id))

#Unstructured error covariance structure
summary(lme(opp ~ time*ccog, 
            data=opposites, 
            random =  ~ time | id, 
            correlation = corSymm(form =  ~ wave | id))) # Lower AIC

######## Orthodont examples

library(nlme)
tbl_df(Orthodont)

plot(Orthodont)

xyplot(distance ~ age | Subject, 
       data=Orthodont,
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)
       },
       as.table=TRUE)

r <- ggplot(data = Orthodont, aes(x=age, y=distance, group=Subject))
r <- r + geom_line(alpha=0.1)
r <- r + stat_smooth(aes(group=1), method="lm")
r <- r + stat_summary(aes(group=1), geom="point", fun.y=mean, shape=17,size=3, color='red')
r

q <- ggplot(data = Orthodont, aes(x=age, y=distance, group=Subject))
q <- q + geom_point()
q <- q + geom_line(alpha=0.1)
q <- q + geom_smooth(group=1)
q

### Orthodont is "groupedData", thus lme will fit random intercept and slope by subject by default ###
lme(distance ~ age, data = Orthodont, method="ML")
### We can define our own random effects ###
mod1 <- lme(distance ~ age + Sex, random = ~ 1, data = Orthodont) # just an intercept
mod2 <- lme(distance ~ age + Sex, random = list(Subject = ~ 1),data=Orthodont) #or like this, if not groupedData
mod3 <- lme(distance ~ age + Sex, random = list(Subject = ~ 1 + age), data = Orthodont) # random intercept and slope

anova(mod1, mod2, mod3) # random intercept more appropriate

### Example mgcv: gamm() for additive mixed models in R ###
library(mgcv)

plot(Soybean)
Soy <- Soybean 
Soy$logweight <- log(Soy$weight)

xyplot(logweight ~ Time | Plot, 
       data=Soy,
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)
       },
       as.table=TRUE)

r <- ggplot(data = Soy, aes(x=Time, y=logweight, group=Plot))
r <- r + geom_line(alpha=0.1)
r <- r + stat_smooth(aes(group=1), method="lm")
r <- r + stat_summary(aes(group=1), geom="point", fun.y=mean, shape=17,size=3, color='red')
r

q <- ggplot(data = Soy, aes(x=Time, y=logweight, group=Plot))
q <- q + geom_point()
q <- q + geom_line(alpha=0.1)
q <- q + geom_smooth(group=1)
q

g1 <- gamm(logweight ~ Year + Variety +  s(Time, bs = "ps", m = c(2,1), k = 20),
           random = list(Plot = ~ 1 + Time), 
           data = Soy, 
           method = "REML") 
# allow for an overall smooth function in time (same shape for all plots),
# but allow intercept and slope to differ by plot (randomly)
g1
plot(g1$gam)

g2 <- gamm(logweight ~ Year + Variety +  s(Time, bs = "ps", m = c(2,1), k = 20),
           random = list(Plot = ~ 1), 
           data = Soy, 
           method = "REML")  
# allow for an overall smooth function in time (same shape for all plots),
# but allow the intercept to differ by plot (randomly)
g2
plot(g2$gam)

###################################
# Generalized Linear Mixed Models #
###################################

### Simulate Poisson Data with Random Intercept ###

n <- 100
n.i <- 8
subject <- rep(1:n, each=n.i)
random.intercept <- rep(rnorm(n), each=n.i)
time <- rep(1:n.i, n)
beta.0 <- beta.1 <- 1
lambda <- exp(beta.0 + random.intercept + beta.1*time)
Y <- rpois(n*n.i, lambda)

data <- data.frame(cbind(subject, time, Y))

# model fitting
glmer(Y ~ time + (1|subject), family=poisson, data = data) #Laplacian approximation
glmer(Y ~ time + (1|subject), family=poisson, data=data, nAGQ=20)

### Seizure data discussed also in Example 8.5 / 9.5 in Diggle, Heagerty, Liang & Zeger (2002) ###
library(geepack)
tbl_df(seizure)

### Reshape data set, one line per subject and time point ###
seiz.long <- reshape(seizure, 
                     varying=c("base","y1","y2","y3","y4"),
                     v.names="y",
                     times=0:4,
                     direction="long")
seiz.long <- arrange(seiz.long, id)

seiz.long$t <- ifelse(seiz.long$time==0, 8, 2) # length of time period (8 weeks for baseline, 2 weeks else)
seiz.long$x <- ifelse(seiz.long$time==0, 0, 1) # mark baseline

tbl_df(seiz.long)

xyplot(y ~ time | id, 
       data=seiz.long,
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.lmline(x,y)
       },
       as.table=TRUE)

r <- ggplot(data = seiz.long, aes(x=time, y=y, group=id))
r <- r + geom_line(alpha=0.1)
r <- r + stat_smooth(aes(group=1), method="lm")
r <- r + stat_summary(aes(group=1), geom="point", fun.y=mean, shape=17,size=3, color='red')
r <- r + facet_grid(.~trt)
r

q <- ggplot(data = seiz.long, aes(x=time, y=y, group=id))
q <- q + geom_point()
q <- q + geom_line(alpha=0.1)
q <- q + geom_smooth(group=1)
q <- q + facet_grid(.~trt)
q

### fit generalized linear mixed model ###

### parameter of interest: beta for x:trt, the interaction between baseline and treatment ###
### - is the change since baseline different in the two treatment groups? ###
### correct for different lengths of time periods with an offset, log(t) ###
### allow for individual differences in seizure rates (random intercept) ###

glm.seiz <- glmer(y ~ offset(log(t)) + x + trt + x:trt + (1|id), family="poisson", nAGQ=20, data=seiz.long)

summary(glm.seiz)

### GEE modeling

library(geepack)
gee.seiz1 <- geese(y ~ offset(log(t)) + x + trt + x:trt, 
                   id=id, 
                   data=seiz.long, 
                   corstr="exch",
                   family=poisson)
summary(gee.seiz1)

gee.seiz2 <- geese(y ~ offset(log(t)) + x + trt + x:trt, 
                   id=id, 
                   data=seiz.long,
                   subset=id!=49, 
                   corstr="exch",
                   family=poisson)
summary(gee.seiz2)

#####################
# Transition models #
#####################

### read in respiratory infection data ###
xerop <- read.table(file="http://faculty.washington.edu/heagerty/Books/AnalysisLongitudinal/xerop.data",
                    sep=" ",
                    header=FALSE)

colnames(xerop) <- c("ID", 
                     "respinf",
                     "intercept",
                     "age", 
                     "xerophthalmia", 
                     "cos.time",
                     "sin.time", 
                     "sex",
                     "height.for.age",
                     "stunted","time", 
                     "baseline.age",
                     "season", 
                     "time2")
tbl_df(xerop)
glimpse(xerop)

xyplot(respinf ~ time | ID, 
       data=xerop[xerop$ID %in% c(121013,121113,121114,121215,121316,121414),],
       panel=function(x,y){
         panel.xyplot(x,y)
         panel.spline(x,y)
       },
       as.table=TRUE)

r <- ggplot(data = xerop, aes(x=time2, y=respinf, group=ID))
r <- r + geom_line(alpha=0.1)
r <- r + stat_smooth(aes(group=1), method="lm")
r <- r + stat_summary(aes(group=1), geom="point", fun.y=mean, shape=17,size=3, color='red')
r <- r + facet_grid(~xerophthalmia)
r

### reshape data set to fit first and second-order transition models ###
xerop <- arrange(xerop, ID, time)

# xerop$respinf.lag1 <- xerop$respinf[c(1, 1:(nrow(xerop)-1))]
# xerop$respinf.lag2 <- xerop$respinf[c(50:51, 1:(nrow(xerop)-2))]

xerop %>% 
  mutate(respinf.lag1=lag(respinf, n=1, default=0),        # for first order transition model
         respinf.lag2=lag(respinf, n=2, default=0),        # for second order
         age2=age-36,
         season2=(season==2),
         time.lag1=lag(time, n=1, default=0) + 1 - time,   # for first order transition model
         time.lag2=lag(time, n=2, default=0) + 2 - time) -> xerop  # for second order

xerop$ID.lag1 <- xerop$ID[c(50,1:(nrow(xerop)-1))] - xerop$ID
xerop %>% filter(time.lag1==0 & ID.lag1==0) -> xerop1

xerop$ID.lag2 <- xerop$ID[c(50:51,1:(nrow(xerop)-2))] - xerop$ID
xerop %>% filter(time.lag2==0 & ID.lag2==0 & time.lag1==0 & ID.lag1==0) -> xerop2

glimpse(xerop)
glimpse(xerop1)
glimpse(xerop2)

### zero-order transition model (assuming independence) ###
transmod1 <- glm(respinf ~ xerophthalmia ,family=binomial, data=xerop)
summary(transmod1)

### first-order transition models ###
transmod2 <- glm(respinf ~ xerophthalmia + respinf.lag1 + respinf.lag1*xerophthalmia, family=binomial, data=xerop1)
summary(transmod2)
transmod3 <- glm(respinf ~ xerophthalmia + respinf.lag1, family=binomial, data=xerop1)
summary(transmod3)
transmod4 <- glm(respinf ~ xerophthalmia + respinf.lag1 + age + season2 + respinf.lag1*xerophthalmia + respinf.lag1*age + respinf.lag1*season2, 
                 family=binomial, 
                 data=xerop1)
summary(transmod4)
transmod5 <- glm(respinf ~ xerophthalmia + respinf.lag1 + age + season2, family=binomial, data=xerop1)
summary(transmod5)

anova(transmod2, transmod3, transmod4, transmod5)

### Models are numbered as in Diggle et al (2002) ###
### Note that here only model-based standard errors are given. ###
### Robust standard errors are close, see Diggle et al (2002). ###

### second-order transition models ###
transmod6 <- glm(respinf ~ xerophthalmia + respinf.lag1+ respinf.lag2 + age + season2, family=binomial, data=xerop2)
summary(transmod6)

#################################################
# multi-state modelling with R: the msm package #
#################################################

# Sharples et al.[32] studied the progression of coronary
# allograft vasculopathy (CAV), a post-transplant deterioration of the arterial walls, using these data.
# Risk factors and the accuracy of the screening test were investigated using multi-state Markov and
# hidden Markov models.

# The first three patient histories are shown below. There are 622 patients in all. PTNUM is the
# subject identifier. Approximately each year after transplant, each patient has an angiogram, at which
# CAV can be diagnosed. The result of the test is in the variable state, with possible values 1,
# 2, 3 representing CAV-free, mild CAV and moderate or severe CAV respectively. A value of 4 is
# recorded at the date of death. years gives the time of the test in years since the heart transplant.
# Other variables include age (age at screen), dage (donor age), sex (0=male, 1=female), pdiag
# (primary diagnosis, or reason for transplant - IHD represents ischaemic heart disease, IDC represents
# idiopathic dilated cardiomyopathy), cumrej (cumulative number of rejection episodes), and
# firstobs, an indicator which is 1 when the observation corresponds to the patient's transplant 
# (the first observation), and 0 when the observation corresponds to a later angiogram.

# A useful way to summarise multi-state data is as a frequency table of pairs of consecutive states.
# This counts over all individuals, for each state r and s, the number of times an individual had an
# observation of state r followed by an observation of state s. The function statetable.msm can
# be used to produce such a table, as follows,

statetable.msm(state=state, subject=PTNUM, data=cav)

# To tell msm what the allowed transitions of our model are, we define a matrix of the same size as
# Q, containing zeroes in the positions where the entries of Q are zero. All other positions contain an
# initial value for the corresponding transition intensity. The diagonal entries supplied in this matrix do
# not matter, as the diagonal entries of Q are defined as minus the sum of all the other entries in the row.
# This matrix will eventually be used as the qmatrix argument to the msm function

Q <- rbind(c(0, 0.25, 0, 0.25),
           c(0.166, 0, 0.166, 0.166),
           c(0, 0.25, 0, 0.25),
           c(0, 0, 0, 0))

Q.crude <- crudeinits.msm(state ~ years, PTNUM, data = cav, qmatrix=Q)

#################################
#     Time-series Modeling      #
#################################

# In order to work with a time series in R, you have to place it into a time-series object
# 
# A vector of numbers, or a column in a data frame, can be saved as a time-series
# object using the ts() function
#
# myseries <- ts(data, start=, end=, frequency=)
# 
# frequency indicates the number of observations per unit time for example, frequency=1 for annual data, 
# frequency=12 for monthly data, and frequency=4 for quarterly data)

sales <- c(18, 33, 41, 7, 34, 35, 24, 25, 24, 21, 25, 20,
           22, 31, 40, 29, 25, 21, 22, 54, 31, 25, 26, 35)

tsales <- ts(sales, start=c(2003, 1), frequency=12)

plot(tsales, type="o",pch=19)
start(tsales)
end(tsales)

# subset

tsales.subset <- window(tsales, start=c(2003,5), end=c(2004,6))
tsales.subset

par(mfrow=c(2,1))
plot(tsales, type="o",pch=19, main="original")
plot(tsales.subset, type="o",pch=19, main="window")

#=========== Smoothing and seasonal decomposition ============#

#### Smoothing with simple moving averages

# Consider the Nile time series. It records the annual flow of the river Nile at Ashwan from 1871-1970

# Time series typically have a significant irregular or error component. In order to
# discern any patterns in the data, you'll frequently want to plot a smoothed curve that
# damps down these fluctuations. One of the simplest methods of smoothing a time
# series is to use simple moving averages. For example, each data point can be replaced
# with the mean of that observation and one observation before and after it. This is
# called a centered moving average

# St = (Y_t-q + ... + Y_t ... + Y_t+q) / (2q + 1)

# where St is the smoothed value at time t and k = 2q + 1 is the number of observations
# that are averaged. The k value is usually chosen to be an odd number (3 in this example). 
# By necessity, when using a centered moving average, you lose the (k - 1) / 2 observations 
# at each end of the series.

# Several functions in R can provide a simple moving average, including SMA() in
# the TTR package, rollmean() in the zoo package, and ma() in the forecast package.
# Here, you'll use the ma() function to smooth the Nile time series that comes with the
# base R installation

library(forecast)

old.opts <- options()
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(Nile), max(Nile))
plot(Nile, main="Raw Time Series")
plot(ma(Nile, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(Nile, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(Nile, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)

pacman::p_load(TTR)
pacman::p_load(zoo)
par(mfrow=c(2,2))
plot(Nile, main="Raw Time Series")
plot(ma(Nile, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(SMA(Nile, 3), main="Simple Moving Averages (k=3), TTR", ylim=ylim)
plot(rollmean(Nile, 3), main="Simple Moving Averages (k=3), ZOO", ylim=ylim)
# pretty much the same

####### Seasonal decomposition

# Time-series data that have a seasonal aspect (such as monthly or quarterly data) can
# be decomposed into a trend component, a seasonal component, and an irregular
# component. The trend component captures changes in level over time. The seasonal component
# captures cyclical effects due to the time of year. The irregular (or error) component
# captures those influences not described by the trend and seasonal effects.

# The decomposition can be additive or multiplicative. In an additive model, the
# components sum to give the values of the time series. Specifically,
# 
#   Y_t = Trend_t + Seasonal_t + Irregular_t
# 
# where the observation at time t is the sum of the contributions of the trend at time t,
# the seasonal effect at time t, and an irregular effect at time t.
# In a multiplicative model, given by the equation
# 
#   Y_t = Trend_t * Seasonal_t * Irregular_t

# A popular method for decomposing a time series into trend, seasonal, and irregular
# components is seasonal decomposition by loess smoothing. In R, this can be
# accomplished with the stl() function. The format is
#   stl(ts, s.window=, t.window=)

# where ts is the time series to be decomposed, s.window controls how fast the seasonal
# effects can change over time, and t.window controls how fast the trend can change
# over time. Smaller values allow more rapid change. Setting s.window="periodic"
# forces seasonal effects to be identical across years. Only the ts and s.window parameters
# are required

# The time series AirPassengers comes with a base R installation and describes the
# monthly totals (in thousands) of international airline passengers between 1949 and
# 1960

par(mfrow=c(1,1))

# AirPassengers
attributes(AirPassengers)
plot(AirPassengers)

lAirPassengers <- log(AirPassengers)
plot(lAirPassengers, ylab="log(AirPassengers")

fit <- stl(lAirPassengers, s.window="period")
plot(fit)

fit$time.series # based on the log version

exp(fit$time.series) # to get on original scale

# Two additional graphs can help to visualize a seasonal decomposition. They're created
# by the monthplot() function that comes with base R and the seasonplot() function
# provided in the forecast package

par(mfrow=c(2,1))
# The month plot displays the subseries for each month (all January values
# connected, all February values connected, and so on), along with the average of each
# subseries
monthplot(AirPassengers)
# The season plot displays the subseries by year
seasonplot(AirPassengers, year.labels="TRUE", main="")

#============= Exponential forecasting models =============#

# Exponential models are some of the most popular approaches to forecasting the
# future values of a time series. They're simpler than many other types of models, but
# they can yield good short-term predictions in a wide range of applications. 

# They differ from each other in the components of the time series that are modeled. A simple
# exponential model (also called a single exponential model) fits a time series that has a
# constant level and an irregular component at time i but has neither a trend nor a seasonal
# component. A double exponential model (also called a Holt exponential smoothing)
# fits a time series with both a level and a trend. Finally, a triple exponential model (also
# called a Holt-Winters exponential smoothing) fits a time series with level, trend, and seasonal
# components.

# Exponential models can be fit with either the HoltWinters() function in the base
# installation or the ets() function that comes with the forecast package.

# ets(ts, model="ZZZ") 

# the model is specified by three letters. The first letter
# denotes the error type, the second letter denotes the trend type, and the third letter
# denotes the seasonal type. Allowable letters are A for additive, M for multiplicative, N
# for none, and Z for automatically selected

# ets(ts, model="ANN") <- simple with level parameter
# ets(ts, model="AAN") <- double with level and slope param
# ets(ts, model="AAA") <- triple with level, slope and seasonal parameters

#### simple exponential smoothing

# Simple exponential smoothing uses a weighted average of existing time-series values to
# make a short-term prediction of future values. 

# The simple exponential smoothing model assumes that an observation in the time
# series can be described by
#   Y_t = level + irregular_t
# The prediction at time Yt+1 (called the 1-step ahead forecast) is written as
#   Y_t+1 = c_0*Y_t + c_1*Y_t-1 + c_2*Y_t-2 + c_2*Y_t-2 + ...
# where c_i = a(1-a)^i, i = 0, 1, 2, ... and 0 <= a <= 1
# The c_i eights sum to one, and the
# 1-step ahead forecast can be seen to be a weighted average of the current value and all
# past values of the time series

# The alpha (a) parameter controls the rate of decay for
# the weights. The closer alpha is to 1, the more weight is given to recent observations.
# The closer alpha is to 0, the more weight is given to past observations. The actual
# value of alpha is usually chosen by computer in order to optimize a fit criterion. A
# common fit criterion is the sum of squared errors between the actual and predicted
# values
# 
# The nhtemp time series contains the mean annual temperature in degrees Fahrenheit
# in New Haven, Connecticut, from 1912 to 1971

par(mfrow=c(1,1))
plot(nhtemp)

fit <- ets(nhtemp, model="ANN")
fit

forecast(fit, 1) # 1-step ahead
forecast(fit, 2)

par(mfrow=c(1,1))
plot(forecast(fit, 1),
     xlab="Year",
     ylab=expression(paste("Temperature (", degree*F,")",)),
     main="New Haven Annual Mean Temperature, one-step")
plot(forecast(fit, 2),
     xlab="Year",
     ylab=expression(paste("Temperature (", degree*F,")",)),
     main="New Haven Annual Mean Temperature, two-steps")

# The forecast package also provides an accuracy() function that displays the most
# popular predictive accuracy measures for time-series forecasts
accuracy(fit)
# The mean error and mean percentage error may not be that useful, because positive and negative errors can cancel out. 
# The RMSE gives the square root of the mean square error
# The mean absolute percentage error reports the error as a percentage of the time-series values. 
# It's unit-less and can be used to compare prediction accuracy across time series. 
# But it assumes a measurement scale with a true zero point (for example, number of passengers per day). 
# Because the Fahrenheit scale has no true zero, you can't use it here

###### Holt and Holt-Winters Exponential Smoothing

# The Holt exponential smoothing approach can fit a time series that has an overall
# level and a trend (slope). The model for an observation at time t is
# Y_t = level + slope*t + irregular_t
#
# The Holt-Winters exponential smoothing approach can be used to fit a time series
# that has an overall level, a trend, and a seasonal component. Here, the model is
#     Y_t = level + slope*t + s_t + irregular_t
# s_t represents the seasonal influence at time t. 

# The code in the following listing applies the Holt-Winters exponential smoothing approach 
# to predicting the next five values of the AirPassengers time series

par(mfrow=c(1,1))
fit <- ets(log(AirPassengers), model = "AAA")
fit
accuracy(fit)

pred <- forecast(fit, 5)
pred

plot(pred, 
     main="Forecast for Air Travel",
     ylab="Log(AirPassengers)",
     xlab="Time")

pred$mean <- exp(pred$mean)
pred$lower <- exp(pred$lower)
pred$upper <- exp(pred$upper)
p <- cbind(pred$mean, pred$lower, pred$upper)
dimnames(p)[[2]] <- c("mean", "Lo 80", "Lo 95", "Hi 80", "Hi 95")
p

###### The ets() function and automated forecasting

# you can invoke the ets() function to automatically select a best-fitting
# model for the data. Let's fit an automated exponential model to the Johnson & Johnson
# data described in the introduction to this chapter. The following code allows the
# software to select a best-fitting model.

fit <- ets(JohnsonJohnson)
fit

plot(forecast(fit), 
     main="Johnson & Johnson Forecasts",
     ylab="Quarterly Earnings (Dollars)", 
     xlab="Time", 
     flty=2)

# Because no model is specified, the software performs a search over a wide array of
# models to find one that minimizes the fit criterion (log-likelihood by default). The
# selected model is one that has multiplicative trend, seasonal, and error components.
# The plot, along with forecasts for the next eight quarters

#======================ARIMA forecasting models========================#

# In the autoregressive integrated moving average (ARIMA) approach to forecasting, predicted
# values are a linear function of recent actual values and recent errors of prediction
# (residuals)

# limit discussion to ARIMA models for non-seasonal time series

# Autocorrelation measures the way observations in a time series relate to each other.
# AC_k is the correlation between observatons Y_t and observations k periods earlier Y_t-k
# Plotting these correlations AC_1, AC_2, and AC_k produces an autocorrelation function plot (ACF)
# ACF plot used to:
#   1. select appropriate parameteres for the ARIMA model
#   2. Assess fit of final model

par(mfrow=c(1,2))
acf(Nile, main="stats") # stats package
Acf(Nile, main="forecast") # forecast

# A partial autocorrelation is the correlation between Y_t
# and Y_t-k with the effects of all Y
# values between the two (Yt-1, Yt-2,..., Y_t-k+1) removed 
# Partial autocorrelations can also be plotted for multiple values of k

stats::pacf(Nile, main="stats")
forecast::Pacf(Nile, main="forecast")

# The PACF plot is also used to
# determine the most appropriate parameters for the ARIMA model

# ARIMA models are designed to fit stationary time series (or time series that can be
# made stationary). In a stationary time series, the statistical properties of the series
# don't change over time. For example, the mean and variance of Y_t are constant. Additionally,
# the autocorrelations for any lag k don't change with time

# It may be necessary to transform the values of a time series in order to achieve constant
# variance before proceeding to fitting an ARIMA model. The log transformation is
# often useful here. Because stationary time series are assumed to have constant means, they can't have
# a trend component. Many non-stationary time series can be made stationary through 
# differencing. In differencing, each value of a time series Y_t is replaced with Y_t-1 - Y_t. Differencing
# a time series once removes a linear trend. Differencing it a second time
# removes a quadratic trend. A third time removes a cubic trend. It's rarely necessary to
# difference more than twice

ndiffs(Nile)
Nile.diff <- diff(Nile, 1)

plot(Nile, main="Nile")
plot(Nile.diff, main="Nile Differenced")

pacman::p_load(tseries)
tseries::adf.test(Nile)

# To summarize, ACF and PCF plots are used to determine the parameters of ARIMA
# models. Stationarity is an important assumption, and transformations and differencing
# are used to help achieve stationarity. With these concepts in hand, we can now
# turn to fitting models with an autoregressive (AR) component, a moving averages
# (MA) component, or both components (ARMA)

# In an autoregressive model of order p, each value in a time series is predicted from a linear
# combination of the previous p values

# In a moving average model of order q, each
# value in the time series is predicted from a linear combination of q previous errors

# Combining the two approaches yields an ARMA(p, q)
# that predicts each value of the time series from the past p values and q residuals

# An ARIMA(p, d, q) model is a model in which the time series has been differenced
# d times, and the resulting values are predicted from the previous p actual values and q
# previous errors. The predictions are "un-differenced" or integrated to achieve the final
# prediction

# The steps in ARIMA modeling are as follows:
# 1 Ensure that the time series is stationary.
# 2 Identify a reasonable model or models (possible values of p and q).
# 3 Fit the model.
# 4 Evaluate the model's fit, including statistical assumptions and predictive accuracy.
# 5 Make forecasts.

# 1 Ensure that the time series is stationary

Nile.diff <- diff(Nile, 1)

par(mfrow=c(2,1))
plot(Nile, main="Nile")
plot(Nile.diff, main="Nile Differenced")

adf.test(Nile)
# Dickey-Fuller = -3.3657, Lag order = 4, p-value = 0.0642
# alternative hypothesis: stationary
adf.test(Nile.diff)
# Dickey-Fuller = -6.5924, Lag order = 4, p-value = 0.01
# alternative hypothesis: stationary

par(mfrow=c(1,2))
Acf(Nile.diff, main="ACF")
Pacf(Nile.diff, main="PACF")

# find p, d, and q

# we know d = 1 from Ndiffs
# You get p and q by comparing the ACF and PACF plots
# guidelines:
# if ACF="Trials off to zero" AND PACF="Zero after lag p"   then ARIMA(p,d,0)
# if ACF="Zero after lag q" AND PACF="Trails off to zero"   then ARIMA(0,d,q)
# if ACF="Trails off to zero" AND PACF="Trails off to zero" then ARIMA(p,d,q)

fit_011 <- arima(Nile, order=c(p=0,d=1,q=1))
fit_011 # AIC=1269
fit_111 <- arima(Nile, order=c(p=1,d=1,q=1))
fit_111 # AIC=1267
fit_010 <- arima(Nile, order=c(p=0,d=1,q=0))
fit_010 # AIC=1296

accuracy(fit_011)
accuracy(fit_111)
accuracy(fit_010)

# evaluating fit

# If the model is appropriate, the residuals should be normally distributed with mean
# zero, and the autocorrelations should be zero for every possible lag

par(mfrow=c(2,2))
qqnorm(fit_011$residuals)
qqline(fit_011$residuals)
Box.test(fit_011$residuals, type="Ljung-Box")

qqnorm(fit_111$residuals)
qqline(fit_111$residuals)
Box.test(fit_111$residuals, type="Ljung-Box")

qqnorm(fit_010$residuals)
qqline(fit_010$residuals)
Box.test(fit_010$residuals, type="Ljung-Box")

# forecasting
par(mfrow=c(2,2))

forecast(fit_011, 3)
plot(forecast(fit_011,3), xlab="Year",ylab="Annual Flow")

forecast(fit_111, 3)
plot(forecast(fit_111,3), xlab="Year",ylab="Annual Flow")

forecast(fit_010, 3)
plot(forecast(fit_010,3), xlab="Year",ylab="Annual Flow")

# Auto ARIMA

auto.fit <- auto.arima(Nile)
auto.fit

plot(forecast(auto.fit, 3), xlab="Year",ylab="Annual Flow", main="Forecasts from auto ARIMA(1,1,1)")

# sunspot data
par(mfrow=c(1,1))
fit <- auto.arima(sunspots)
fit
plot(forecast(fit,10))

#############################################################################################################






#############################################################################################################

### RAPI - Rutgers Alcohol Problems Index
rapi.df <- read.csv("C:\\Users\\rrevans\\Downloads\\RAPI.Final_.csv", header = TRUE)
glimpse(rapi.df)

### TLFB - Daily drinks from the Time Line Follow-back
tlfb.df <- read.csv("C:\\Users\\rrevans\\Downloads\\TLFB.Final_.csv", header = TRUE)
glimpse(tlfb.df)

### Set categorical variables as factors
rapi.df <- within(rapi.df, {
  gender <- factor(gender, 0:1, c("Women","Men"))
})

tlfb.df <- within(tlfb.df, {
  gender <- factor(gender, 0:1, c("Women","Men"))
  weekend <- factor(weekend, 0:1, c("Weekday","Weekend"))
  fratsor <- factor(fratsor, 0:1, c("Not FratSor","FratSor"))
})

### Number of participants; both files are "long" files, hence the
### use of unique()
length(unique(tlfb.df$id)) # 980
length(unique(rapi.df$id)) # 818

### How much data per person?
table(table(tlfb.df$id)) # 719 have all 30 days
table(table(rapi.df$id)) # 561 have all 5 assessments

############# Figure 1 -- histogram of outcomes #############

### fit OLS to rapi with and without transformation
rapi.ols <- lm(rapi ~ gender*time, data = rapi.df)
rapi.ols.t <- lm(log(rapi+1) ~ gender*time, data = rapi.df)

par(mfrow=c(2,2))
plot(table(tlfb.df$drinks), lwd=3, ylab = "Frequency", 
     xlab = "Number of Drinks",
     main= "TLFB Data")
plot(table(rapi.df$rapi), lwd=3, ylab = "Frequency", 
     xlab = "Number of Alcohol Related Problems", 
     main = "RAPI Data")
hist(resid(rapi.ols), breaks = 25, col = grey(.6), xlab = "",
     main = "Residuals from OLS Regression \n of RAPI Data")
hist(resid(rapi.ols.t), breaks = 25, col = grey(.6), xlab = "",
     main = "Residuals from OLS Regression \n of RAPI Data With Log Transformation")

################### Basic Plots for TLFB #################################
#
### if not previously installed: install.packages("Hmisc")
library(Hmisc)

### M and SD by weekend and fraternity/sorority status
summary(drinks ~ weekend + fratsor, data = tlfb.df, 
        method = "cross",
        fun = function(x) round(smean.sd(x), 1))

### Data for plots of M and bootstrapped CI, separately for
### non-zero data (s.t1) and zero vs. non-zero (s.t2)
#
### Uses summarize() function in Hmisc package
s.t1 <- with(tlfb.df[tlfb.df$drinks > 0,], {
  Hmisc::summarize(drinks, llist(weekend, gender, fratsor), smean.cl.boot, conf.int = 0.95)
})
s.t1

s.t2 <- with(tlfb.df, {
  Hmisc::summarize(drinks > 0, llist(weekend, gender, fratsor), smean.cl.boot, conf.int = 0.95)
})
s.t2 # change name of column with mean
names(s.t2)[4] <- "drinks"

### Change some labels for better presentation in Figure
levels(s.t1$fratsor) <- c("Not in Fraternity or Sorority",
                          "Fraternity or Sorority")
levels(s.t2$fratsor) <- c("Not in Fraternity or Sorority",
                          "Fraternity or Sorority")

### To plot M and CI via lattice, we'll use the panel.errbars function
### from memisc package.
#
### install.packages("memisc")
library(memisc)

### Figure 2

panel.errbars1 <- function(x,y0,y1,ewidth=0){
  x <- as.numeric(x)
  offs <- ewidth/2
  panel.segments(x0=x,x1=x,y0=y0,y1=y1)
  panel.segments(x0=x-offs,x1=x+offs,y0=y0,y1=y0)
  panel.segments(x0=x-offs,x1=x+offs,y0=y1,y1=y1)
}

panel.errbars <- function(x,y,...,panel.xy=panel.xyplot,make.grid=c("horizontal","vertical","both","none"),ewidth=0){
  Y <- matrix(y,nrow=length(y)/3,ncol=3)
  y <- Y[,1]
  y0 <- Y[,2]
  y1 <- Y[,3]
  make.grid <- match.arg(make.grid)
  if(make.grid=="horizontal")
    panel.grid(h=-1,v=0)
  else if(make.grid=="vertical")
    panel.grid(v=-1,h=0)
  else if(make.grid=="both")
    panel.grid(v=-1,h=-1)
  panel.errbars1(x,y0,y1,ewidth=ewidth)       
  panel.xy(x,y,...)
}

### First, generate plots but don't actually plot, save to objects
ob1 <- xyplot(cbind(drinks, Lower, Upper) ~ weekend | fratsor, 
              data = s.t1, panel = panel.errbars, groups = gender, 
              ewidth=0.1, 
              par.settings = simpleTheme(pch=c(17,19), lwd=2, 
                                         cex = 1.1, col = c("black", grey(0.5))), 
              auto.key = list(x = 0.05, y = 0.85, lines = FALSE, 
                              columns = 1, border = TRUE,
                              file = "white"), 
              type = "b", 
              ylab = "Number of Drinks on Drinking Days", 
              xlab = "Weekday vs. Weekend")
ob1

ob2 <- xyplot(cbind(drinks, Lower, Upper) ~ weekend | fratsor, 
              data = s.t2, panel = panel.errbars, groups = gender, 
              ewidth=0.10, 
              par.settings = simpleTheme(pch=c(17,19), lwd=2, 
                                         cex = 1.1, col = c("black", grey(0.5))), 
              auto.key = list(x = 0.05, y = 0.85, lines = FALSE, 
                              columns = 1, border = TRUE,
                              file = "white"), 
              type = "b", 
              ylab = "Proportion Drinking", 
              xlab = "Weekday vs. Weekend")
ob2

pdf("thingywingy.pdf")
trellis.par.set(canonical.theme("postscript", color=FALSE))
print(ob1, split=c(1,1,1,2), more=TRUE)
print(ob2, split=c(1,2,1,2))
dev.off()

####################### Plots by individuals to visualize REs #################

### use lmList function
library(lme4)

### TLFB
#
### Create binary indicator of drinking or not
tlfb.df$drk.bin <- as.numeric(tlfb.df$drinks > 0)

### fit individual logistic regression by person
tlfb.bin.lis <- lmList(drk.bin ~ weekend | id, data = tlfb.df, family = binomial)
### NOTE: Series of error msgs from individual fits, which can 
###		  functionally be ignored; these relate to individuals
###		  with little data.
#
### Extract individual coefficients
tlfb.bin.cc <- coef(tlfb.bin.lis)

### Average of individual intercepts and slopes
colMeans(tlfb.bin.cc, na.rm = TRUE)

### Plot distributions
par(mfrow=c(1,2))
hist(plogis(tlfb.bin.cc[,1]), col = grey(.6), breaks=20)
hist(plogis(tlfb.bin.cc[,2]), col = grey(.6), breaks=20)
### NOTE: using plogis() which is inverse logit function
#
### Count regressions by individual
#
### NOTE: given that we are plotting only non-zero distribution, 
###		  these should technically be truncated Poisson regressions
###		  but for current purposes, we won't worry about it
tlfb.cnt.lis <- lmList(drinks ~ 1 | id,  data = filter(tlfb.df, drk.bin==1), family = poisson)
tlfb.cnt.cc <- coef(tlfb.cnt.lis)

### Plot intercepts
par(mfrow=c(1,1))
hist(exp(tlfb.cnt.cc[,1]), col = grey(.6), breaks=20)

tlfb.df$id <- factor(tlfb.df$id)
rnd <- sample(levels(tlfb.df$id), 8)

xyplot(drinks ~ day | id,
       data=tlfb.df,
       type=c("g","b"),
       layout=c(4,2,1),
       pch=16,
       ylab="Drinks",xlab="Time (days)",main="Trajectories for 6 Randomly Selected Individuals",
       subset= id %in% rnd)


p <- ggplot(data=tlfb.df, aes(x=day, y=drinks, group=id))
p <- p + geom_line(alpha=0.1)
p <- p + stat_smooth(aes(group=1), method="glm")
p <- p + stat_summary(aes(group=1), geom="point", fun.y=mean, shape=17, size=1, color="red")
p <- p + facet_grid(~weekend)
p

######################### Mixed-Effects Models ########################

### NOTE: In R, there are a variety of ways to fit mixed models, and
### generalized linear mixed models in particular.  We will primarily 
### focus on glmer() in the lme4 package.  This approach uses maximum
### likelihood as the underlying fitting approach.  Later, we will use
### Bayesian methods for some of the other models via MCMCglmm package,
### and we will show one or two other examples from different packages.

### GLMM - Poisson
library(lme4)

### Create variable to capture over-dispersion
tlfb.df$over <- 1:nrow(tlfb.df)

### R automatically generates contrasts for factor variables, so
### let's check what the contrasts are:
contrasts(tlfb.df$weekend)
contrasts(tlfb.df$gender) # Men coded 1
contrasts(tlfb.df$fratsor)

### TLFB - intercept only
dks.glmer <- glmer(drinks ~ weekend + gender + fratsor + (1 | id), 
                   data = tlfb.df,
                   nAGQ=5,
                   family = poisson)
summary(dks.glmer)
### NOTE: The syntax in (g)lmer is quite close to the regression
###		  equation itself, with outcome separated from predictors
###		  by the tilde (~).  Random-effects are noted by parentheses
###		  where the grouping factor comes after the pipe.

### Include (additive) over-dispersion term
dks.glmer.1 <- glmer(drinks ~ weekend + gender + fratsor + (1 | id) + (1 | over), 
                     data = tlfb.df, verbose = TRUE,
                     family = poisson)
summary(dks.glmer.1)
### NOTE: Warning msg about over-dispersion term can be ignored

### Include random effect for weekend
dks.glmer.2 <- glmer(drinks ~ weekend + gender + fratsor + (weekend | id) + (1 | over), 
                     data = tlfb.df,
                     glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                     family = poisson)



### Include all 2-way interactions and also time the fit
system.time(
  dks.glmer.3 <- glmer(drinks ~ (weekend + gender + fratsor)^2 + (weekend | id) + (1 | over), 
                       data = tlfb.df, 
                       glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)),
                       family = poisson)
)

### Compare the previous models with deviance tests and AIC/BIC
anova(dks.glmer, dks.glmer.1, dks.glmer.2, dks.glmer.3)

### NOTE: LR test and AIC like bigger model, BIC likes 
### 	  smaller model by a smidge

### Look at model summaries
print(dks.glmer.2, cor = FALSE)
print(dks.glmer.3, cor = FALSE)

### Main effects model suggests (not surprisingly) that men drink more
### than women, fraternity / sorority members drink more than those not
### in the greek system, and students drink more on weekends than weekdays

##################### Model Discrimination ###############################
#
### How many zeroes are predicted by model?
#
### For a count model without random-effects, we could simply use the
### fitted values to define the mean of a Poisson distribution and use
### dpois() to get the density at zero.
#
### With random-effects we need to average over the random-effects
### distribution, and we will use the simulate() command as one route
### to doing that.
#
### Simulate a 1000 new outcomes from the fitted model (~10 sec each)
dks.glmer.sim <- simulate(dks.glmer, nsim = 1000) # baseline model
dks.glmer.sim3 <- simulate(dks.glmer.3, nsim = 1000) # full model
head(dks.glmer.sim)

### Average over simulations to get predicted counts
out <- matrix(NA, ncol=2, nrow=101)
cnt <- 0:100

### There is likely a faster, more clever way to do this, but this works:
#
### Be patient, this takes a minute or so
for (i in 1:length(cnt)) {
  out[i,1] <- mean(sapply(dks.glmer.sim, FUN = function(x){sum(x == cnt[i])}))
  out[i,2] <- mean(sapply(dks.glmer.sim3, FUN = function(x){sum(x == cnt[i])}))
}
out

### Extra Figure - Raw counts with fitted counts from baseline and full
plot(table(tlfb.df$drinks[tlfb.df$drinks < 46]), 
     ylab = "Counts", xlab = "", 
     main = "TLFB with Fitted Counts", 
     lwd = 2)
lines(x = 0:44, y = out[1:45,1], lwd=2, lty = 2, col = "red")
lines(x = 0:44, y = out[1:45,2], lwd=2, lty = 3, col = "blue")
legend(x = 6, y = 15000, 
       legend = c("Random Intercept Only",
                  "Random Intercept, Slope,\n and Over-dispersion"),
       lwd = 3, lty = c(2,3), col = c("red","blue"))

### As a table
round(cbind(Obs = table(tlfb.df$drinks)[1:25], out[1:25,]), 2)

### NOTE: Relative to random-intercept model, the full model does a
###		  better job of reflecting the distribution of counts.  But,
###		  there is also notable lack of fit in low, non-zero counts

### Distribution of random-effects
#
### Get random-effects
tlfb.rr <- ranef(dks.glmer.3)
### See structure and note list of lists
str(tlfb.rr)

### NOTE: The structure of the ranef() output is as a list of lists,
###		  which is why the code below has [[num]][[num]] indexing
par(mfrow = c(3,2))
hist(tlfb.rr[[1]][[1]], main = "Over-dispersion", col = grey(0.60), xlab="")
qqnorm(tlfb.rr[[1]][[1]], main = "Over-dispersion")
qqline(tlfb.rr[[1]][[1]])

hist(tlfb.rr[[2]][[1]], main = "Intercept Variance", col = grey(0.60), breaks = 12, xlab="")
qqnorm(tlfb.rr[[2]][[1]], main = "Intercept Variance")
qqline(tlfb.rr[[2]][[1]])

hist(tlfb.rr[[2]][[2]], main = "Slope Variance", col = grey(0.60), breaks = 12, xlab="")
qqnorm(tlfb.rr[[2]][[2]], main = "Slope Variance")
qqline(tlfb.rr[[2]][[2]])

### NOTE: *Major* problem with over-dispersion variance; we should not
###		  trust results of this model and should look to hurdle/zero-inflated

################### Negative Binomial Mixed Models #################
#
### Both glmer() (above) and MCMCglmm() (below) include the 
### over-dispersed Poisson model, but do not include negative binomial
### mixed models.  There are several "recent additions" within R that
### will fit a negative binomial mixed model, and we include code 
### examples below.  
#
### gamlss package
library(gamlss.mx)

### Random-intercept negative binomial mixed model
tlfb.nbmix <- gamlssNP(drinks ~ gender*day,
                       data = tlfb.df,
                       random = ~ 1 | id,
                       family = NBI,
                       mixture = "gq", k = 20)
summary(tlfb.nbmix)

tlfb.nbmix2 <- gamlssNP(drinks ~ gender*day + weekend*day,
                       data = tlfb.df,
                       random = ~ 1 | id,
                       family = NBI,
                       mixture = "gq", k = 20)
summary(tlfb.nbmix2)

tlfb.nbmix3 <- gamlssNP(drinks ~ gender*day + weekend*day + fratsor*day,
                        data = tlfb.df,
                        random = ~ 1 | id,
                        family = NBI,
                        mixture = "gq", k = 20)
summary(tlfb.nbmix3)

### Random intecept and slope
tlfb.nbmix4 <- gamlssNP(drinks ~ gender*day,
                       data = tlfb.df,
                       random = ~ day | id,
                       family = NBI,
                       mixture = "gq", k = 20)
summary(tlfb.nbmix4)

tlfb.nbmix5 <- gamlssNP(drinks ~ gender*day + weekend*day,
                        data = tlfb.df,
                        random = ~ day | id,
                        family = NBI,
                        mixture = "gq", k = 20)
summary(tlfb.nbmix5)

tlfb.nbmix6 <- gamlssNP(drinks ~ gender*day + weekend*day + fratsor*day,
                        data = tlfb.df,
                        random = ~ day | id,
                        family = NBI,
                        mixture = "gq", k = 20)
summary(tlfb.nbmix6)

gam.list <- list(tlfb.nbmix, tlfb.nbmix2, tlfb.nbmix3, tlfb.nbmix4, tlfb.nbmix5, tlfb.nbmix6)
model.names <- c("Intercept-Gender", "Int-Gender+Weekend","Int-Gen+Week+Frat",
                 "Slope-Gender","Slope-Gender+Weekend","Slope-Gen+Week+Frat")
names(gam.list) <- model.names

sapl.func <- function(x){
  AIC=x$aic
  SBC=x$sbc
  return(list(AIC=AIC,SBC=SBC))
}
sapply(gam.list, sapl.func)

# Full Model Preferred by AIC and SBC

### glmmADMB package that links AD Model Builder to R
#
### It is not yet on CRAN, so we need to call the repository
### explicitly in the call to install.packages
install.packages("R2admb")
install.packages("glmmADMB", repos=c("http://glmmadmb.r-forge.r-project.org/repos", getOption("repos")),
                 type="source")
library(glmmADMB)
library(R2admb)

### Syntax is very similar to glmer() - Random intercept

tlfb.df$male <- ifelse(tlfb.df$gender=="Men", 1, 0)

tlfb.nbmix.1 <- glmmadmb(drinks ~ male*day + (1|id) + (day|id),
                         data = tlfb.df,
                         family = "nbinom",
                         verbose = TRUE,
                         zeroInflation=TRUE,
                         extra.args=100000)
summary(tlfb.nbmix.1)

rapi.nbmix2.1.2 <- glmmadmb(rapi ~ gender*time + (time|id),
                            data = rapi.df,
                            family = "nbinom", 
                            zeroInflation = TRUE,
                            verbose = TRUE)

############################ Bayesian ##################################
library(MCMCglmm)

### To fit zero-inflated and hurdle mixed models in R, we will use the
### MCMCglmm package.  These functions use a Bayesian approach based on 
### Markov chain Monte Carlo methods.  MCMC is a fundamentally different
### approach to statistical inference and model fitting, relative to 
### frequentist / maximum likelihood, which is what the lme4 package uses.
#
### The MCMCglmm package has 2 great resources for learning about its
### functions and Bayesian analyses more generally.  These two documents
### can be found as "vignettes" within the package, as well as at the
### CRAN homepage:
#
### http://cran.r-project.org/web/packages/MCMCglmm/index.html
#
### Bayesian analyses require "prior" distributions for all of the
### parameters.  For fixed-effects, normal distributions with wide
### variances are typically used.  For random-effects, the package
### uses inverse-Wishart distributions.
#
### We would *strongly* recommend some basic reading in Bayesian
### and MCMC methods prior to using the MCMCglmm package and functions
### described below.  Reasonable starting places are:
#
### Gelman, A., & Hill, J. (2007). Data analysis using regression and
### multilevel/hierarchical models.  New York: Cambridge.
#
### Lynch, S. (2007). Introduction to applied Bayesian statistics and 
### estimation for social scientists.  New York: Springer.
#
### let's examine the Bayesian approach to our earlier model for the
### RAPI:

### RAPI with Poisson-Normal model
rapi.mcmc2 <- MCMCglmm(rapi ~ gender*time, 
                       data = rapi.df, family = "poisson",
                       random = ~ us(1 + time):id,
                       nitt = 65000, burnin = 15000, thin = 50, # see below
                       verbose = TRUE, pr = TRUE, pl = TRUE)
summary(rapi.mcmc2)

### NOTE: With this example, the results between glmer and MCMCglmm are virtually identical

tlfb.mcmc <- MCMCglmm(drinks ~ male*day, 
                       data = tlfb.df, family = "poisson",
                       random = ~ us(1 + day):id,
                       thin=50,
                       verbose = TRUE, pr = TRUE, pl = TRUE)
summary(tlfb.mcmc)

### With maximum likelihood, the model fitting has a built-in stopping
### criterion -- that is, when the model's (log) likelihood value changes
### by a very small amount, we say that the model "converges" and results
### are then reported.
#
### With Bayesian models, we simulate from the posterior distribution of
### the parameters, but the number of simulations do not stop due to an
### internal (to the model fitting) criterion.  Instead, the analyst must
### specify the total number of iterations (via "nitt"), how many initial
### iterations are discarded (via "burnin"), and how often to save estimates
### for analysis (via "thin").
#
### After the model has been fit, we need to then evaluate whether the 
### estimates have converged to a stable answer, which can be assessed
### several different ways.
#
### The effective samples column ("eff.samp") in the summary output provides some
### evidence for convergence, and we can also examine traceplots via:

pdf('mcmcplots.pdf')
plot(rapi.mcmc2)
plot(tlfb.mcmc2)
dev.off()

### NOTE: For the random-effects it might be good to run the simulations
###		  for a longer time (eg, try uncommenting the line with nitt)
#
### Perhaps the best way to examine convergence of MCMC is to run
### multiple "chains" (really, just multiple fits) and compare the
### within to between chain variance.  some programs such as WinBUGS
### will allow you to specify a number of chains in a single call.
### with MCMCglmm, we need to run the model several times
tlfb.mcmc2.1 <- MCMCglmm(drinks ~ male*day, 
                         data = tlfb.df, family = "poisson",
                         random = ~ us(1 + day):id,
                         thin=50,
                         pr = TRUE, pl = TRUE)
tlfb.mcmc2.2 <- MCMCglmm(drinks ~ male*day, 
                         data = tlfb.df, family = "poisson",
                         random = ~ us(1 + day):id,
                         thin=50,
                         pr = TRUE, pl = TRUE)

### Pull out fixed-effects from 3 runs into list
tlfb.glmm.list.fe <- list(tlfb.mcmc$Sol[,1:4], 
                          tlfb.mcmc2.1$Sol[,1:4], 
                          tlfb.mcmc2.2$Sol[,1:4])
coda::gelman.diag(tlfb.glmm.list.fe)

### NOTE: gelman.diag() function is in the coda package, but gets loaded
###		  automatically with MCMCglmm
### NOTE: This reports the R-hat statistics, and Gelman & Hill suggest
###			values less than 1.10 imply that the chains have all converged
###			to the same posterior distributions 

### Each of these has 1,000 samples and we can use colMeans or
### posterior.mode() to get summaries
#
### For example, here are coefficient estimates and 95% CI for 
### fixed-effects (first 4 columns)
cbind(B = colMeans(tlfb.mcmc$Sol[,1:4]),
      CI = HPDinterval(tlfb.mcmc$Sol[,1:4]))

### Save random-effect intercepts and slopes
mcmc.int <- colMeans(tlfb.mcmc$Sol[,5:982])
mcmc.sl <- colMeans(tlfb.mcmc$Sol[,983:1964])

### Check distributional assumptions of random-effects
par(mfrow=c(2,2))
qqnorm(mcmc.int)
qqline(mcmc.int)
hist(mcmc.int, col = grey(.6), main = "Intercept")

qqnorm(mcmc.sl)
qqline(mcmc.sl)
hist(mcmc.sl, col = grey(.6), main = "Slope") # really bad

######################## TLFB Zero-Altered Mixed Model #######################

### MCMCglmm has 3 related families for fitting zero-altered models:
#
### zipoisson: zero-inflated (over-dispersed) Poisson
### hupoisson: hurdle (over-dispersed) Poisson
### zapoisson: zero-altered (over-dispersed) Poisson
#
### where the final model is similar to a hurdle model, but uses a 
### a complementary log-log link function for the logistic regression.
#
### NOTE: In the text we use "zero-altered" to refer generally to models
###		  that include a specific submodel for (extra / excess) zeroes, 
###		  whereas MCMCglmm refers to a specific model as a zero-altered
###		  model (i.e., a hurdle model using a complementary log-log link)
#
### From the MCMCglmm documentation
#------------------------------------------------------------------------------------
# The prior specification is passed to MCMCglmm by the prior argument. It takes
# a list of 3 elements: R, G and B, which specify the priors for the R-structure,
# G-structure and the fixed effects. G is also a list with an element for each
# random effect. The covariance matrices are assumed to be (conditional) inverseWishart
# distributed and individual elements for each variance structure take
# the arguments V, n and fix which specify the (co)variance matrix, the degree of
# freedom parameter, and the partition to condition on
# 
#
### To fit any of these three, we will need to explicitly set-up prior
### distributions, where a prior is a list which can take elements:
#
### B - priors for fixed-effects
### R - priors for residual variance (including over-dispersion)
### G - priors for random-effects
#
### Zero-altered models (generally) are a type of multivariate GLMM in that each
### outcome is transformed into 2 outcomes: 1) logit model, 2) count model
#
### Here is a general prior for zero-altered models with a random intercept
### term in each submodel:
zi.prior <-  list(R = list(V = diag(2), n = 1.002, fix = 2),
                  G = list(G1 = list(V = 1, n = 0.002),     
                           G2 = list(V = 1, n = 0.002)))

### This prior leaves the priors for fixed-effects at their default, but
### calls for a diagonal matrix for the residual variances -- that is, 
### the residual variances for the logit and Poisson models.  However, there
### is no observed residual variance for binary data, and thus we "fix" that
### value to a constant (ie, it is not estimated).  Note also that in these
### models the count portion is indexed first, and logit second (which is why
### fix=2 fixes the logit portion).
#
### The G list calls for 2 separate priors, each with one element, which is
### appropriate for a random-intercept in both the count and logit models.
#
### NOTE: Our data does not have missing values in it, but the current
###		  version of MCMCglmm wants missing data removed prior to fitting

### A couple comments on the syntax for MCMCglmm with multivariate outcomes:
#
### "trait" and "units" are special, internal variables within MCMCglmm:
#
### units refers to the residual variance(s).
#
### trait indexes columns of the outcome in a multivariate model.  
#
### Thus, by fitting interactions between trait and our fixed-effects 
### (and removing the overall intercept), we will get separate 
### fixed-effects for logit and count models.
#
### NOTE: It is a good idea *not* to have any variables named units or trait
#
### Somewhat similarly, the "rcov" argument is a formula for the residual
### variance; the syntax above asks for separate residual variances for each
### level of trait (ie, count and logit models), though we fixed the logit
### residual variance to a constant via our prior specification.
#
### The random statement includes several new functions:
#
### "idh" is for fitting a diagonal variance-covariance matrix (i.e., all off
### diagonal terms are set to zero, no covariances)
#
### "us" is for fitting unstructured variance-covariance matrices, and hence,
### our second model allows the random intercepts and slopes of the count
### model to be correlated.
#
### at.level() indexes the levels of a factor, and thus, at.level(trait, 1)
### is specifying the count portion of the model, and at.level(trait, 2) is
### specifying the logit portion of the model.

tlfb.zaglmm <- MCMCglmm(drinks ~ -1 + trait*((weekend + gender + fratsor)^2), 
                        data = tlfb.df, 
                        family = "zapoisson",
                        random = ~idh(at.level(trait,2)):id + idh(at.level(trait,1)):id,
                        rcov = ~ idh(trait):units,
                        prior = zi.prior,
                        #nitt = 500000, burnin = 100000, thin = 400,
                        verbose = TRUE, pr = TRUE, pl = TRUE)

summary(tlfb.zaglmm)

### Model including weekend random-effect in both parts of the 
###	model and all two-way interactions for fixed-effects
a.V <- matrix(c(100,0,0,100), ncol=2, byrow = TRUE)
zi.prior3 <-  list(R = list(V = diag(2), n = 1.002, fix = 2), # Residual variances, fix variance at 2
                   G = list(G1 = list(V = diag(2), n = 1.002, # Random effects
                                      alpha.mu = c(0,0),
                                      alpha.V = a.V), 
                            G2 = list(V = diag(2), n = 1.002,
                                      alpha.mu = c(0,0),
                                      alpha.V = a.V)))

### NOTE: It appears that weekend needs to be numeric in the random statement
tlfb.df <- within(tlfb.df, weekend.n <- as.numeric(weekend) - 1)

### NOTE: Given the size of the data and coplexity of the model, this takes a while to run
tlfb.zaglmm3 <- MCMCglmm(drinks ~ -1 + trait*((weekend + gender + fratsor)^2), 
                         data = tlfb.df, 
                         family = "zapoisson",
                         random = ~us(at.level(trait,1) + at.level(trait,1):weekend.n):id + 
                                   us(at.level(trait,2) + at.level(trait,2):weekend.n):id,
                         rcov = ~ idh(trait):units,
                         prior = zi.prior3,
                         #nitt = 500000, burnin = 100000, thin = 400,
                         verbose = TRUE, pr = TRUE, pl = TRUE)
summary(tlfb.zaglmm3)
pdf('tlfb_zaglmm3.pdf')
plot(tlfb.zaglmm3)
dev.off()

### NOTE: As with other zero-altered models above, we should run this
###		  longer and verify that parameters have converged
#
### As above with RAPI, the Sol component of the object only has the
### fixed-effects and random-effects - pull out first 14 columns
tlfb.cc <- cbind(post.mode=posterior.mode(tlfb.zaglmm3$Sol[,1:14]), 
                 HPDinterval(tlfb.zaglmm3$Sol[,1:14], .95))
tlfb.cc

### exponentiate and re-arrange terms so that all "za" terms are together
tlfb.cc <- data.frame(exp(tlfb.cc[c(1, 3:8, 2, 9:14),]))
round(tlfb.cc, 3)

#################### Marginal Predictions ##############################
#
### As noted in the text, fixed-effect coefficients from GLMM with
### non-identity link functions are "conditional" and do not average
### over the random-effects.  However, we can generate marginal
### predictions (that do average over random-effects) using the methods
### described in the technical appendix, which we show here.

### Create grid of covariate values
pred.grid <- expand.grid(weekend = levels(tlfb.df$weekend),
                         gender = levels(tlfb.df$gender),
                         fratsor = levels(tlfb.df$fratsor))
pred.grid

### Generate design matrix of fixed-effects
x.mat <- model.matrix(~ (weekend + gender + fratsor)^2, data = pred.grid)
x.mat

### Generate design matrix of random-effects, which is simply the
### first two columns of x.mat
z.mat <- x.mat[,1:2] # just intercept and weekend
z.mat

### Using MCMC output, extract estimate of variance-covariance matrix
### of random-effects
D.mcmc <- matrix(colMeans(tlfb.zaglmm3$VCV)[1:4], ncol = 2)
D.mcmc

### Over-dispersion variance
od.mcmc <- mean(tlfb.zaglmm3$VCV[,9])
od.mcmc

### Vector of fixed-effect coefficients
b.mcmc <- colMeans(tlfb.zaglmm3$Sol[,c(1,3:8)])
b.mcmc

### Predictions without random-effects (ie, these are "conditional")
cond.est <- exp(x.mat %*% b.mcmc)

### Marginal predictions
marg.est <- exp(x.mat %*% b.mcmc + diag(z.mat %*% D.mcmc %*% t(z.mat))/2 + od.mcmc/2)
marg.est

### Compare values
data.frame(pred.grid, cond.est, marg.est)

# One of the advantages of MCMC estimation is that the posterior draws can
# be combined in fairly arbitrary ways and summarized as we have done
# previously for coefficients.  Thus, to get point estimates and CI we can
# work directly with the full set of MCMC posterior draws.
#
### Count regression submodel

### Pull out vectors / matrices for coefficients, var-cov, and over-dispersion
tlfb.cnt.b <- tlfb.zaglmm3$Sol[,c(1,3:8)] # coefficients
tlfb.cnt.D <- tlfb.zaglmm3$VCV[,1:4] # var-cov
tlfb.cnt.od <- tlfb.zaglmm3$VCV[,9] # over-dispersion

dim(tlfb.cnt.b) # 1,000 x 7
dim(tlfb.cnt.D) # 1,000 x 4
head(tlfb.cnt.od)

### Each row represents a set of estimates, and so we will basically use the
### the same formula as above, but run it 1,000 times across the 1,000 rows
### of our MCMC posterior draws.
#
### Empty matrix to collect output
out.cnt <- matrix(NA, nrow=1000, ncol=8)

### Loop through rows of posterior draws, estimating marginal predictions
for (i in 1:nrow(out.cnt)) {
  D <- matrix(tlfb.cnt.D[i,], ncol=2)
  out.cnt[i,] <- exp(x.mat %*% tlfb.cnt.b[i,] + (diag(z.mat %*% D %*% t(z.mat)))/2 + tlfb.cnt.od[i]/2)	
}
head(out.cnt) # 1,000 sets of marginal predictions
colMeans(out.cnt) # means

### Get M and CI
tlfb.cc2 <- data.frame(cbind((colMeans(out.cnt)), (HPDinterval(as.mcmc(out.cnt)))))
tlfb.cc2

### Create factor with labels
tlfb.cc2 <- within(tlfb.cc2, {
  nms <- factor(c("Women / WD / No Frat",
                  "Women / WE / No Frat",
                  "Men / WD / No Frat",
                  "Men / WE / No Frat",
                  "Women / WD / Frat",
                  "Women / WE / Frat",
                  "Men / WD / Frat",
                  "Men / WE / Frat"))
})

### Use errbar function in Hmisc library to plot
library(Hmisc)

with(tlfb.cc2, {
  errbar(x = nms, y = V1, yplus = upper, yminus = lower,
         main = "TLFB: Count Submodel",
         ylab = "Predicted Drinks on Drinking Days")
})
