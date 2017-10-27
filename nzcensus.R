# install the nzcensus package (note it is part of the nzelect GitHub repository):
# devtools::install_github("ellisp/nzelect/pkg2")

# load up packages:
library(leaflet)
library(nzcensus)

# remove Chatham Islands 
tmp <- AreaUnits2013[AreaUnits2013$WGS84Longitude > 0 & 
                     !is.na(AreaUnits2013$MedianIncome2013), ]

# create colour palette function
pal <- colorQuantile("RdBu", NULL, n = 10)

# create labels for popups
labs <- paste0(tmp$AU_NAM, " $", format(tmp$MedianIncome2013, big.mark = ","))


# draw map:
leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircles(lng = tmp$WGS84Longitude, lat = tmp$WGS84Latitude,
             color = pal(-tmp$MedianIncome2013),
             popup = labs,
             radius = 500) %>%
  addLegend(
    pal = pal,
    values = -tmp$MedianIncome2013,
    title = "Quantile of median<br>household income",
    position = "topleft",
    bins = 5)

#===================setup=======================
library(ggplot2)
library(scales)
library(boot)
library(dplyr)
library(Hmisc) # for spearman2
library(mgcv)  # for gam
library(caret) # for RMSE
library(grid)
library(stringr)
library(ggrepel)
library(glmnet)
library(maps) # for country borders on the smoothing plot

au <- AreaUnits2013 %>%
  # drop the columns with areas' code and name, 
  # and the redundant coordinate system
  select(-AU2014, -AU_NAM, -NZTM2000Easting, -NZTM2000Northing) %>%
  # drop some intrinsically collinear variables that would be exactly collinear
  # if it weren't for rounding error and confidentialisation:
  select(-PropWorked40_49hours2013, -Prop35to39_2013, -PropFemale2013)

# give meaningful rownames, helpful for some diagnostic plots later
row.names(au) <- AreaUnits2013$AU_NAM

# remove some repetition from the variable names
names(au) <- gsub("_2013", "", names(au))
names(au) <- gsub("2013", "", names(au))
names(au) <- gsub("Prop", "", names(au))

# restrict to areas with no missing data.  An improvement for later
# is to use imputation and include this step within the validation.
au <- au[complete.cases(au), ]

# give the data a generic name for ease of copying and pasting
the_data <- au

# we need a dummy variable for the Chathams because it's extreme value of long
# makes any spatial variables otherwise highly problematic.
# the_data$chathams <- the_data$WGS84Longitude < 100
the_data <- the_data[the_data$WGS84Longitude > 100, ]

names(the_data) <- make.names(names(the_data))

fit_lm <- function(data, i){
  mod1 <- lm(MedianIncome ~ ., data = data[i, ])
  # use the model based on resample on the original data to estimate how
  # good or not it is:
  RMSE(predict(mod1, newdata = data), data$MedianIncome)
}

# use bootstrap validation for an unbiaased estimate of root mean square error
rmses_lm_boot <- boot(the_data, statistic = fit_lm, R = 99)
lm_rmse <- mean(rmses_lm_boot$t)

# save a single version of this model for later
mod_lm <- lm(MedianIncome ~ ., data = the_data)

# Check out classic diagnostic plots:
par(mfrow = c(2, 2))
plot(mod_lm)

# A lasso, ridge rigression, or elastic net (which combines the two) is a way
# of dealing with the collinearity by forcing some coefficients to shrink 
# (possibly to zero) while doing minimal damage to the inferential qualities 
# of the rest and to the overall model fit.

# First we need to decide between ridge regression and lasso or elastic net (between the two)
# define folds for cross validation so can check the impact of different values of alpha 

foldid <- sample(1:10, nrow(the_data), replace = TRUE)

# separate out the explanatory from response variable
# for when using lasso and ridge regression
X <- the_data %>% select(-MedianIncome)
Y <- the_data$MedianIncome 

cv_results <- data_frame(lambda = numeric(),
 alpha = numeric(), 
 mcve = numeric()
 )
alphas <- seq(from = 0, to = 1, length.out = 9)

for(i in alphas){
  cvfit <- cv.glmnet(as.matrix(X), Y, foldid = foldid, alpha = i)
  tmp <- data_frame(lambda = cvfit$lambda, alpha = i, mcve = cvfit$cvm)   
  cv_results <- rbind(cv_results, tmp)
}

arrange(cv_results, mcve) 
# best alpha with this see is 0.75 but right combination of alpha and lambda
# works pretty well for any alpha

# For the graphic I take square root of mcve so it is back on same scale as RMSE used elsewhere in this post
cv_results %>%
  ggplot(aes(x = lambda, 
    y = sqrt(mcve), 
    colour = as.factor(round(alpha, 3)))
  ) %>%
  add(geom_line(size = 2)) %>%
  add(geom_line(size = 0.2, 
    colour = "grey50", 
    aes(group = as.factor(round(alpha, 3))))
  ) %>%
  add(scale_x_log10()) %>%
  add(coord_cartesian(ylim = c(1800, 4000),
                      xlim = c(5, 1000))) %>%
  add(scale_colour_brewer("alpha", 
    palette = "Greens", 
    guide = guide_legend(reverse = TRUE)
    )) %>%
  add(ggtitle("Cross-validation to select hyper parameters\nin elastic net regression")) %>%
  add(scale_y_continuous("Square root of mean cross validation error",
                         label = comma)) %>%
  add(theme(legend.position = "right"))

# Choosing 0.750 as the value of alpha and using cross-validation for each resampled dataset to choose lambda,
# we can now use bootstrap validation to check out the results of this modelling approach.

fit_elastic <- function(data, i){
  # i = sample(1:nrow(data), nrow(data), replace = TRUE)
  Y_orig <- data$MedianIncome
  X_orig <- as.matrix(select(data, -MedianIncome))
  data2 <- data[i, ]
  Y_new <- data2$MedianIncome
  X_new <- as.matrix(select(data2, -MedianIncome))
  lambda <- cv.glmnet(X_new, Y_new, alpha = 0.85)$lambda.min
  mod1 <- glmnet(X_new, Y_new, lambda = lambda, alpha = 0.85)
  # use the model based on resample on the original data to estimate how
  # good or not it is:
  rmse <- RMSE(predict(mod1, newx = X_orig), Y) 
  return(rmse)
}

rmses_elastic_boot <- boot(data = the_data, 
                           statistic = fit_elastic, R = 99) # takes a few minutes
elastic_rmse <- mean(rmses_elastic_boot$t)
elastic_rmse

sp <- spearman2(MedianIncome ~ ., data=the_data)
sp

the_formula <- terms(MedianIncome ~  
                       s(FullTimeEmployed, k = 6) + 
                       s(InternetHH, k = 6) +
                       s(NoQualification, k = 5) +
                       s(UnemploymentBenefit, k = 5) +
                       s(Smoker, k = 5) +
                       s(Partnered, k = 5)  +
                       s(Managers, k = 4) +
                       s(Bachelor, k = 4) +
                       s(SelfEmployed, k = 4) +
                       s(NoMotorVehicle, k = 4) +
                       s(Unemployed, k = 3) +
                       s(Labourers, k = 3) +
                       s(Worked50_59hours, k = 3) +
                       s(Separated, k = 3) +
                       s(Maori, k = 3) +
                       s(WGS84Longitude, WGS84Latitude) +
                       .,
                     data = the_data)

gam_model <- gam(the_formula, data = the_data)

# grid of nonlinear relations
par(bty = "l", mar = c(5,4, 2, 1))
plot(gam_model, 
  residuals = TRUE,
   pages = 1, 
   shade = TRUE,
   seWithMean = TRUE,
    ylab = "")
grid.text("Impact of area average variables on median income by area unit (New Zealand census 2013)", 
          0.5, 0.99,
          gp = gpar(fontfamily = "myfont"))

# map of spatial impacts		 
par(bty = "l", family = "myfont", fg = "white")
plot(gam_model,
 shade = TRUE, 
 select = 16, 
 rug = TRUE, 
 se = FALSE, 
 scheme = 2, 
 col = topo.colors(100), 
 pch = 1, 
 ylab = "Latitude", 
 xlab = "Longitude", 
 main = "Spatial pattern in regression of income\non demographic area variables")
map(add = TRUE, col = "grey75")

