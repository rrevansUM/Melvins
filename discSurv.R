if (!require(discSurv)) install.packages("discSurv")
library(discSurv)

if (!require(Ecdat)) install.packages("Ecdat")
library(Ecdat)

#===== contToDisc =====# 

# Discretizes continuous time variable into a specified grid of censored data for discrete survival
# analysis. It is a data preprocessing step, before the data can be extendend in long format and further
# analysed with discrete survival models.


# Example copenhagen stroke study data
if (!require(pec)) install.packages("pec")
library(pec)
data(cost)
head(cost)

# Convert observed times to months
# Right borders of intervals [0, a_1), [a_1, a_2), ... , [a_{\max-1}, a_{\max})
IntBorders <- 1:ceiling(max(cost$time) / 30) * 30

# Select subsample
subCost <- cost[1:100, ]
CostMonths <- contToDisc(dataSet = subCost, 
                         timeColumn = "time", 
                         intervalLimits = IntBorders)
head(CostMonths)

# Select subsample giving number of equidistant intervals
CostMonths <- contToDisc (dataSet = subCost, 
                          timeColumn = "time", 
                          intervalLimits = 10, 
                          equi = TRUE)
head(CostMonths)

#=========== dataCensoring ============#

# Function for transformation of discrete survival times in censoring encoding. Prior this function the
# data has to be already transformed to long format. With this new generated variable, the discrete
# censoring process can be analysed instead of the discrete survival process. In discrete survival
# analysis this information is used to constructs weights for predictive evaluation measures. It is
# applicable in single event survival analysis.

# Convert to long format based on months
CostMonthsLong <- dataLong(dataSet = CostMonths, 
                           timeColumn = "timeDisc", 
                           censColumn = "status")
head(CostMonthsLong, 20)

# Generate censoring process variable
CostMonthsCensor <- dataCensoring(dataSetLong = CostMonthsLong, 
                                  respColumn = "y",
                                  idColumn = "obj")
head(CostMonthsCensor)
tail(CostMonthsCensor[CostMonthsCensor$obj == 1, ], 10)
tail(CostMonthsCensor[CostMonthsCensor$obj == 3, ], 10)

#=========== dataLong ===========#

# Transform data from short format into long format for discrete survival analysis and right censoring.
# Data is assumed to include no time varying covariates, e. g. no follow up visits are allowed. It is
# assumed that the covariates stay constant over time, in which no information is available

# Example unemployment data
library(Ecdat)
data(UnempDur)

# Select subsample
subUnempDur <- UnempDur [1:100, ]
head(subUnempDur)

# Convert to long format
UnempLong <- dataLong (dataSet = subUnempDur, 
                       timeColumn = "spell", 
                       censColumn = "censor1")
head(UnempLong, 20)

# Is there exactly one observed event of y for each person?
splitUnempLong <- split(UnempLong, UnempLong$obj)
all(sapply(splitUnempLong, function (x) sum(x$y)) == subUnempDur$censor1)

# Second example: Acute Myelogenous Leukemia survival data
library(survival)

head(leukemia)
leukLong <- dataLong(dataSet = leukemia, 
                     timeColumn = "time", 
                     censColumn = "status")
head(leukLong, 30)

# Estimate discrete survival model
estGlm <- glm(formula = y ~ timeInt + x,
              data = leukLong, 
              family = binomial())
summary(estGlm)

# Estimate survival curves for non-maintained chemotherapy
newDataNonMaintained <- data.frame(timeInt = factor(1:161), 
                                   x = rep("Nonmaintained"))
predHazNonMain <- predict(estGlm, 
                          newdata = newDataNonMaintained, 
                          type = "response")
predSurvNonMain <- cumprod(1 - predHazNonMain)

# Estimate survival curves for maintained chemotherapy
newDataMaintained <- data.frame(timeInt = factor(1:161), 
                                x = rep("Maintained"))
predHazMain <- predict(estGlm, 
                       newdata = newDataMaintained, 
                       type = "response")
predSurvMain <- cumprod(1 - predHazMain)

# Compare survival curves
plot(x = 1:50, y = predSurvMain[1:50], 
     xlab = "Time", ylab = "S(t)", las = 1,
     type = "l", 
     main = "Effect of maintained chemotherapy on survival of leukemia patients")
lines(x = 1:161, y = predSurvNonMain, col = "red")
legend("topright", 
       legend = c("Maintained chemotherapy", "Non-maintained chemotherapy"),
       col = c("black", "red"),
       lty = rep(1, 2))
# The maintained therapy has clearly a positive effect on survival over the time range

#============== dataLongtTimeDep ===============#

# Transforms short data format to long format for discrete survival modelling of single event analysis
# with right censoring. Covariates may vary over time

# Example Primary Biliary Cirrhosis data
library(survival)

dataSet1 <- pbcseq

# Only event death is of interest
dataSet1$status[dataSet1$status == 1] <- 0
dataSet1$status[dataSet1$status == 2] <- 1
table(dataSet1$status)

# Convert to months
dataSet1$day <- ceiling(dataSet1$day / 30) + 1
names(dataSet1)[7] <- "month"

# Convert to long format for time varying effects
pbcseqLong <- dataLongTimeDep(dataSet = dataSet1, 
                              timeColumn = "month",
                              censColumn = "status", 
                              idColumn = "id")
pbcseqLong[pbcseqLong$obj==1, ]