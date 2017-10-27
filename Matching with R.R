#================================
# Matching in R
#================================

library(wakefield) # random data sets
library(MatchIt) # matching
library(tableone) # cool tables

# generate random data
df.1 <- r_data_frame(n = 250,
                     age(x = 30:78,
                         name = 'Age'),
                     sex(x = c("Male", "Female"),
                         prob = c(0.70, 0.30),
                         name = "Sex"))
df.1$Sample <- as.factor('Patients')

df.2 <- r_data_frame(n = 1000,
                     age(x = 18:80,
                         name = 'Age'),
                     sex(x = c("Male", "Female"),
                         prob = c(0.50, 0.50),
                         name = "Sex"))
df.2$Sample <- as.factor('Population')

df.3 <- rbind(df.1, df.2)
df.3$Group <- as.logical(df.3$Sample == 'Patients')
# some continuous covariate [0:42]
df.3$X <- ifelse(df.3$Sex == 'Male',
                 age(nrow(df.3), x = 0:42, name = 'X'),
                 age(nrow(df.3), x = 15:42, name = 'X'))

matched <- matchit(Group ~ Age + Sex,
                   data = df.3,
                   method = "exact",
                   ratio = 1)

df.match <- match.data(matched)[1:ncol(df.3)] # save matched data

CreateTableOne(vars = c("Age", "Sex", "X"),
               data = df.match,
               factorVars = "Sex",
               strata = "Sample") # just see the output
