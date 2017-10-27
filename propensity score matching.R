#================================#
# Propensity Score Matching in R #
#================================#

# 1. Propensity Scores

# Propensity score matching (PSM) is a "statistical matching technique that 
# attempts to estimate the effect of a treatment, policy, or other intervention 
# by accounting for the covariates that predict receiving the treatment". 
# In a broader sense, propensity score analysis assumes that an unbiased 
# comparison between samples can only be made when the subjects of both samples
# have similar characteristics. Thus, PSM can not only be used as 
# "an alternative method to estimate the effect of receiving treatment when random
# assignment of treatments to subjects is not feasible" (Thavaneswaran 2008).
# It can also be used for the comparison of samples in epidemiological studies. 
# Let's give an example:
# 
# Health-related quality of life (HRQOL) is considered an important outcome in 
# cancer therapy. One of the most frequently used instruments to measure HRQOL 
# in cancer patients is the core quality-of-life questionnaire of the European
# Organisation for Research and Treatment of Cancer. 
# The EORTC QLQ-C30 is a 30-item instrument comprised of five functioning scales,
# nine symptom scales and one scale measuring Global quality of life.
# All scales have a score range between 0 and 100. 
# While high scores of the symptom scales indicate a high burden of symptoms, 
# high scores of the functioning scales and on the GQoL scale indicate better 
# functioning resp. quality of life.

# 2. Creating two random dataframes

# Since we don't want to use real-world data, we need to emulate the data. 
# This can be easily done using the Wakefield package.
# 
# In the first step, we create a dataframe named df.patients.
# We want the dataframe to contain specifications of age and sex for 250 patients.
# The patients' age shall be between 30 and 78 years. 
# Furthermore, 70% of patients shall be male

library(wakefield)

df.patients <- wakefield::r_data_frame(
  n = 250,
  age(x = 30:78, name = "Age"),
  sex(x = c("Male", "Female"),
      prob = c(0.70, 0.30), name = "Sex")
  )

df.patients$Sample <- as.factor("Patients")

# The summary-function returns some basic information about the dataframe created.

summary(df.patients)

# As we can see, the mean age of the patient sample is 53.7 and 
# roughly 70% of the patients are male (69.2%).
# 
# In a second step, we create another dataframe named df.population. 
# We want this dataframe to comprise the same variables as df.patients 
# with different specifications. With 18 to 80, the age-range of the population 
# shall be wider than in the patient sample and the proportion of female and male
# patients shall be the same.

df.population <- wakefield::r_data_frame(
  n = 1000, 
  age(x = 18:80,name = 'Age'), 
  sex(x = c("Male", "Female"), 
      prob = c(0.50, 0.50),
      name = "Sex")
  )

df.population$Sample <- as.factor('Population')

# The following table shows the sample's mean age (49.5 years) and the proportion 
# of men (48.5%) and women (51.5%).

summary(df.population)

# 3. Merging the Dataframes

# Before we match the samples, we need to merge both dataframes. 
# Based on the variable Sample, we create a new variable named 
# Group (type *logic*) and a further variable (*Distress*) containing information 
# about the individuals' level of distress. The *Distress* variable is created 
# using the age-function of the Wakefield package. 
# As we can see, women will have higher levels of distress.

mydata <- rbind(df.patients, df.population)
mydata$Group <- as.logical(mydata$Sample == 'Patients')
mydata$Distress <- ifelse(mydata$Sex == 'Male', 
                          age(nrow(mydata), x = 0:42, name = 'Distress'), 
                          age(nrow(mydata), x = 15:42, name = 'Distress')
)

head(mydata)

# When we compare the distribution of age and sex in both samples, 
# we discover significant differences:

library(tableone)

table1 <- CreateTableOne(vars = c("Age","Sex","Distress"), 
                         data = mydata,
                         factorVars = "Sex", 
                         strata = "Sample"
                         )

table1 <- print(table1, printToggle = FALSE, noSpaces = TRUE)
table1
table1[, 1:3]

# Furthermore, the level of distress seems to be significantly higher in the 
# population sample.

# 4. Matching the Samples

# Now, that we have completed preparation and inspection of data,
# we are going to match the two samples using the matchit-function of the 
# MatchIt package. The method command method="nearest" specifies that the
# nearest neighbors method will be used. Other matching methods 
# are *exact matching*, *subclassification*, *optimal matching*, 
# *genetic matching*, and 
# *full matching* (method = c("exact", "subclass", "optimal", ""genetic", "full")).
# The ratio command ratio = 1 indicates a one-to-one matching approach. 
# With regard to our example, for each case in the patient sample exactly 
# one case in the population sample will be matched. Please also note that the 
# Group variable needs to be logic (TRUE vs. FALSE)

library(MatchIt)
match.it <- MatchIt::matchit(Group ~ Age + Sex, data = mydata, 
                             method = "nearest",
                             ratio = 1)
a <- summary(match.it)
a

# For further data presentation, we save the output of the summary-function
# into a variable named a.

# After matching the samples, the size of the population sample was reduced to 
# the size of the patient sample (n=250; see table 2).

a$nn

# The following output shows, that the distributions of the variables Age and 
# Sex are nearly identical after matching:

a$sum.matched[c(1,2,4)]

# The distributions of propensity scores can be visualized using the
# plot-function which is part of the MatchIt package

plot(match.it, type = "jitter", interactive = FALSE)

# 4. Saving the Matched Samples

df.match <- match.data(match.it)[1:ncol(mydata)]
rm(df.patients, df.population)

# Eventually, we can check whether the differences in the level of distress
# between both samples are still significant.

table4 <- CreateTableOne(vars = c('Age', 'Sex', 'Distress'),  
                         data = df.match, 
                         factorVars = 'Sex', 
                         strata = 'Sample')

print(table4,  printToggle = FALSE,  noSpaces = TRUE)

