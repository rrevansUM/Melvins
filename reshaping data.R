###############################################
# How to Reshape Data in R: tidyr vs reshape2 #
###############################################

# Here we consider the two packages tidyr and reshape2, 
# our aim is to see where their purposes overlap and where they differ by comparing the functions gather(), 
# separate() and spread(), from tidyr, with the functions melt(), colsplit() and dcast(), from reshape2

#=============== Data Tidying ==================#

# Data tidying is the operation of transforming data into a clear and simple form that makes it easy to work with. 
# "Tidy data" represent the information from a dataset as data frames where each row is an observation and each 
# column contains the values of a variable

####### Gather() vs. Melt()

library(tidyr)
library(reshape2)

# data set creation
set.seed(10)
messy <- data.frame(id = 1:4,
                    trt = sample(rep(c('control','treatment'), each=2)),
                    work.T1 = runif(4),
                    home.T1 = runif(4),
                    work.T2 = runif(4),
                    home.T2 = runif(4))
messy

# Our first step is to put the data in the tidy format, to do that we use tidyr's functions gather() and separate().
# Following Wickham's tidy data definition,
# this data frame is not tidy because some variable values are in the column names. 
# We bring this messy data frame from the wide to the long format by using the gather() function

messy %>% tidyr::gather(key=key, value=value, -id, -trt) -> gather.messy
gather.messy

# - or - 

messy %>%
  reshape2::melt(variable.name="key",
                 value.names="value",
                 id.vars = c("id","trt")) -> molten.messy
molten.messy
cbind(gather.messy, molten.messy) # same

# We now compare the two functions by running them over the data without any further parameter and see what happen:

tidyr::gather(messy) # bad
reshape2::melt(messy) # not so bad

# We see a different behaviour: gather() has brought messy into a long data format with a warning 
# by treating all columns as variable, while melt() has treated trt as an "id variables". 
# Id columns are the columns that contain the identifier of the observation that is represented as a row in 
# our data set. Indeed, if melt() does not receive any id.variables specification, then it will use the factor 
# or character columns as id variables. gather() requires the columns that needs to be treated as ids, 
# all the other columns are going to be used as key-value pairs.

# Despite those last different results, we have seen that the two functions can be used to perform the exactly 
# same operations on data frames, and only on data frames! Indeed, gather() cannot handle matrices or arrays, 
# while melt() can as shown below.

set.seed(3)
M <- matrix(rnorm(6),ncol=3)
dimnames(M) <- list(letters[1:2],letters[1:3])

reshape2::melt(M)
tidyr::gather(M) # error

######### Split a column: Separate() vs. colsplit()

# Our next step is to split the column key into two different columns in order to separate the location 
# and time variables and obtain a tidy data frame

tidy <- tidyr::separate(gather.messy,
                        key,
                        into = c("location","time"),
                        sep="\\.")
tidy

res.tidy <- cbind(molten.messy[1:2],
                  colsplit(molten.messy[,3],"\\.", c("location","time")),
                  molten.messy[4])
res.tidy

# Again, the result is the same but we need a workaround:
# because colsplit() operates only on a single column we use cbind() to insert the new two columns in the data frame. 
# separate() performs all the operation at once reducing the possibility of making mistake

############ From the long to the wide format: spread() vs dcast()

# Finally, we compare spread() with dcast() using the data frame example for the spread() documentation itself. 
# Briefly, spread() is complementary to gather() and brings data from the long to the wide format

set.seed(14)
stocks <- data.frame(time=as.Date('2009-01-01') + 0:9,
                     X=rnorm(10,0,1),
                     Y=rnorm(10,0,2),
                     Z=rnorm(10,0,4))
stocks

stocksm <- tidyr::gather(stocks, key=stock, value=price, -time) # to long
stocksm

spread.stock <- tidyr::spread(stocksm, stock, price) # back to wide
spread.stock

cast.stock <- reshape2::dcast(stocksm, formula = time ~ stock, value.var = "price")
cast.stock

# Again, the same result produced by spread() can be obtained using dcast() by specifying the correct formula

############# Data Aggregation

# Up to now we made reshape2 following tidyr, 
# showing that everything you can do with tidyr can be achieved by reshape2, too, 
# at the price of a some workarounds

# As we now go on with our simple example we will get out of the purposes of tidyr and have no more functions 
# available for our needs. Now we have a tidy data set - one observation per row and one variable per column - 
# to work with. We show some aggregations that are possible with dcast() using the tips data frame from reshape2. 
# Tips contains the information one waiter recorded about each tip he received over a period of a few months 
# working in one restaurant

glimpse(tips)
m.tips <- melt(tips)
glimpse(m.tips)

# We use dcast() to get information on the average total bill, tip and group size per day and time:

reshape2::dcast(m.tips, day + time ~ variable, mean)

# Averages per smoker or not in the group

reshape2::dcast(m.tips, smoker ~ variable, mean)

# There is no function in the tidyr package that allows us to perform a similar operation, 
# the reason is that tidyr is designed only for data tidying and not for data reshaping.
