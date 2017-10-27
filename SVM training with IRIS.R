#===================================================#
# Using support vector machines as 'Flower Finders' # 
#===================================================#

# Nature field guides are filled with pictures of plants and animals that teach us
# what to look for and how to name what we see. For example, 
# a flower finder might display pictures of different iris species,
# such as the illustrations in the plot below. 
# You have in hand your own specimen from your garden, 
# and you carefully compare it to each of the pictures until you find a
# good-enough match. The pictures come from Wikipedia, 
# but the data used to create the plot are from the R dataset iris: 
#   sepal and petal length and width measured on 
# 150 flowers equally divided across three species.

library(e1071)

data(iris)

# Classification mode, using the default with factor response

model <- svm(Species ~ ., data = iris)
model
summary(model)

# - or - 

x <- subset(iris, select = -Species)
y <- iris$Species
model <- svm(x, y)
print(model)
summary(model)

# Testing with training data subset

pred <- predict(model, x) # == fitted(model)
pred

# Check predictive accuracy

table(pred, y)

# Computing decision values and probabilities

pred2 <- predict(model,x, decision.values = TRUE)
attr(pred2, "decision.values")[1:10, ]

# Visualize classes by color and support vector by crosses
# 
# Classical multidimensional scaling (MDS) of a data matrix. 
# Also known as principal coordinates analysis

plot(cmdscale(dist(iris[,-5])),
     col=as.integer(iris[,5]),
     pch=c("o","+")[1:150 %in% model$index + 1])

# We will focus on the last block of R code that generates the metric 
# multidimensional scaling (MDS) of the distances separating the 150 flowers 
# calculated from sepal and petal length and width 
# (i.e., the dist function applied to the first four columns of the iris data). 
# Species plays no role in the MDS with the flowers positioned in a 
# two-dimensional space in order to reproduce the pairwise Euclidean distances.
# However, species is projected onto the plot using color, 
# and the observations acting as support vectors are indicated with plus signs (+).
# 
# The setosa flowers are represented by black circles and black plus signs. 
# These points are separated along the first dimension from the versicolor 
# species in red and virginica in green. The second dimension, on the other hand, 
# seems to reflect some within-species sources of differences that do not 
# differentiate among the three iris types.
# 
# We should recall that the dist function calculates pairwise distances in the
# original space without any kernel transformation. The support vectors, 
# on the other hand, were identified from the svm function using a radial kernel 
# and then projected back onto the original observation space. Of course, 
# we can change the kernel, which defaults to "radial" as in this example 
# from the R package. A linear kernel may do just as well with the iris data, 
# as you can see by adding kernel="linear" to the svm function in the above code.

model <- svm(x, y, kernal = "linear")

plot(cmdscale(dist(iris[,-5])),
     col=as.integer(iris[,5]),
     pch=c("o","+")[1:150 %in% model$index + 1])

# These are the same
# 
# It appears that we do not need all 150 flowers in order to identify the iris 
# species. We know this because the svm function correctly classifies over 97% 
# of the flowers with 51 support vectors (also called "landmarks" as noted in my 
# last post Seeing Similarity in More Intricate Dimensions). The majority of 
# the +'s are located between the two species with the greatest overlap. 
# I have included the pictures so that the similarity of the red and 
# green categories is obvious. This is where there will be confusion, 
# and this is where the only misclassifications occur. 
# If your iris is a setosa, your identification task is relatively easy and 
# over quickly. But suppose that your iris resembles those in the cluster of
# red and green pluses between versicolor and virginica. This is where the 
# finer distinctions are being made.