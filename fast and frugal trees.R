#============================#
#    Fast and Frugal Trees   #
#============================#

# One popular class of simple decision rules are Fast and Frugal Trees 
# (FFTs, Gigerenzer & Todd, 1999). 
# Fast and frugal trees make very fast decisions based on a few (usually 1 to 5)
# pieces of information and ignore all other information.
# In other words, Fast and frugal trees are noncompensatory, 
# meaning that once they make a decision based on a few pieces of information, 
# no additional information can ever change the decision. 
# Because they are so simple to use, 
# they have been used in many real-world decision tasks from making coronary 
# artery disease diagnoses (Green & Mehr, 1997), to detecting depression 
# (Jenny, Pachur, Williams, Becker & Margraf, 2013). 
# However, lest you think that fast and frugal trees are only useful when time is
# limited, research has shown that fast and frugal trees can out-predict more 
# complex models in decidedly non-human simulations 
# (Gigerenzer, Czerlinski & Martignon, 1999).
# 
# The main function in the package is fft(), 
# which takes a standard formula and data argument, 
# and returns a fast and frugal tree (fft) object.
# From this object, you can view its underlying trees,
# along with many standard classification statistics
# (e.g.; hit-rate, false alarm rate, AUC) applied to both training and test
# (i.e.; prediction) datasets. Finally, the function has two alternative 
# classification algorithms, logistic regression and CART, built in, 
# so you can always compare the accuracy of your fast and frugal trees to two 
# gold-standards in the classification literature.

# install.packages("FFTrees")
library(FFTrees)

# The data we'll use comes from the Wisconsin Breast Cancer Database.
# The data is stored as a dataframe with 699 rows, representing 699 patients,
# and 10 columns. The 10 columns represent 9 physiological measurements, 
# from cell sizes to cell shapes, and 1 binary variable (diagnosis) 
# indicating whether the woman truly does, or does not have breast cancer. 
# Here is how the first few rows of the dataframe look:

breastcancer <- read.csv(file = "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data", sep=",", header=FALSE)

var.names <- c("ID", 
               "THICKNESS",
               "CELLSIZE.UNIF",
               "CELLSHAPE.UNIF",
               "ADHESION",
               "EPITHELIAL",
               "NUCLEI.BARE",
               "CHROMATIN",
               "NUCLEOLI",
               "MITOSES",
               "DIAGNOSIS")

colnames(breastcancer) <- var.names
breastcancer$DIAGNOSIS <- ifelse(breastcancer$DIAGNOSIS == 2, 0, 1)

head(breastcancer[,c(2:11)])

# To create a fast and frugal tree from the dataset, we'll use the fft() function,
# entering formula = diagnosis ~ ., meaning that we want to predict diagnosis as 
# a function of (potentially), all other variables, and data = breastcancer.
# We'll assign the result to a new object of class fft called breastcancer.fft

bc.fft <- fft(DIAGNOSIS ~ ., data = breastcancer[,c(2:11)])
bc.fft

# The printout tells us that the final fft object contains 6 different trees, 
# and the largest tree only uses 4 of the original 9 cues. 
# To see the best tree, we can simply plot the fft object:

plot(bc.fft, 
     main = "Breastcancer Fast and Frugal Trees",
     decision.names = c("Healthy","Cancer"))

# There's one of our fast and frugal trees! In the top section of the plot, 
# we see that the data had 444 true healthy cases, and 239 true cancer cases.
# In the middle section, we see the actual tree. The tree then starts by looking 
# at the cue cellsize.u. If the value is less than 3, the tree decides that the 
# person is healthy. If the value is not less than 3, 
# then the tree looks at cellshape. If the cellshape. <= 2, 
# the tree decides the patient is healthy. If cellshape. > 2, 
# the tree decides that the person does have cancer.

# how well does it perform? The bottom section of the above plot shows a series
# of performance statistics. On the bottom left hand corner, we can see a 
# classification table, which shows how the tree's decisions compare to the truth.
# Entries on the main diagonal (Cor Rej and Hit) correspond to correct decisions,
# while the other entries correspond to incorrect decisions. 
# As you can see, the tree performed exceptionally well:  
#   it made correct diagnoses in 646 (424 + 222) out of all 683 cases 
# (95% correct). Additional performance statistics, including specificity 
# (1 - false alarm rate), hit rate, d-prime, AUC (area under the curve) are
# also displayed. Finally, in the bottom right plot, 
# you can see an ROC curve which compares the performance of the trees 
# to CART (in red) and logistic regression (in blue).
# 
# Cross-validation: The fft() function allows you to easily create trees 
# from a training dataset, and test the performance of the trees with a test
# dataset (aka, cross-validation). You can do this by either entering an explicit 
# test dataset in the data.test argument, or by randomly splitting the main
# dataset into a separate training and testing sample with train.p. 
# For example, train.p = 0.5 will randomly split the data into a 
# 50% training set, which will be used to build the trees, and a 50% test set,
# which will be used to evaluate their prediction performance.

bc.test.fft <- fft(DIAGNOSIS ~ ., data = breastcancer, train.p = 0.5)
bc.test.fft
plot(bc.test.fft)
