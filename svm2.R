setwd("~/projects/school/ML/ML_Project")

## the SVM is located in two different packages: one of them is 'e1071'
library(e1071)

beers.df <- read.csv("beers.csv")
dataset <- subset(beers.df, select = c(IBU, ABV, Size.L., OG, BoilSize, BoilTime, Color, Gsum, type))

# Use only the numeric variables for the SVM
#dataset <- subset(combo.pca, select = c(IBU, ABV, Size.L., OG, BoilSize, BoilTime, Color, Gsum, type))


## Now we define a utility function for performing k-fold CV
## the learning data is split into k equal sized parts
## every time, one part goes for validation and k-1 go for building the model (training)
## the final error is the mean prediction error in the validation parts
## Note k=N corresponds to LOOCV

## using k-folds cross validation 
k <- 10 
N <- nrow(dataset)
folds <- sample(rep(1:k, length=N), N, replace=FALSE) 
valid.error <- rep(0,k)



#dataset$type <- as.character(dataset$type)
#dataset$type[dataset$type != "IPA"] <- "notIpa"
#dataset$type <- as.factor(dataset$type)


C <- 1

## Function from KMLMM subject
train.svm.kCV <- function (which.kernel, mycost)
{
  for (i in 1:k) 
  {  
    train <- dataset[folds!=i,] # for building the model (training)
    valid <- dataset[folds==i,] # for prediction (validation)
    
    x_train <- train[,1:8]
    t_train <- train[,9]
    
    switch(which.kernel,
           linear={model <- svm(x_train, factor(t_train), type="C-classification", cost=mycost, kernel="linear", scale = FALSE)},
           poly.2={model <- svm(x_train, factor(t_train), type="C-classification", cost=mycost, kernel="polynomial", degree=2, coef0=1, scale = FALSE)},
           poly.3={model <- svm(x_train, factor(t_train), type="C-classification", cost=mycost, kernel="polynomial", degree=3, coef0=1, scale = FALSE)},
           RBF={model <- svm(x_train, factor(t_train), type="C-classification", cost=mycost, kernel="radial", scale = FALSE)},
           stop("Enter one of 'linear', 'poly.2', 'poly.3', 'radial'"))
    
    x_valid <- valid[,1:8]
    pred <- predict(model,x_valid)
    t_true <- valid[,9]
    
    # compute validation error for part 'i'
    valid.error[i] <- sum(pred != t_true)/length(t_true)
  }
  # return average validation error
  sum(valid.error)/length(valid.error)
}

# Fit an SVM with different kernels
#(VA.error.linear <- train.svm.kCV ("linear", C))
#(VA.error.poly.2 <- train.svm.kCV ("poly.2", C))
#(VA.error.poly.3 <- train.svm.kCV ("poly.3", C))
(VA.error.RBF <- train.svm.kCV ("RBF", C))

# Trying with varying the cost
#C <- 50
#(VA.error.linear <- train.svm.kCV ("linear", C))
#(VA.error.RBF <- train.svm.kCV ("RBF", C))

#C <- 5
#(VA.error.linear <- train.svm.kCV ("linear", C))
#(VA.error.RBF <- train.svm.kCV ("RBF", C))

#C <- 0.05
#(VA.error.linear <- train.svm.kCV ("linear", C))
#(VA.error.RBF <- train.svm.kCV ("RBF", C))


# The support vector machines algorithm is a very good algorithm to classify data. 
# In SVM the main goal is to find a hyperplane that separates the data with a margin as big as possible 
# from the closest "support vector",  the closest datapoint of a class to the hyperplane separating the 
# classes, in one class to the one in the other class. The SVM does this by applying a kernel function 
# to the data. The kernel function is a way of mapping the datapoints to a higher dimension by applying 
# a function that uses the features in the original space as input and outputs the dot product of the data 
# in the feature space. This makes it a lot less computationally expensive than the alternative of elevating 
# the whole dataset to a higher dimension to compute the dot product from the transformed data. By mapping 
# the data to a higher dimensional feature space we are able to make a linear separation of data that were 
# not linearly separable in a lower dimension. 
# How strictly we want to be with allowing possible outliers inside the margin of the separating hyperplane 
# is determined by the variable epsilon, where a larger value will allow for more datapoints inside the 
# margin. A lower epsilon will give us a smaller margin and a more complex function, while a larger value 
# will lead to a less complex function but it might be less accurate as well. This trade-off is to be 
# expected in most machine learning algortithms as a more accurate function will usually be more 
# computationally expensive.


# We see that we get a fairly large error on predicting the type of beer from this dataset using SVM. The 
# reason for this is most likely a combination of several factors that makes SVMs unsuited for this data. 
# First of all SVMs are not good when there is a large imbalance in the number of members to each class, 
# with Ale and IPA being highly over-represented in the dataset compared to the other classes this makes 
# SVMs a bad fit. Also SVMs are not well suited for datasets with multiple classes which ours also has. 

# We can see that by classifying the beer as IPA or not IPA, thereby reducing the number of classes to two 
# and also evening out the members of each class, we get a much better result. This supports the theory 
# above that SVMs are a bad fit.

