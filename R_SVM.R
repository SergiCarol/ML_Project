setwd("~/projects/school/ML/ML_Project")
library(FactoMineR)
library(factoextra)
library(calibrate)
library(ggplot2)
library(ggrepel)
library(DMwR)
library(kernlab)

set.seed(123)

# Read
recipe <- read.csv('recipeData.csv',na.strings = "N/A", fileEncoding = "ISO-8859-1")
head(recipe)

str(recipe)
summary(recipe)
table(is.na(recipe))
#library(FactoMineR)
#catdes(recipe,1)



## Now we will prepare the data. To do that we'll start with pattern matching and replacement.
# We will aggregate the types of beers according to their style

recipe$Style = as.character(recipe$Style)
levels(as.factor(recipe$Style))

recipe = subset(recipe, select = -c(MashThickness, UserId, PrimingAmount, PrimingMethod, PrimaryTemp, PitchRate))

ale =  recipe[grep("Ale", recipe$Style, ignore.case=TRUE),]

american_ale_style =  recipe[grep("American Pale Ale", recipe$Style, ignore.case=TRUE),]
american_ale_style$type = "American Ale"

ale_style <- ale[!(ale$BeerID %in% american_ale_style$BeerID),]
ale_style$type  = "Ale"

ipa_style =  recipe[grep("IPA", recipe$Style, ignore.case=TRUE),]
ipa_style$type  = "IPA"

stout_style =  recipe[grep("Stout", recipe$Style, ignore.case=TRUE),]
stout_style$type  = "Stout"

lager_style =  recipe[grep("Lager", recipe$Style, ignore.case=TRUE),]
lager_style$type  = "Lager"

bitter_style =  recipe[grep("Bitter", recipe$Style, ignore.case=TRUE),]
bitter_style$type  = "Bitter"

cider_style =  recipe[grep("Cider", recipe$Style, ignore.case=TRUE),]
cider_style$type  = "Cider"

porter_style =  recipe[grep("Porter", recipe$Style, ignore.case=TRUE),]
porter_style$type  = "Porter"

combo = rbind(ale_style, ipa_style, stout_style, lager_style, bitter_style, cider_style, porter_style,
              american_ale_style)

combo = subset(combo, select= -c(Style, URL, StyleID, BeerID, Efficiency))

head(combo)

# Set factors

combo$Name = as.character(combo$Name)
combo[is.na(combo$Name),1] = "Unknown Beer"
combo$Name = as.factor(combo$Name)
combo$type = as.factor(combo$type)
combo$SugarScale = as.factor(combo$SugarScale)
combo$type =  droplevels(combo$type)
combo$Name =  droplevels(combo$Name)
combo$SugarScale =  droplevels(combo$SugarScale)

# Impute Boil Gravity
# a = knnImputation(data = combo, k=5)

# Check for Outliers, remove all bigger than 658 since thats the most bitter beer in the world (else all outliers)
plot(combo$IBU)

combo = combo[combo$IBU < 658,]
combo = combo[combo$Color <= 100,] # MAX BLACK is 50

combo$Size.L. = log(combo$Size.L.)
combo$BoilSize = log(combo$BoilSize)

combo$Gsum=combo$OG+combo$FG

combo$FG = NULL



#### PCA  ####
combo.pca = subset(combo, select = -c(BoilGravity, Name))

#beer.df = subset(combo.pca, select = -c(Size.L., SugarScale, BrewMethod, type))
#distances <- mahalanobis(scale(beer.df), center=FALSE, cov=cov(beer.df))
#cutoff_value <- sqrt(qchisq(0.975, ncol(beer.df)))
#combo.pca <- combo.pca[distances < cutoff_value,]



####################################################
####################################################
####################################################
# Kernels
####################################################
####################################################
################################################
## Kernel-Based Learning & Multivariate Modeling
## MIRI Master - September 2017
################################################

## the SVM is located in two different packages: one of them is 'e1071'
library(e1071)

## First we create a simple two-class data set:

dataset <- subset(combo.pca, select = c(Color, IBU, type))
plot(dataset$Color,dataset$IBU,col=as.factor(dataset$type))

## Now we define a utility function for performing k-fold CV
## the learning data is split into k equal sized parts
## every time, one part goes for validation and k-1 go for building the model (training)
## the final error is the mean prediction error in the validation parts
## Note k=N corresponds to LOOCV

## a typical choice is k=10
k <- 10 
folds <- sample(rep(1:k, length=N), N, replace=FALSE) 
N <- 39
valid.error <- rep(0,k)

C <- 1
i = 1
## This function is not intended to be useful for general training purposes but it is useful for illustration
## In particular, it does not optimize the value of C (it requires it as parameter)

train.svm.kCV <- function (which.kernel, mycost)
{
  for (i in 1:k) 
  {  
    train <- dataset[folds!=i,] # for building the model (training)
    valid <- dataset[folds==i,] # for prediction (validation)
    
    x_train <- train[,1:2]
    t_train <- train[,3]
    
    switch(which.kernel,
           linear={model <- svm(x_train, t_train, type="C-classification", cost=mycost, kernel="linear", scale = FALSE)},
           poly.2={model <- svm(x_train, t_train, type="C-classification", cost=mycost, kernel="polynomial", degree=2, coef0=1, scale = FALSE)},
           poly.3={model <- svm(x_train, t_train, type="C-classification", cost=mycost, kernel="polynomial", degree=3, coef0=1, scale = FALSE)},
           RBF={model <- svm(x_train, t_train, type="C-classification", cost=mycost, kernel="radial", scale = FALSE)},
           stop("Enter one of 'linear', 'poly.2', 'poly.3', 'radial'"))
    
    x_valid <- valid[,1:2]
    pred <- predict(model,x_valid)
    t_true <- valid[,3]
    
    # compute validation error for part 'i'
    valid.error[i] <- sum(pred != t_true)/length(t_true)
  }
  # return average validation error
  sum(valid.error)/length(valid.error)
}

# Fit an SVM with linear kernel

(VA.error.linear <- train.svm.kCV ("linear", C))

## We should choose the model with the lowest CV error and refit it to the whole learning data
## then use it to predict the test set; we will do this at the end

## As for now we wish to visualize the models

# so first we refit the model:

model <- svm(dataset[,1:2],dataset[,3], type="C-classification", cost=C, kernel="linear", scale = FALSE)

## Now we are going to visualize what we have done; since we have artificial data, instead of creating
## a random test set, we can create a grid of points as test

plot.prediction <- function (model.name, resol=200)
  # the grid has a (resol x resol) resolution
{
  x <- cbind(dataset$Color,dataset$IBU)
  rng <- apply(x,2,range);
  tx <- seq(rng[1,1],rng[2,1],length=resol);
  ty <- seq(rng[1,2],rng[2,2],length=resol);
  pnts <- matrix(nrow=length(tx)*length(ty),ncol=2);
  k <- 1
  for(j in 1:length(ty))
  {
    for(i in 1:length(tx))
    {
      pnts[k,] <- c(tx[i],ty[j])
      k <- k+1
    } 
  }
  
  # we calculate the predictions on the grid
  
  pred <- predict(model, pnts, decision.values = TRUE)
  
  z <- matrix(attr(pred,"decision.values"),nrow=length(tx),ncol=length(ty))
  
  # and plot them
  
  image(tx,ty,z,xlab=model.name,ylab="",axes=FALSE,
        xlim=c(rng[1,1],rng[2,1]),ylim=c(rng[1,2],rng[2,2]),
        col = cm.colors(64))
  #        col = rainbow(200, start=0.9, end=0.1))
  
  # then we draw the optimal separation and its margins
  
  contour(tx,ty,z,add=TRUE, drawlabels=TRUE, level=0, lwd=3)
  contour(tx,ty,z,add=TRUE, drawlabels=TRUE, level=1, lty=1, lwd=1, col="grey")
  contour(tx,ty,z,add=TRUE, drawlabels=TRUE, level=-1, lty=1, lwd=1, col="grey")
  
  # then we plot the input data from the two classes
  
  points(dataset[dataset$target==1,1:2],pch=21,col=1,cex=1)
  points(dataset[dataset$target==-1,1:2],pch=19,col=4,cex=1)
  
  # finally we add the SVs
  
  sv <- dataset[c(model$index),];
  sv1 <- sv[sv$target==1,];
  sv2 <- sv[sv$target==-1,];
  points(sv1[,1:2],pch=13,col=1,cex=2)
  points(sv2[,1:2],pch=13,col=4,cex=2)
}

## plot the predictions, the separation, the support vectors, everything
plot.prediction ("linear")

## right, now a quadratic SVM model 

(VA.error.poly.2 <- train.svm.kCV ("poly.2", C))

model <- svm(dataset[,1:2],dataset[,3], type="C-classification", cost=C, kernel="polynomial", degree=2, coef0=1, scale = FALSE)
plot.prediction ("poly.2")

## right, now a cubic SVM model 

(VA.error.poly.3 <- train.svm.kCV ("poly.3", C))

model <- svm(dataset[,1:2],dataset[,3], type="C-classification", cost=C, kernel="polynomial", degree=3, coef0=1, scale = FALSE)
plot.prediction ("poly.3")

## and finally an RBF Gaussian SVM model 

(VA.error.RBF <- train.svm.kCV ("RBF", C))

model <- svm(dataset[,1:2],dataset[,3], type="C-classification", cost=C, kernel="radial", scale = FALSE)
plot.prediction ("RBF")

## Now in a real scenario we should choose the model with the lowest CV error
## which in this case is the RBF

## In a real setting we should optimize the value of C, again with CV; this can be done
## very conveniently using tune() in this package to do automatic grid-search

## another, more general, possibility is to use the train() method in the {caret} package

## Just for illustration, let's see the effect of altering C (significantly):
C <- 50

(VA.error.linear <- train.svm.kCV ("linear", C))
model <- svm(dataset[,1:2],dataset[,3], type="C-classification", cost=C, kernel="linear", scale = FALSE)
plot.prediction ("linear")

(VA.error.RBF <- train.svm.kCV ("RBF", C))
model <- svm(dataset[,1:2],dataset[,3], type="C-classification", cost=C, kernel="radial", scale = FALSE)
plot.prediction ("RBF")

C <- 0.05

(VA.error.linear <- train.svm.kCV ("linear", C))
model <- svm(dataset[,1:2],dataset[,3], type="C-classification", cost=C, kernel="linear", scale = FALSE)
plot.prediction ("linear")

(VA.error.RBF <- train.svm.kCV ("RBF", C))
model <- svm(dataset[,1:2],dataset[,3], type="C-classification", cost=C, kernel="radial", scale = FALSE)
plot.prediction ("RBF")


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
