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
library(kernlab)


k.1 <- stringdot("spectrum", length = 5, normalized = T )
k.2 <- stringdot("exponential", length = 6, normalized = T )
k.3 <- stringdot("spectrum", length = 5, normalized = T )
#k.3 <- stringdot("spectrum", length = 8, normalized = T )
K.1 <- kernelMatrix(k.1, combo.pca) #Mira la similitud entre tots els textos
K.2 <- kernelMatrix(k.2, combo.pca) #Mira la similitud entre tots els textos
#K.3 <- kernelMatrix(k.3, little.dataset$Text) #Mira la similitud entre tots els textos
#k.1 <- stringdot(type="exponential", lambda=1.5, normalized=T)
K <- 0.3*K.1+0.7*K.2
# applying ksvm
# The fit a SVM in the train part
(N <- dim(combo.pca)[1])  # number of rows
ntrain <- round(N*2/3)     # number of training examples
tindex <- sample(N,ntrain) # indices of training examples
svm1.train <- ksvm (K[tindex,tindex],combo.pca$type[tindex], type="C-svc", kernel='matrix')




train.svm.kCV <- function (which.kernel, mycost)
{
  for (i in 1:k) 
  {  
    train <- combo.pca[folds!=i,] # for building the model (training)
    valid <- combo.pca[folds==i,] # for prediction (validation)
    
    x_train <- train[,1:7]
    t_train <- train[,10]
    
    switch(which.kernel,
           linear={model <- svm(x_train, t_train, type="C-classification", cost=mycost, kernel="linear", scale = FALSE)},
           poly.2={model <- svm(x_train, t_train, type="C-classification", cost=mycost, kernel="polynomial", degree=2, coef0=1, scale = FALSE)},
           poly.3={model <- svm(x_train, t_train, type="C-classification", cost=mycost, kernel="polynomial", degree=3, coef0=1, scale = FALSE)},
           RBF={model <- svm(x_train, t_train, type="C-classification", cost=mycost, kernel="radial", scale = FALSE)},
           stop("Enter one of 'linear', 'poly.2', 'poly.3', 'radial'"))
    
    x_valid <- valid[,1:7]
    pred <- predict(model,x_valid)
    t_true <- valid[,10]
    
    # compute validation error for part 'i'
    valid.error[i] <- sum(pred != t_true)/length(t_true)
  }
  # return average validation error
  sum(valid.error)/length(valid.error)
}
k = 10
folds <- sample(rep(1:k, length=N), N, replace=FALSE) 

valid.error <- rep(0,k)

C <- 1
(VA.error.linear <- train.svm.kCV ("linear", C))
(VA.error.poly.2 <- train.svm.kCV ("poly.2", C))
(VA.error.RBF <- train.svm.kCV ("RBF", C))

dataset <-combo.pca
model <- svm(dataset[,1:7],dataset[,10], type="C-classification", cost=C, kernel="linear", scale = FALSE)


## Just for illustration, let's see the effect of altering C (significantly):
C <- 50
(VA.error.linear <- train.svm.kCV ("linear", C))
(VA.error.poly.2 <- train.svm.kCV ("poly.2", C))
(VA.error.RBF <- train.svm.kCV ("RBF", C))

C <- 0.05
(VA.error.linear <- train.svm.kCV ("linear", C))
(VA.error.poly.2 <- train.svm.kCV ("poly.2", C))
(VA.error.RBF <- train.svm.kCV ("RBF", C))


pca <- prcomp(dataset[,1:7], scale = TRUE)
pca

## This a PCA biplot, a standard visualization tool in multivariate data analysis
## (not covered in this course)
## It allows information on both examples and variables of a data matrix to be displayed simultaneously

## This is a nice visualization tool; if you are interested in meaning, have a look at:

# http://forrest.psych.unc.edu/research/vista-frames/help/lecturenotes/lecture13/biplot.html

biplot(pca)

## these are the returned singular values of the data matrix
## (the square roots of the eigenvalues of the covariance/correlation matrix)
pca$sdev

eigenval <- pca$sdev^2
xpercent <- eigenval[1]/sum(eigenval)*100   # proportion of variance explained by the first PC
ypercent <- eigenval[2]/sum(eigenval)*100   # proportion of variance explained by the second PC

plot (pca$x[,1], pca$x[,2], col=as.integer(iris[,5]),
      main=paste(paste("PCA -", format(xpercent+ypercent, digits=3)), "% explained variance"),
      xlab=paste("1st PC (", format(xpercent, digits=2), "%)"),
      ylab=paste("2nd PC (", format(ypercent, digits=2), "%)"))

## We see that PCA does a wonderful job here in representing/separating the three flower species
## in a lower dimensional space (from 4 to 2 dimensions)
## This is not always the case, given that PCA is an unsupervised and linear technique

## The unsupervised character cannot be changed, but we can capture non-linear PCs with kernel PCA

## first we create a plotting function 

plotting <-function (kernelfu, kerneln, iftext=FALSE)
{
  xpercent <- eig(kernelfu)[1]/sum(eig(kernelfu))*100
  ypercent <- eig(kernelfu)[2]/sum(eig(kernelfu))*100
  
  plot(rotated(kernelfu), col=as.integer(iris[,5]), 
       main=paste(paste("Kernel PCA (", kerneln, ")", format(xpercent+ypercent,digits=3)), "%"),
       xlab=paste("1st PC -", format(xpercent,digits=3), "%"),
       ylab=paste("2nd PC -", format(ypercent,digits=3), "%"))
  
  if (iftext) text(rotated(kernelfu)[,1], rotated(kernelfu)[,2], rownames(iris), pos= 3)
}

## 1. -------------------------------Linear Kernel---------------------
kpv <- kpca(~., data=dataset[,1:7], kernel="vanilladot", kpar=list(), features=2)
plotting (kpv, "linear")

## 2. ------------------------------Polynomial Kernel (degree 3)-----------------

kpp <- kpca(~., data=dataset[,1:7], kernel="polydot", kpar=list(degree=3,offset=1), features=2)
plotting(kpp,"cubic")

## 3. -------------------------------RBF Kernel-----------------------

kpc1 <- kpca(~., data=dataset[,1:7], kernel="rbfdot", kpar=list(sigma=0.6), features=2)
plotting(kpc1,"RBF - sigma 0.6")

kpc2 <- kpca(~., data=dataset[,1:7], kernel="rbfdot", kpar=list(sigma=1), features=2)
plotting(kpc2,"RBF - sigma 1.0")

## Note we could use our pre-defined 'rbf' kernel as well
kpc3 <- kpca(~., data=dataset[,1:7], kernel=rbf, features=2)
plotting(kpc3,"RBF - sigma 1.0")

## The effect of sigma is a large one ...
kpc4 <- kpca(~., data=dataset[,1:7], kernel="rbfdot", kpar=list(sigma=10), features=2)
plotting(kpc4,"RBF - sigma 10")

## The effect of sigma is a large one ...
kpc5 <- kpca(~., data=dataset[,1:7], kernel="rbfdot", kpar=list(sigma=0.01), features=2)
plotting(kpc5,"RBF - sigma 0.01")


pca.ratings <- prcomp(dataset[,1:7], scale = TRUE)
pca.ratings

summary(pca.ratings)
biplot(pca.ratings, cex=0.6)
## Do the following (in separate sections)

# 1. Do a PCA plot using the first 2 PCs, as you did in Part 1 for the Iris data
#    Add names to the plot; you may use:

text(pca$x[,1], pca$x[,2], rownames(dataset), pos= 3, cex=0.6)

# 2. Do a kernel PCA plot using the first 2 PCs, as you did in Part 1 for the Iris data
#    You will have to modify the plotting function, for example to eliminate colors (there are no classes here)
#    Add state names to the plot; you may use:

text(rotated(kernelfu)[,1], rotated(kernelfu)[,2], rownames(dataset), pos= 3, cex=0.6)
