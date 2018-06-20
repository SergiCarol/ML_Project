### README ###
# This files contains both the pre processing of the data and the models.
# It is important to note that we are using two different files
# The recipeData.csv is the original data
# The beers.csv is the file with all the pre processed data, as such
# it is not needed to execute the pre processing all the time but it
# is possible to start running the program from the MODELS section


# Load and install necessary packages
requiredPackages <- c("FactoMineR",
                      "factoextra",
                      "calibrate",
                      "ggplot2",
                      "ggrepel",
                      "DMwR",
                      "rgl", "randomForest", "ROCR",
                      "rpart")

for (pac in requiredPackages) {
  if(!require(pac,  character.only=TRUE)){
    install.packages(pac, repos="http://cran.rstudio.com")
    library(pac,  character.only=TRUE)
  } 
}
rm(pac)
rm(requiredPackages)

set.seed(123)

# Read
recipe <- read.csv('recipeData.csv',na.strings = "N/A", fileEncoding = "ISO-8859-1")
  head(recipe)

str(recipe)
summary(recipe)
table(is.na(recipe))



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
par(mfrow=c(1,2))
plot(combo$IBU, main="IBU")
plot(combo$Color, main="Color")

combo = combo[combo$IBU < 658,]
combo = combo[combo$Color <= 100,] # MAX BLACK is 50

combo$Size.L. = log(combo$Size.L.)
combo$BoilSize = log(combo$BoilSize)

combo$Gsum=combo$OG+combo$FG

combo$FG = NULL

plot(combo$type, combo$Color, col=c(2:10))


#### PCA  ####
combo.pca = subset(combo, select = -c(BoilGravity, Name))

combo.pca$type[combo.pca$type ==  "Porter"] = as.factor("Stout")
combo.pca$type = as.factor(combo.pca$type)
combo.pca$type = droplevels(combo.pca$type)
table(combo.pca$type)


pca = PCA(combo.pca, quanti.sup = c(1, 6), quali.sup = c(8, 9, 10))
fviz_pca_ind(pca, col.ind = "contrib")
fviz_eig(pca)
nd = 3


#### Second Cluster with grouped classes ####

pca$var$cos2

Psi = pca$ind$coord[, 1:nd]

# Cluster for large data
n1 = 50
k1 <- kmeans(Psi, n1, iter.max=19) # Converges at 19
k2 <- kmeans(Psi, n1, iter.max=19)
table(k2$cluster,k1$cluster)
clas <- (k2$cluster-1)*n1+k1$cluster
freq <- table(clas) 
cdclas <- aggregate(as.data.frame(Psi),list(clas),mean)[,2:(nd+1)]

d2 <- dist(cdclas)
h2 <- hclust(d2,method="ward.D2",members=freq) # COMPARE THE COST
plot(h2)
barplot(h2$height[(nrow(cdclas)-40):(nrow(cdclas)-1)]) # PLOT OF THE LAST 39 AGGREGATIONS 
nc = 4
c2 <- cutree(h2,nc)
cdg <- aggregate((diag(freq/sum(freq)) %*% as.matrix(cdclas)),list(c2),sum)[,2:(nd+1)] # WHY WEIGHT 
k6 <- kmeans(Psi,centers=cdg)

Bss <- sum(rowSums(k6$centers^2)*k6$size)
Wss <- sum(k6$withinss)
Ib6 <- 100*Bss/(Bss+Wss)
Ib6

rgl::plot3d(Psi, col=k6$cluster)
points3d(pca$quali.sup$coord[levels(combo.pca$type), 1:3], col = "orange", size=10)
text3d(pca$quali.sup$coord[levels(combo.pca$type), 1:3], texts = levels(combo.pca$type), col="yellow")

(c_des <- catdes(data.frame(as.factor(k6$cluster), combo.pca), num.var = 1))
plot(c_des)


#### Explanation  ####
# We found that separating with 4 clusters appears to be the best option.
# By plotting by color, IBU, and type we found that there were two main different clusters one of them that could be splitted in three.
# This splitting is mainly by ABV, and then by IBU and Color. We can appreciate how the first cluster (in catdes) is conformed by those type of beers which 
# are lighter and not much bitter. The second cluster is confirmed by IPA beers which is most bitter of all the beers.
# The third cluster has the Stout beers, which are Darker in color but not as bitter as IPA's, it is also worth noting that the Brew Method
# Partial Mash is highly used in this cluster.
# The last cluster (4) is mainly conformed by Lager beers, which are lighter in color and massively produced (size, boil, OG...).
# It is also worth noting that its the only beer which extensevily uses Plato as SugarStyle.

# Add clusters as colum

beers.df <- data.frame("Cluster" = as.factor(k6$cluster), combo.pca)

pie(table(beers.df$type))



#### MODELS ####
# Load and install necessary packages
requiredPackages <- c("readr",
                      "DMwR",
                      "rgl", "randomForest", "ROCR",
                      "rpart",
                      "dplyr", "sampling",
                      "class",
                      "nnet", "caret", "e1071")

for (pac in requiredPackages) {
  if(!require(pac,  character.only=TRUE)){
    install.packages(pac, repos="http://cran.rstudio.com")
    library(pac,  character.only=TRUE)
  } 
}
rm(pac)
rm(requiredPackages)

set.seed(123)


beers.df <- read.csv("beers.csv")


totalRows =  nrow(beers.df)

rows <- sample(totalRows, round(totalRows*0.8))
trainingData <- beers.df[rows,]
testData <- beers.df[-rows,]

#### Cross Validation ####



#### Random Forests ####
# Parameter selection
#(ntrees <- round(10^seq(1,3,by=0.2)))
# prepare the structure to store the partial results

# rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
# colnames (rf.results) <- c("ntrees", "OOB")
# rf.results[,"ntrees"] <- ntrees
# rf.results[,"OOB"] <- 0
# 
# ii <- 1
# 
# for (nt in ntrees) {
#   print(nt)
# 
#   rf <- randomForest(formula = type ~ .,
#                      data=trainingData,
#                      ntree=nt,
#                      importance=TRUE,
#                      xtest=subset(testData, select= -type),
#                      ytest=testData$type,
#                      keep.forest=TRUE)
#   # get the OOB
#   rf.results[ii,"OOB"] <- rf$err.rate[nt,1]
# 
#   ii <- ii+1
# }
# rf.results
# lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
# (ntrees.best <- rf.results[lowest.OOB.error,"ntrees"])

# Find the best mtry parameter
# bestmtry <- tuneRF(trainingData[, -11], trainingData[,11], stepFactor=1.5, ntree=600)

# Best result is with 600 trees, with mtry 2



rf <- randomForest(formula = type ~ .,
                   data=trainingData,
                   ntree=600,
                   mtry=3,
                   importance=TRUE,
                   xtest=subset(testData, select= -type),
                   ytest=testData$type,
                   keep.forest = TRUE)
plot(rf)
legend("topright", colnames(rf$err.rate), col=1:5, cex=0.8, fill=1:5)

print(rf)

(results <- table(testData$type, rf_predict))
(accuracy <- sum(diag(results))/nrow(testData))

tree <- getTree(rf, k=200)
tree
x <- ctree(Cluster ~ ., data=trainingData)
plot(x, type="simple")
windows()
par(mfrow=c(2,4))

cr <- colorRamp(c("blue", "red"))

for (i in 1:7){
  plot(rf$importance[,i], col= rgb(cr(abs(rf$importance[,i]/max(rf$importance[,i]))), maxColorValue=255), ylab="Importance", main=colnames(rf$importance)[i])
  text(rf$importance[,i], labels = rownames(rf$importance), pos=2, col= rgb(cr(abs(rf$importance[,i]/max(rf$importance[,i]))), maxColorValue=255))
}

barplot(rf$importance, ylab="Importance", main=paste("Cluster ", i,sep=""))

varImpPlot(rf)
varUsed(rf, by.tree=FALSE, count = TRUE)




#### Multinomial classiffier ####

multi <- multinom(type ~ ., data=trainingData)
# # weights:  119 (96 variable)
# initial value 81062.724989 
# iter 100 value 44511.404646
# final value 44511.404646 
summary(multi)
(z <- summary(multi)$coefficients/summary(multi)$standard.errors)
(p <- (1 - pnorm(abs(z), 0, 1))*2) #pvalues
exp(coef(multi))

results <- predict(multi, testData)

(results <- table(testData$type, results))
(accuracy <- sum(diag(results))/nrow(testData))



#### Gradient Boost ####

# Model Selection
# fitControl <- trainControl(method = "cv", number = 5) 
# xgbGrid <- expand.grid(nrounds = 50,
#                        max_depth = seq(from=7, to=12, by =1),
#                        eta = seq(from 0.00, to=0.05, by=0.01),
#                        gamma = seq(0.1, 0.5, by=0.1),
#                        colsample_bytree = .9,
#                        min_child_weight = 1,
#                        subsample = 1)
# # Model
# StyleXGB = train(type ~ ., data = trainingData,
#                  method = "xgbTree",
#                  tuneGrid = xgbGrid,na.action = na.paxss,
#                  trControl = fitControl,
#                  metric = "Accuracy",
#                  num_class=7)

# Model traning
xgbGrid <- expand.grid(nrounds = 50,
                       max_depth = 10,
                       eta = 0.05,
                       gamma = 0.3,
                       colsample_bytree = .9,
                       min_child_weight = 1,
                       subsample = 1)
# Model
StyleXGB = train(type ~ ., data = trainingData,
                 method = "xgbTree",
                 tuneGrid = xgbGrid,
                 metric = "Accuracy",
                 num_class=7)

importance <- varImp(StyleXGB) 
plot(importance)

results <- predict(StyleXGB, testData)

(results <- table(testData$type, results))
(accuracy <- sum(diag(results))/nrow(testData))

class.error = rep(0, nrow(results))
for (i in 1:nrow(results)){
  class.error[i] <-1 - (results[i,i]/sum(results[i,]))
  }
cbind(results, class.error)

#### Neural Networks ####

# Model selection, be adivsed this takes a LONG time to finish.
# fitControl <- trainControl(method = "repeatedcv", 
#                            number = 5, 
#                            repeats = 5)
# 
# nnetGrid <-  expand.grid(size = seq(from = 10, to = 15, by = 1),
#                          decay = seq(from = 0.0, to = 0.4, by = 0.1))
# 
# nnetFit <- train(type ~ ., 
#                  data = trainingData,
#                  method = "nnet",
#                  metric = "Accuracy",
#                  #trControl = fitControl,
#                  tuneGrid = nnetGrid,
#                  verbose = FALSE,
#                  maxit=500)

# NN model
nnet.results <- nnet(type ~ ., data=trainingData, size=10, maxit=500, decay=0.1)
results <- predict(nnet.results, testData, type="class")

(results <- table(testData$type, results))
(accuracy <- sum(diag(results))/nrow(testData))



#### SVM ####


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

(VA.error.RBF <- train.svm.kCV ("RBF", C))
