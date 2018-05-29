# Load and install necessary packages
requiredPackages <- c("readr",
                      "DMwR",
                      "rgl", "randomForest", "ROCR",
                      "rpart",
                      "dplyr", "sampling",
                      "class")

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

beers.df$Cluster <- as.factor(beers.df$Cluster)
droplevels(beers.df$Cluster)


beers.df$Cluster <- as.factor(beers.df$type)
droplevels(beers.df$Cluster)

totalRows =  nrow(beers.df)
testRange = seq(0.6*totalRows, totalRows)

size_training <- 0.8*totalRows/7

#stratified <- strata(beers.df,"type" ,size = rep(size_training,7), method = "srswor") # With repetitions or not enough data for cider
rows <- sample(totalRows, round(totalRows*0.8))
trainingData <- beers.df[rows,]
testData <- beers.df[-rows,]
#trainingData <- getdata(beers.df, stratified)

# trainingData$ID_unit = NULL
# trainingData$Stratum = NULL
# trainingData$Prob =NULL
# 
testData <- beers.df[testRange,]


#### Cross Validation ####



#### Random Forests ####
# (ntrees <- round(10^seq(1,3,by=0.2)))
# 
# # prepare the structure to store the partial results
# 
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

# bestmtry <- tuneRF(trainingData[, -11], trainingData[,11], stepFactor=1.5, ntree=500)

# Best result is with 600 trees, with mtry 2



rf <- randomForest(formula = type ~ .,
                   data=trainingData,
                   ntree=600,
                   mtry=2,
                   importance=TRUE,
                   xtest=subset(testData, select= -type),
                   ytest=testData$type,
                   keep.forest = TRUE,
                   proximity=TRUE)
plot(rf)
legend("topright", colnames(rf$err.rate), col=1:5, cex=0.8, fill=1:5)

print(rf)

(results <- table(testData$type, rf_predict))
(accuracy <- sum(diag(results))/nrow(testData))

tree <- getTree(rf, k=200)
tree
x <- ctree(Cluster ~ ., data=trainingData)
plot(x, type="simple")

par(mfrow=c(2,2))

cr <- colorRamp(c("blue", "red"))

for (i in 1:7){
  plot(rf$importance[,i], col= rgb(cr(abs(rf$importance[,i]/max(rf$importance[,i]))), maxColorValue=255), ylab="Importance", main=colnames(rf$importance)[i])
  text(rf$importance[,i], labels = rownames(rf$importance), pos=2, col= rgb(cr(rf$importance[,i]/max(rf$importance[,i])), max=255))
}

barplot(rf$importance, ylab="Importance", main=paste("Cluster ", i,sep=""))

varImpPlot(rf)
varUsed(rf, by.tree=FALSE, count = TRUE)






#### Multinomial classiffier ####

# Needs tunning

multi <- multinom(type ~ ., data=trainingData)
results <- predict(multi, testData)

(results <- table(testData$type, results))
(accuracy <- sum(diag(results))/nrow(testData))


#### Gradient Boost ####
# https://machinelearningmastery.com/gentle-introduction-xgboost-applied-machine-learning/
xgbGrid <- expand.grid(nrounds = 50,
                       max_depth = 10,
                       eta = .05,
                       gamma = 0.3,
                       colsample_bytree = .9,
                       min_child_weight = 1,
                       subsample = 1)

StyleXGB = train(type ~ ., data = trainingData,
                 method = "xgbTree",
                 tuneGrid = xgbGrid,na.action = na.pass,
                 metric = "Accuracy",
                 num_class=7)

importance <- varImp(StyleXGB) 
plot(importance)

results <- predict(StyleXGB, testData)

(results <- table(testData$type, results))
(accuracy <- sum(diag(results))/nrow(testData))
