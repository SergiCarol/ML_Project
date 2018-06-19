# Load and install necessary packages
requiredPackages <- c("readr",
                      "DMwR",
                      "rgl", "randomForest", "ROCR",
                      "rpart",
                      "dplyr", "sampling",
                      "class",
                      "nnet", "caret")

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

par(mfrow=c(2,2))

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

# Parameter tunning
xgbGrid <- expand.grid(nrounds = 50,
                       max_depth = 10,
                       eta = .05,
                       gamma = 0.3,
                       colsample_bytree = .9,
                       min_child_weight = 1,
                       subsample = 1)
# Model
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