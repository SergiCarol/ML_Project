# Load and install necessary packages
requiredPackages <- c("readr",
                      "DMwR",
                      "rgl", "randomForest", "ROCR",
                      "rpart",
                      "dplyr", "sampling")

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
testRange = seq(0.6*totalRows, totalRows)

size_test <- 0.6*totalRows/7

stratified <- strata(beers.df,"type" ,size = rep(size_test,7), method = "srswr") # With repetitions or not enough data for cider

trainingData <- getdata(beers.df, stratified)

trainingData$ID_unit = NULL
trainingData$Stratum = NULL
trainingData$Prob =NULL

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

# Best result is with 400 trees



rf <- randomForest(formula = type ~ .,
                   data=trainingData,
                   ntree=1000,
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

#for (i in 1:4){
#  plot(rf$importance[,i], col= rgb(cr(rf$importance[,i]/max(rf$importance[,i])), max=255), ylab="Importance", main=paste("Cluster ", i,sep=""))
#  text(rf$importance[,i], labels = rownames(rf$importance), pos=2, col= rgb(cr(rf$importance[,i]/max(rf$importance[,i])), max=255))
#}

barplot(rf$importance, col= rgb(cr(rf$importance/max(rf$importance)), max=255), ylab="Importance", main=paste("Cluster ", i,sep=""))

varImpPlot(rf)
varUsed(rf, by.tree=FALSE, count = TRUE)

