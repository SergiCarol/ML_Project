# Load and install necessary packages
requiredPackages <- c("FactoMineR",
                      "factoextra",
                      "calibrate",
                      "ggplot2",
                      "ggrepel",
                      "DMwR",
                      "rgl", "randomForest", "ROCR",
                      "rpart",
                      "Hotelling")

for (pac in requiredPackages) {
  if(!require(pac,  character.only=TRUE)){
    install.packages(pac, repos="http://cran.rstudio.com")
    library(pac,  character.only=TRUE)
  } 
}
rm(pac)
rm(requiredPackages)

getIndexByName <- function(df, name) {
    return (grep(name, colnames(df)))
}


getIndexesByName <- function(df, columnNames) {
    indexes = c()
    for(col in columnNames) {
        indexes = append(indexes, getIndexByName(df, col))
    } 
    return (indexes)
}

set.seed(123)

# Read
recipe <- read.csv('recipeData.csv',na.strings = "N/A", fileEncoding = "ISO-8859-1")
rownames(recipe) <- NULL


head(recipe)
str(recipe)
summary(recipe)
table(is.na(recipe))
nrow(recipe[!complete.cases(recipe),])
nrow(recipe)

#library(FactoMineR)
#catdes(recipe,1)



## Now we will prepare the data. To do that we'll start with pattern matching and replacement.
# We will aggregate the types of beers according to their style

recipe$Style = as.character(recipe$Style)
levels(as.factor(recipe$Style))

recipe = subset(recipe, select = -c(MashThickness, UserId, PrimingAmount, PrimingMethod, PrimaryTemp, PitchRate))
nrow(recipe[!complete.cases(recipe),])
nrow(recipe)

ale =  recipe[grep("Ale", recipe$Style, ignore.case=TRUE),]

american_ale_style =  recipe[grep("American Pale Ale", recipe$Style, ignore.case=TRUE),]
  american_ale_style$type = "American Ale"

ale_style <- ale[!(ale$BeerID %in% american_ale_style$BeerID),]
  ale_style$type  = "Ale"
  levels(as.factor(ale$Style)) # Know how many ALE are...
  levels(as.factor(ale_style$Style)) # Know how many ALE are...
  levels(as.factor(american_ale_style$Style)) # Know how many American ALE are...
  
ipa_style =  recipe[grep("IPA", recipe$Style, ignore.case=TRUE),]
  ipa_style$type  = "IPA"
  levels(as.factor(ipa_style$Style)) # Know how many IPA are...

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
rownames(combo) <- NULL

# Set factors

combo$Name = as.character(combo$Name)
combo[is.na(combo$Name),1] = "Unknown Beer"
combo$Name = as.factor(combo$Name)
combo$type = as.factor(combo$type)
combo$SugarScale = as.factor(combo$SugarScale)
combo$type =  droplevels(combo$type)
combo$Name =  droplevels(combo$Name)
combo$SugarScale =  droplevels(combo$SugarScale)

nrow(combo[!complete.cases(combo),])
nrow(combo)

# Impute Boil Gravity
SHOULD_IMPUTE = FALSE
fileName = "BoilGravityImputed.csv"
if(SHOULD_IMPUTE){
    a = knnImputation(data = combo, k=5)    
    write.csv2(x = a, file = fileName)
} else {
    a = read.csv2(file = fileName, header = TRUE, stringsAsFactors = TRUE)
    a$X <- NULL
    a$Name = as.character(a$Name)
}

combo = a 

combo$Gsum=combo$OG+combo$FG
combo$FG = NULL

summary(combo)
dim(combo)
levels(combo$type)
table(combo$type)

# Check for Outliers, remove all bigger than 658 since thats the most bitter beer in the world (else all outliers)
plot(combo$IBU, main = "IBU")
abline(h=658, col="red")

plot(combo$Color, main = "Color")
abline(h=100, col="red")


combo = combo[combo$IBU < 658,]
combo = combo[combo$Color <= 100,] # MAX BLACK is 50
dim(combo)


hist(combo$Size.L., main = "Size L")
hist(combo$BoilSize, main = "Boil Size")

combo$Size.L. = log(combo$Size.L.)
combo$BoilSize = log(combo$BoilSize)


hist(combo$Size.L., main = "Size L")
hist(combo$BoilSize, main = "Boil Size")


dev.off()
plot(combo$type, combo$Color, main = "Color per type", col=c(2:10), xlab = "Type", ylab = "Color")
plot(combo$type, combo$IBU, main = "IBU per type", col=c(2:10), xlab = "Type", ylab = "IBU")

# More plots
for (i in c(3,4,5,6,7,12)){
  plot(density(combo[,i]), main=colnames(combo)[i])
}


###### VALIDATION #####
rownames(combo) <- NULL
totalRows = nrow(combo)

rows <- sample(totalRows, round(totalRows*0.8))
trainingData <- combo[rows,]
testData <- combo[-rows,]


testDataRows <- as.numeric(rownames(testData))

#combo.pca <- trainingData
combo.pca <- combo

summary(trainingData)
summary(testData)

table(trainingData$type)
table(testData$type)

#### PCA  ####
combo.pca = subset(combo.pca, select = -c(BoilGravity, Name))

rgl::plot3d(combo.pca$type, combo.pca$Color, combo.pca$IBU, col=as.numeric(combo.pca$type))

#beer.df = subset(combo.pca, select = -c(Size.L., SugarScale, BrewMethod, type))
#distances <- mahalanobis(scale(beer.df), center=FALSE, cov=cov(beer.df))
#cutoff_value <- sqrt(qchisq(0.975, ncol(beer.df)))
#combo.pca <- combo.pca[distances < cutoff_value,]
par(mfrow=c(1,2))

quantiSup = c("Size.L.", 
              "BoilSize")
qualiSup = c("SugarScale", "BrewMethod", "type")
testDataRowsA = as.vector(testDataRows)
pca = PCA(combo.pca, quanti.sup = getIndexesByName(combo.pca, quantiSup),
          quali.sup = getIndexesByName(combo.pca, qualiSup),
          ind.sup = testDataRows)

pca$var$cos2

plot(combo.pca$type, combo.pca$Color, col=c(2:10)) # Cloud be interesting?

pca$eig
fviz_pca_ind(pca, col.ind = "contrib")
fviz_eig(pca)
nd = 3


#### First Cluster ####

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
nc = 5
c2 <- cutree(h2,nc)
cdg <- aggregate((diag(freq/sum(freq)) %*% as.matrix(cdclas)),list(c2),sum)[,2:(nd+1)] # WHY WEIGHT 
k6 <- kmeans(Psi,centers=cdg)

Bss <- sum(rowSums(k6$centers^2)*k6$size)
Wss <- sum(k6$withinss)
Ib6 <- 100*Bss/(Bss+Wss)
Ib6

plot(Psi, type="n",main="Clustering of beers in 5 clusters")
text(Psi,col=k6$cluster, cex = 0.6)
abline(h=0,v=0,col="gray")
legend("bottomright",c("c1","c2", "c3", "c4", "c5", "c6", "c7"),pch=20,col=c(1:7))
#points(cdg, col="blue")
text(k6$centers,labels=c("G1","G2", "G3", "G4", "G5", "G6", "G7"),col="blue")

rgl::plot3d(Psi, col=k6$cluster)
points3d(pca$quali.sup$coord[levels(combo.pca$type), 1:3], col = "orange", size=10)
text3d(pca$quali.sup$coord[levels(combo.pca$type), 1:3], texts = levels(combo.pca$type), col="yellow")

(c_des <- catdes(data.frame(as.factor(k6$cluster), combo.pca), num.var = 1))
plot(c_des)


# We can see how Stout and Porter are very simmilar, and very different from all the other classes, with a little research
# Porter is the predecessor of Stout.
plot(combo.pca$type, combo.pca$Color, col=c(2:10))

combo.pca$type[combo.pca$type ==  "Porter"] = as.factor("Stout")
combo.pca$type = as.factor(combo.pca$type)
combo.pca$type = droplevels(combo.pca$type)
table(combo.pca$type)



pca = PCA(combo.pca, quanti.sup = c(1, 6), quali.sup = c(8, 9, 10))

plot(combo.pca$type, combo.pca$Color, col=c(2:10)) # Cloud be interesting?

pca$eig
fviz_pca_ind(pca, col.ind = "contrib")
fviz_eig(pca)
nd = 3


#### Second Cluster with grouped classes ####

pca = PCA(combo.pca, quanti.sup = c(1, 6), quali.sup = c(8, 9, 10))
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

# Green: CLuster 3 -> Stout
# Black: Cluster 1 -> Light
# Red: Cluster 2 -> IPA
# Blue: Cluster 4 -> Lager
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

droplevels(beers.df$Cluster)

totalRows =  nrow(beers.df)

### VALIDATION ####
#### PART 2 ####
categoricalVals <- c("Name", "SugarScale", "BrewMethod", "type")
idx = getIndexesByName(testData, categoricalVals)

(hot = hotelling.test(testData[, -idx], trainingData[, -idx]))
hot$pval
# Since pval is > 0.05, we cannot reject the null hypothesis that they have the same mean of distr.


#### Random Forests ####
table(trainingData$type)
table(testData$type)
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
#                      keep.forest=TRUE)
#   # get the OOB
#   rf.results[ii,"OOB"] <- rf$err.rate[nt,1]
# 
#   ii <- ii+1
# }
# rf.results
# lowest.OOB.error <- as.integer(which.min(rf.results[,"OOB"]))
# (ntrees.best <- rf.results[lowest.OOB.error,"ntrees"])
# bestmtry <- tuneRF(trainingData[, -11], trainingData[,11], stepFactor=1.5, ntree=1000)
# Best result is with 1000 trees and mtry 2
rf <- randomForest(formula = type ~ .,
                   data=trainingData,
                   ntree=1000,
                   mtry=2,
                   importance=TRUE,
                   xtest=subset(testData, select= -type),
                   ytest=testData$type,
                   keep.forest = TRUE)

print(rf)

plot(rf)
legend("topright", colnames(rf$err.rate), col=1:5, cex=0.8, fill=1:5)


par(mfrow=c(2,4))

cr <- colorRamp(c("blue", "red"))

for (i in 1:7){
    plot(rf$importance[,i], col= rgb(cr(abs(rf$importance[,i]/max(rf$importance[,i]))), maxColorValue=255), ylab="Importance", main=colnames(rf$importance)[i])
    text(rf$importance[,i], labels = rownames(rf$importance), pos=2,col= rgb(cr(abs(rf$importance[,i]/max(rf$importance[,i]))), maxColorValue=255))
}
