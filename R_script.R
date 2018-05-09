# Load and install necessary packages
requiredPackages <- c("FactoMineR",
                      "factoextra",
                      "calibrate",
                      "ggplot2",
                      "ggrepel",
                      "DMwR")

for (pac in requiredPackages) {
  if(!require(pac,  character.only=TRUE)){
    install.packages(pac, repos="http://cran.rstudio.com")
    library(pac,  character.only=TRUE)
  } 
}
rm(pac)
rm(requiredPackages)


# Read
recipe <- read.csv('recipeData.csv',na.strings = "N/A", fileEncoding = "ISO-8859-1")
  head(recipe)
style <- read.csv('stylerecipe.csv')
  head(style)

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

ale_style =  recipe[grep("Ale", recipe$Style, ignore.case=TRUE),]
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

combo = rbind(ale_style, ipa_style, stout_style, lager_style, bitter_style, cider_style, porter_style)
combo = subset(combo, select= -c(Style, URL, StyleID, BeerID, Efficiency))

head(combo)
?grep

# Set factors

combo$Name = as.character(combo$Name)
combo[is.na(combo$Name),1] = "Unknown Beer"
combo$Name = as.factor(combo$Name)
combo$type = as.factor(combo$type)
droplevels(combo$type)
droplevels(combo$Name)


# Impute Boil Gravity
a = knnImputation(data = combo, k=5)

# Check for Outliers, remove all bigger than 658 since thats the most bitter beer in the world (else all outliers)
combo = combo[combo$IBU < 658,]

plot(combo$IBU)
text(combo$IBU, labels=combo$Name)
# TODO: Do scatter matrix

# PCA
combo.pca = subset(combo, select = -c(BoilGravity, Name))
par(mfrow=c(1,2))
pca = PCA(combo.pca, quanti.sup = c(1, 7), quali.sup = c(9, 10, 11))

pca$eig
fviz_pca_ind(pca, col.ind = "contrib")
fviz_eig(pca)
nd = 3


# Cluster

Psi = pca$ind$coord[, 1:nd]
# dist_matrix = dist(Psi)
# write.matrix(dist_matrix, "dist_matrix.csv")
#cluster <- hclust(dist_matrix, method='ward.D2')

# Cluster for large data
n1 = 100
k1 <- kmeans(Psi, n1)
k2 <- kmeans(Psi, n1)
table(k2$cluster,k1$cluster)
clas <- (k2$cluster-1)*n1+k1$cluster
freq <- table(clas) 
cdclas <- aggregate(as.data.frame(Psi),list(clas),mean)[,2:(nd+1)]
cdclas
d2 <- dist(cdclas)
h2 <- hclust(d2,method="ward.D2",members=freq) # COMPARE THE COST
plot(h2)
barplot(h2$height[(nrow(cdclas)-40):(nrow(cdclas)-1)]) # PLOT OF THE LAST 39 AGGREGATIONS 
nc = 7
c2 <- cutree(h2,nc)
cdg <- aggregate((diag(freq/sum(freq)) %*% as.matrix(cdclas)),list(c2),sum)[,2:(nd+1)] # WHY WEIGHT 
k6 <- kmeans(Psi,centers=cdg)

Bss <- sum(rowSums(k6$centers^2)*k6$size)
Wss <- sum(k6$withinss)
Ib6 <- 100*Bss/(Bss+Wss)
Ib6

plot(Psi, type="n",main="Clustering of countries in 2 clusters")
text(Psi,col=k6$cluster, cex = 0.6)
abline(h=0,v=0,col="gray")
legend("bottomright",c("c1","c2", "c3", "c4", "c5", "c6", "c7"),pch=20,col=c(1:7))
#points(cdg, col="blue")
text(k6$centers,labels=c("G1","G2", "G3", "G4", "G5", "G6", "G7"),col="blue")

