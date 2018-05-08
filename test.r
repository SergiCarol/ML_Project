library(readr)
library(DMwR)
recipeData <- read_csv("recipeData.csv")

summary(recipeData)




recipes <- subset(recipeData, select=-c(Name,URL,Style,UserId, PrimingMethod,PrimingAmount,SugarScale,BrewMethod))
recipes$BoilGravity <- as.numeric(recipes$BoilGravity)
recipes$MashThickness <- as.numeric(recipes$MashThickness)
recipes$PitchRate <- as.numeric(recipes$PitchRate)
recipes$PrimaryTemp <- as.numeric(recipes$PrimaryTemp)
summary(recipes)


fill_missings <-knnImputation(recipes)


View(recipeData[48527,]) # Outlier for IBU