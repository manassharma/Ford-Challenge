#randomForest is used to build the ensemble forest and fscaret is used for other data processing methods like splitting the training data set
library("randomForest")
library("fscaret")
library(pROC)

#Setting the working directory
setwd("E:/Datasets/Ford")

print("*****Starting variable preparation phase*****")
myTrain <- read.csv("fordTrain.csv")
fTest <- read.csv("fordTest.csv")
solution <- read.csv("Solution.csv")
partition <- createDataPartition(myTrain$IsAlert, p=0.20, list=FALSE)
myTrain <- data.frame(myTrain[partition,])


print("*****Starting Model Generation Phase*****")
myRF <- randomForest(IsAlert ~., data = myTrain, do.trace = TRUE, importance=TRUE, method="class", ntree=100, forest=TRUE)
plot(myRF, uniform=TRUE, main = "MY RANDOM FOREST")

print("*****Predicting Values*****")
myPrediction <- predict(myRF, newdata = fTest, type= 'class')

predictionMetric <- data.frame(myPrediction)

randomForestResult <- data.frame(actual=solution$Prediction, calculated=round(myPrediction))

target <- round((predictionMetric$myPrediction))
fTest$IsAlert <- as.numeric(as.character(fTest$IsAlert))
result <- mean((fTest$IsAlert-target)^2)
print("*****The root mean squared Error is*****")
sqrt(result)

print("***Confusion matrix is as follows***")
table(randomForestResult$actual, randomForestResult$calculated)

print("***Plotting results***")
randomForestPlot <- roc(randomForestResult$actual, randomForestResult$calculated, ci=TRUE, of="thresholds", thresholds=0.9)
randomForestPlot
plot(randomForestPlot)
