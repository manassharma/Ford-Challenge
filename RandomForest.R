#randomForest is used to build the ensemble forest and fscaret is used for other data processing methods like splitting the training data set
library("randomForest")
library("fscaret")


#Setting the working directory
setwd("E:/Datasets/Ford")

print("*****Starting variable preparation phase*****")
myTrain <- read.csv("fordTrain.csv")
myTrain$IsAlert <- as.factor(myTrain$IsAlert)

#Splitting the data set into training and testing
partition <- createDataPartition(myTrain$IsAlert, p=0.20, list=FALSE)
myTrainSet <- data.frame(myTrain[partition,])
myTestSet <- data.frame(myTrain[-partition,])

print("*****Starting Model Generation Phase*****")
myRF <- randomForest(myTrainSet, myTrainSet$IsAlert, do.trace = TRUE, importance=TRUE, method="class", ntree=100, forest=TRUE)
plot(myRF, uniform=TRUE, main = "MY RANDOM FOREST")

print("*****Predicting Values*****")
myPrediction <- predict(myRF, newdata = myTestSet, type= 'class')

predictionMetric <- data.frame(myPrediction)

randomForestResult <- data.frame(actual=myTestSet$IsAlert, calculated=myPrediction)

target <- as.numeric(as.character(predictionMetric$myPrediction))
myTestSet$IsAlert <- as.numeric(as.character(myTestSet$IsAlert))
result <- mean((myTestSet$IsAlert-target)^2)
print("*****The root mean squared Error is*****")
sqrt(result)


###The snippet below was used for model evaluation on unseen data########


#myGenEval <- head(myTestSet, 2000)
#myGenEval$IsAlert <- as.factor(myGenEval$IsAlert)
#myGenEval <- data.frame(myGenEval)
#generalizedPrediction <- predict(myRF, newdata = myGenEval, type='class')
#print()
#backup <- head(myTestSet, 2000)
#target <- as.numeric(as.character(predictionMetric$myPrediction))
#result <- mean((myTestSet$IsAlert-target)^2)
#result <- sqrt(result)

#count <- 0
#for(i in 1:2000){
 # + if(backup$IsAlert[i] != myGenEval$IsAlert[i]){
  #  + count = count+1}
  #+ }