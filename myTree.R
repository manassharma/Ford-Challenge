#rpart is used to build the classification tree and fscaret is used for other data processing methods like splitting the training data set
library(rpart)
library("fscaret")

#Setting the working directory
setwd("E:/Datasets/Ford")

print("*****Starting variable preparation phase*****")
myTrain <- read.csv("fordTrain.csv")
myTrain <- data.frame(myTrain)

#Splitting the data set into training and testing
partition <- createDataPartition(myTrain$IsAlert, p=0.80, list=FALSE)
myTrainSet <- data.frame(myTrain[partition,])
myTestSet <- data.frame(myTrain[-partition,])

evaluationIndex <- myTestSet

#Setting the target variable to all 0s(clearing)
myTestSet$IsAlert <- 0

print("*****Starting Model Generation Phase*****")
fit <- rpart(IsAlert ~ V11 + E9 + E5, method = "class", data = myTrainSet)

plot(fit,uniform= TRUE,main= "Classification Tree for Ford Challenge")
text(fit,use.n=TRUE, all=TRUE, cex=.8)
post(fit, title = "Classification Tree for Ford Challenge")
pfit <- prune(fit,cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

#Plotting the prunned classification tree
plot(pfit,uniform=TRUE,main = 'Pruned Classification Tree For Ford Challenge')
text(pfit,use.n=TRUE, all= TRUE, cex=.8)

print("*****Predicting Values*****")
myPrediction <- predict(pfit, newdata = myTestSet, type= 'class')
predictionMetric <- data.frame(myPrediction)

originalCase <- sum(evaluationIndex$IsAlert == 0)
outputCase <- sum(predictionMetric$IsAlert == 0)

########Evaluation of Result###################

target <- as.numeric(as.character(predictionMetric$myPrediction))
result <- mean((myTestSet$IsAlert-target)^2)
print("*****The root mean squared Error is*****")
print(result)

treeResult <- data.frame(actual = myTestSet$IsAlert, calculated = as.numeric(as.character(predictionMetric$myPrediction)))
