#rpart is used to build the classification tree and fscaret is used for other data processing methods like splitting the training data set
library(rpart)
library("fscaret")
library(pROC)

#Setting the working directory
setwd("E:/Datasets/Ford")

print("*****Starting variable preparation phase*****")
myTrain <- read.csv("fordTrain.csv")
myTrain <- data.frame(myTrain)
rpartTest <- read.csv("fordTest.csv")
rpartTest <- data.frame(rpartTest)
eval <- read.csv("Solution.csv")
eval <- data.frame(eval)


#Setting the target variable to all 0s(clearing)
rpartTest$IsAlert <- 0

print("*****Starting Model Generation Phase*****")
fit <- rpart(IsAlert ~ V11 + E9 + E5, method = "class", data = myTrain)

plot(fit,uniform= TRUE,main= "Classification Tree for Ford Challenge")
text(fit,use.n=TRUE, all=TRUE, cex=.8)
post(fit, title = "Classification Tree for Ford Challenge")
pfit <- prune(fit,cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

#Plotting the prunned classification tree
plot(pfit,uniform=TRUE,main = 'Pruned Classification Tree For Ford Challenge')
text(pfit,use.n=TRUE, all= TRUE, cex=.8)

print("*****Predicting Values*****")
myPrediction <- predict(pfit, newdata = rpartTest, type= 'class')
predictionMetric <- data.frame(myPrediction)

originalCase <- sum(eval$Indicator == 0)
outputCase <- sum(predictionMetric$IsAlert == 0)

########Evaluation of Result###################

target <- as.numeric(as.character(predictionMetric$myPrediction))
result <- mean((eval$Prediction-target)^2)

print("*****The root mean squared Error is*****")
print(result)

treeResult <- data.frame(actual = eval$Prediction, calculated = as.numeric(as.character(predictionMetric$myPrediction)))

print("***Confusion Matrix is as Follows***")
table(treeResult$actual, treeResult$calculated)

print("***Plotting ROC for the classification tree***")
rpartPlot <- roc(treeResult$actual, treeResult$calculated, ci=TRUE, of="thresholds", thresholds=0.9)
rpartPlot
plot(rpartPlot)