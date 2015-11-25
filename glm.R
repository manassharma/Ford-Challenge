#fscaret is used for data processing methods like splitting the training data set
library("fscaret")
library(pROC)

#Setting the working directory
setwd("E:/Datasets/Ford")

print("*****Starting variable preparation phase*****")
myTrain <- read.csv("fordTrain.csv")
glmTest <- read.csv("fordTest.csv")
solution <- read.csv("Solution.csv")

print("*****Starting Model Generation Phase*****")
svp <- glm(IsAlert ~., data=myTrain, family = binomial)

#plot(svp, uniform=TRUE, main = "GLM Plot")

print("*****Predicting Values*****")
pred <- predict(svp, newdata = glmTest, type="response")

count <-0
for(i in 1: nrow(glmTest)){
  if(pred[[i]] > 0.5){
    pred[[i]] = 1
  }
  else{
    pred[[i]] = 0
  }
}

glmResult <- data.frame(actual=solution$Prediction, calculated=pred)

count <-0
for(i in 1: nrow(glmTest)){
  if(pred[[i]] > 0.5){
    pred[[i]] = 1
  }
  else{
    pred[[i]] = 0
  }
}


errorCount <-0
for(i in 1: nrow(glmTest)){
  if(pred[[i]] != solution$Prediction[i]){
    errorCount = errorCount+1
  }
}

error <- errorCount/nrow(glmTest)
print("Net error is")
print(error)


print("***Confusion Matrix is as Follows***")
table(glmResult$actual, glmResult$calculated)

print("***Plotting Results***")
glmPlot <- roc(glmResult$actual, glmResult$calculated, ci=TRUE, of="thresholds", thresholds=0.9)
glmPlot
plot(glmPlot)