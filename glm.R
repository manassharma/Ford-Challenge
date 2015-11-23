#fscaret is used for data processing methods like splitting the training data set
library("fscaret")

#Setting the working directory
setwd("E:/Datasets/Ford")

print("*****Starting variable preparation phase*****")
myTrain <- read.csv("fordTrain.csv")
partition <- createDataPartition(myTrain$IsAlert, p=0.60, list=FALSE)

myTrainSetGLM <- data.frame(myTrain[partition,])
myTestSetGLM <- data.frame(myTrain[-partition,])

print("*****Starting Model Generation Phase*****")
svp <- glm(IsAlert ~., data=myTrainSetGLM, family = binomial)

#plot(svp, uniform=TRUE, main = "GLM Plot")

print("*****Predicting Values*****")
pred <- predict(svp, newdata = myTestSetGLM, type="response")

count <-0
for(i in 1: 241731){
  if(pred[[i]] > 0.5){
    pred[[i]] = 1
  }
  else{
    pred[[i]] = 0
  }
}

glmResult <- data.frame(actual=myTestSetGLM$IsAlert, calculated=pred)

count <-0
for(i in 1: 241731){
  if(pred[[i]] > 0.5){
    pred[[i]] = 1
  }
  else{
    pred[[i]] = 0
  }
}


errorCount <-0
for(i in 1: 241731){
  if(pred[[i]] != myTestSetGLM$IsAlert[i]){
    errorCount = errorCount+1
  }
}

error <- errorCount/nrow(myTestSetGLM)
print("Net error is")

print(error)
