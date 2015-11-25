The datasets have been included in this package:

fordTrain - Training dataset.
fordTest - Testing dataset.
Solution - Ford's solution for the testing dataset (holdout/cross-validation data).

The source code included in the package use the following libraries:

pROC
fscaret
randomForest
rpart

Kindly ensure that they are downloaded on your work environment.

Example to pull libraries from the R central repository is as follows:
install.packages("packageName")

Also ensure that your working directory points to the location of the dataset files in your local file system
Can be set with the R command - setwd("localFSPath")