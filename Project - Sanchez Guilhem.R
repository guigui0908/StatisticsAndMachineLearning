#/////////////////LOADING LIBRARIES

library(caret)
library(corrplot)
library(rpart)
library(rattle)


#/////////////////LOADING DATA
training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")



#/////////////////REMOVING ZERO COVARIATES

  #First, we see variables that have a very variability and will not be predictors
training_nsv = nearZeroVar(training, saveMetrics = TRUE); training_nsv
testing_nsv = nearZeroVar(testing, saveMetrics = TRUE); testing_nsv

  #We delete all the columns that are marked like TRUE in the "nzv" column because this predictor is a near zero variance predictor
training = training[,!training_nsv$nzv]
testing = testing[,!testing_nsv$nzv]

  #We see we have less variable (100 in the training data, 59 in the testing one)
dim(training); dim(testing)




#/////////////////REMOVING VARIABLES CONTAINING "NA"
training = training[, colSums(is.na(training)) == 0]

  #We now have 59 variables as in the testing set
dim(training)



#/////////////////REMOVING MANUALLY PARTICULIAR VARIABLES
  #We delete all this non-numeric variables, as "classe"
training[,c("X","user_name","raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "num_window")] <- list(NULL)
testing[,c("X","user_name","raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "num_window")] <- list(NULL)

dim(training)
dim(testing)


#/////////////////CORRELATION MATRIX

mat_correlation = cor(training[,-53])
corrplot(mat_correlation, method="color", type="lower")

#/////////////////DIMENSION

set.seed(33333)

#/////////////////ESTABLISHING TRAINING AND TESTING SAMPLES

inTrain <- createDataPartition(training$classe, p=0.75, list=FALSE)
trainingSample = training[inTrain,]
testingSample = training[-inTrain,]

dim(trainingSample)
dim(testingSample)



#/////////////////CLASSIFICATION TREE

FitTree <- rpart(classe ~., data=trainingSample, method = "class")

#We display the classification tree
fancyRpartPlot(FitTree, sub ="Classification Tree")

predictionTree <- predict(FitTree, newdata=testingSample, type = "class")

accuracyTree = postResample(predictionTree, testingSample$classe); accuracyTree

realTree = predict(FitTree, newdata=testing, type="class"); realTree




#/////////////////BOOSTING

#We put verbose with FALSE because the gbm method can produce a lot of outputs without saying the list is false
FitBoosting = train(classe ~., data = trainingSample, method ="gbm", verbose = FALSE,
                    trControl = trainControl(method = "cv", 5))

predictBoosting = predict(FitBoosting, newdata = testingSample); predictBoosting

BoostingTable = table(predictionBoosting, testingSample$classe)

qplot(predictBoosting, classe, data=testingSample)
sum(predictionBoosting != testingSample$classe)


accuracyBoosting = postResample(predictBoosting, testingSample$classe); accuracyBoosting

realBoosting = predict(FitBoosting, newdata = testing); realBoosting


#/////////////////LINEAR DISCRIMINANT ANALYSIS AND NAIVE BAYES

#lda is for linear discriminant analysis
FitLDA = train(classe ~., data=trainingSample, method="lda",
               trControl = trainControl(method = "cv", 5))

#nb is for Naives Bayes
FitNB = train(classe ~., data=trainingSample, method="nb",
              trControl = trainControl(method = "cv", 5))

predictionLDA = predict(FitLDA, newdata = testingSample); predictionLDA

predictionNB = predict(FitNB, newdata = testingSample); predictionNB

accuracyLDA = postResample(predictionLDA, testingSample$classe); accuracyLDA
accuracyNB = postResample(predictionNB, testingSample$classe); accuracyNB

realLDA = predict(FitLDA, newdata = testing);realLDA

realNB = predict(FitNB, newdata = testing);realNB

#/////////////////CONCLUSION

realBoosting

qplot((1:20), realBoosting, colour=realBoosting,
      xlab = "Observation Number", ylab = "Prediction of the classe")



