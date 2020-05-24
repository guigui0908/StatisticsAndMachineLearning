#/////////////////LOADING DATA
training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

#We load the library
library(caret)

#We quickly check how is the information
training

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
training = training[ , colSums(is.na(training)) == 0]
  #We now have 59 variables as in the testing set
dim(training)
training

  #We delete all this non-numeric variables, as "classe"
training[,c("X","user_name","raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "num_window")] <- list(NULL)
testing[,c("X","user_name","raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "num_window", "problem_id")] <- list(NULL)



#/////////////////CORRELATION MATRIX
library(corrplot)
mat_correlation = cor(training)
corrplot(mat_correlation, method="color", type="lower")



#/////////////////CLASSIFICATION TREE
#We do a decision tree for determining consistent variables
#We load the rpart library
library(rpart)
Fit <- rpart(classe ~., data=training, method = "class")

#We load the rattle library for a pretty classification tree
library(rattle)
#We display the classification tree
fancyRpartPlot(Fit)

prediction <- predict(Fit,newdata=testing); prediction

#/////////////////RANDOM FOREST
  #We have to wait for a certain time for the function below runs
forestFit <- train(classe ~., data=training, method="rf", prox=TRUE)
forestFit
