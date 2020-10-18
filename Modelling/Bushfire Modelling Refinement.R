############################################################

# FIT3164 Predictive Modelling Refinement
# Team 14
# Start Date: 30-09-2020
# Last Modified Date: 18-10-2020

# This file consists of a number of different refined models,
# namely, 'Cross Validated and Pruned Decision Tree', 'Naive
# Bayes', 'Tuned Bagging', 'Cross Validated Bagging', 'Tuned 
# Boosting', 'Cross Validated Boosting', 'Tuned Random Forest'
# and 'Tuned Artificial Neural Network'. 
# Each of these models has been fitted using the training
# set and predicted bushfire occurrence has been made using 
# the test set. After predicting, the performance of each 
# model has been evaluated subsequently.
# Note: Each of these models has been trained with the tuned 
# parameters and eventually we have chosen the one with the 
# highest accuracy as our final model. 
# i.e. 'Tuned Random Forest'

############################################################

# Clean the environment
rm(list = ls())

# Load libraries
library(docstring)
library(tree)
library(e1071)
library(ROCR)
library(adabag)
library(rpart)
library(randomForest)

# Read the aggregated bushfire dataset
bushfires = read.csv('https://raw.githubusercontent.com/Zhening-Wang/BushfireModel-Data-Aggregation-Modelling/master/Aggregated%20Datasets/bushfires_modelling_5days.csv')

# Omit empty records in the dataframe
bushfires = na.omit(bushfires)

# Only keep real-valued attributes
bushfires = bushfires[c(5:11)]

# Convert the target variable to a factor
bushfires$isBushfire = as.factor(bushfires$isBushfire)

# Split the dataset into training set and test set
set.seed(14)
train.row = sample(1:nrow(bushfires), 0.7*nrow(bushfires))
bushfires.train = bushfires[train.row,]
bushfires.test = bushfires[-train.row,]

######### Decision Tree (cross validation and prune) #########

bushfires.DT.cv = function(train.data, test.data) {
  #' Fit a decition tree model.
  #' @description This function fits a decision tree model using the training data, cross validates
  #' and then prunes the decision tree.
  #' @param train.data dataframe. Training data used for modelling.
  #' @param test.data dataframe. Test data used for predicting.
  #' @usage bushfires.DT(train.data, test.data)
  #' @return The confusion matrix of the pruned decision tree model.
  
  # Set the seed to our team number
  set.seed(14)
  
  # Fit the decision tree model
  bushfires.tree = tree(isBushfire~ ., data = bushfires.train) 
  
  # Cross Validation and Pruning
  bushfires.tree.cv = cv.tree(bushfires.tree, FUN = prune.misclass)
  prune.bushfires.tree = prune.misclass(bushfires.tree, best = 14)
  
  # Predict the bushfire occurrence with pruned decision tree
  prune.bushfires.tree.pred = predict(prune.bushfires.tree, test.data, type = "class")
  confusion.matrix = table(actual = test.data$isBushfire, predicted = prune.bushfires.tree.pred)

  return(confusion.matrix)
}


######### Naive Bayes #########

bushfires.NB = function(train.data, test.data) {
  #' Fit a naive bayes model
  #' This function fits a naive bayes model using the training data and predicts the 
  #' bushfire occurrence using the test data. Confusion matrix, ROC and AUC are evaluated subsequently.
  #' @param train.data dataframe. Training data used for modelling.
  #' @param test.data dataframe. Test data used for predicting.
  #' @usage bushfires.NB(train.data, test.data)
  #' @return The confusion matrix of the naive bayes model.
  #' @note The naive bayes model cannot be further improved as naive bayes is a parametric algorithm.
  
  # Set the seed to our team number
  set.seed(14)
  
  # Fit the naive bayes model
  bushfires.nbfit = naiveBayes(isBushfire~ ., data = train.data) 
  
  # Predict the bushfire occurrence
  bushfires.nbfit.predict = predict(bushfires.nbfit, test.data)
  
  # Confusion Matrix
  confusion.matrix = table(actual = test.data$isBushfire, predicted = bushfires.nbfit.predict)
  
  # Output as confidence levels and draw ROC
  bushfires.pred.NB = predict(bushfires.nbfit, test.data, type = 'raw')
  bushfires.NB.pred = prediction(bushfires.pred.NB[,2], test.data$isBushfire)
  bushfires.NB.perf = performance(bushfires.NB.pred, "tpr", "fpr")
  plot(bushfires.NB.perf, add=TRUE, col="blueviolet")
  
  # Calculate the AUC
  bushfires.NB.auc = performance(bushfires.NB.pred,"auc")
  print(paste("auc:", as.numeric(bushfires.NB.auc@y.values)), sep = "")
  return(confusion.matrix)
}

######### Bagging (tuned mfinal) #########

bushfires.bagging.tuned = function(train.data, test.data) {
  #' Fit a bagging model
  #' @description This function fits a bagging model using the training data and predicts the 
  #' bushfire occurrence using the test data. Confusion matrix, ROC and AUC are evaluated subsequently.
  #' @param train.data dataframe. Training data used for modelling.
  #' @param test.data dataframe. Test data used for predicting.
  #' @usage bushfires.bagging(train.data, test.data)
  #' @return The confusion matrix of the bagging model.
  #' @note This function might take a while to load the result.
  
  # Set the seed to our team number
  set.seed(14)
  
  # Fit the bagging model
  bushfires.bag = bagging(isBushfire~ ., data = train.data, mfinal = 5) 
  
  # Predict the bushfire occurrence
  bushfires.bag.predict = predict.bagging(bushfires.bag, newdata = test.data)
  
  # Confusion Matrix
  confusion.matrix = table(actual=test.data$isBushfire, predicted=bushfires.bag.predict$class)
  
  # Draw ROC
  bushfires.bag.pred = prediction(bushfires.bag.predict$prob[,2], test.data$isBushfire)
  bushfires.bag.perf = performance(bushfires.bag.pred, "tpr", "fpr")
  plot(bushfires.bag.perf, add=TRUE, col="blue")
  
  # Calculate the AUC
  bushfires.bag.auc = performance(bushfires.bag.pred,"auc")
  print(paste("auc:", as.numeric(bushfires.bag.auc@y.values)), sep = "")
  return(confusion.matrix)
}

######### Bagging (cross validation) #########

bushfires.bagging.cv = function(train.data, test.data) {
  #' Fit a cross validated bagging model
  #' @description This function fits a cross validated bagging model using the training data and predicts the 
  #' bushfire occurrence using the test data. The model uses a 3-fold cross validation and runs for 15 iterations.
  #' @param train.data dataframe. Training data used for modelling.
  #' @param test.data dataframe. Test data used for predicting.
  #' @usage bushfires.bagging.cv(train.data, test.data)
  #' @return The confusion matrix of the cross validated bagging model.
  #' @note This function might take a while to load the result.
  
  # Set the seed to our team number
  set.seed(14)

  # Fit the cross validated bagging model
  bushfires.bag.cv = bagging.cv(isBushfire~ ., data = test.data, mfinal = 15, v = 3)
  return(bushfires.bag.cv$confusion)
}

######### Boosting (tuned mfinal) #########

bushfires.boosting.tuned = function(train.data, test.data) {
  #' Fit a boosting model
  #' @description This function fits a boosting model using the training data and predicts the 
  #' bushfire occurrence using the test data. Confusion matrix, ROC and AUC are evaluated subsequently.
  #' @param train.data dataframe. Training data used for modelling.
  #' @param test.data dataframe. Test data used for predicting.
  #' @usage bushfires.boosting(train.data, test.data)
  #' @return The confusion matrix of the boosting model.
  #' @note This function might take a while to load the result.
  
  # Set the seed to our team number
  set.seed(14)
  
  # Fit the boosting model
  bushfires.boost = boosting(isBushfire~ ., data = train.data, mfinal = 115) 
  
  # Predict the bushfire occurrence
  bushfires.boost.predict = predict.boosting(bushfires.boost, newdata = test.data)
  
  # Confusion Matrix
  confusion.matrix = table(actual=test.data$isBushfire, predicted=bushfires.boost.predict$class)
  
  # Draw ROC
  bushfires.boost.pred = prediction(bushfires.boost.predict$prob[,2], test.data$isBushfire)
  bushfires.boost.perf = performance(bushfires.boost.pred, "tpr", "fpr")
  plot(bushfires.boost.perf, add=TRUE, col="red")
  
  # Calculate the AUC
  bushfires.boost.auc = performance(bushfires.boost.pred,"auc")
  print(paste("auc:", as.numeric(bushfires.boost.auc@y.values)), sep = "")
  return(confusion.matrix)
}

######### Boosting (cross validation) #########

bushfires.boosting.cv = function(train.data, test.data) {
  #' Fit a cross validated boosting model
  #' @description This function fits a cross validated boosting model using the training data and predicts the 
  #' bushfire occurrence using the test data. The model uses a 5-fold cross validation and runs for 15 iterations.
  #' @param train.data dataframe. Training data used for modelling.
  #' @param test.data dataframe. Test data used for predicting.
  #' @usage bushfires.boosting.cv(train.data, test.data)
  #' @return The confusion matrix of the cross validated boosting model.
  #' @note This function might take a while to load the result.
  
  # Set the seed to our team number
  set.seed(14)

  # Fit the cross validated boosting model
  bushfires.boost.cv = boosting.cv(isBushfire~ ., data = test.data, mfinal = 15, v = 5)
  return(bushfires.boost.cv$confusion)
}

######### Random Forest (tuned) #########

bushfires.RF.tuned = function(train.data, test.data) {
  #' Fit a random forest model
  #' @description This function fits a random forest model using the training data and predicts the 
  #' bushfire occurrence using the test data. Confusion matrix, ROC and AUC are evaluated subsequently.
  #' @param train.data dataframe. Training data used for modelling.
  #' @param test.data dataframe. Test data used for predicting.
  #' @usage bushfires.RF(train.data, test.data)
  #' @return The confusion matrix of the random forest model.
  
  # Set the seed to our team number
  set.seed(14)
  
  # Fit the Random Forest Model
  bushfires.randomForest = randomForest(isBushfire~ ., data = train.data, ntree = 600)
  
  # Predict the bushfire occurrence
  bushfires.randomForest.predict = predict(bushfires.randomForest, test.data)
  
  # Confusion Matrix
  confusion.matrix = table(actual=test.data$isBushfire, predicted = bushfires.randomForest.predict)
  
  # Draw ROC
  bushfires.pred.RF = predict(bushfires.randomForest, test.data, type = "prob")
  bushfires.RF.pred = prediction(bushfires.pred.RF[,2], test.data$isBushfire)
  bushfires.RF.perf = performance(bushfires.RF.pred, "tpr", "fpr")
  plot(bushfires.RF.perf, add=TRUE, col="darkgreen")
  
  # Calculate the AUC
  bushfires.RF.auc = performance(bushfires.RF.pred,"auc")
  print(paste("auc:", as.numeric(bushfires.RF.auc@y.values)), sep = "")
  return(confusion.matrix)
}

# Run the decision tree model
bushfires.DT.cv(bushfires.train, bushfires.test)

# Run the naive bayes model
bushfires.NB(bushfires.train, bushfires.test)

# Run the bagging model (This function will take a while to load the result)
bushfires.bagging.tuned(bushfires.train, bushfires.test) 

# Run the cross validated bagging model (This function will take a while to load the result)
bushfires.bagging.cv(bushfires.train, bushfires.test)

# Run the boosting model (This function will take a while to load the result)
bushfires.boosting.tuned(bushfires.train, bushfires.test) 

# Run the cross validated boosting model (This function will take a while to load the result)
bushfires.boosting.cv(bushfires.train, bushfires.test)

# Run the random forest model
bushfires.RF.tuned(bushfires.train, bushfires.test)

######### Artificial Neurual Network #########

bushfires.ANN.tuned = function(train.data, test.data) {
  #' Fit an artificial neural network model
  #' @description This function fits an artificial neural network model using the training data and predicts the 
  #' bushfire occurrence using the test data. Confusion matrix is evaluated subsequently.
  #' @param train.data dataframe. Training data used for modelling.
  #' @param test.data dataframe. Test data used for predicting.
  #' @usage bushfires.ANN(train.data, test.data)
  #' @return The confusion matrix of the artificial neural network model.
  #' @note This function will take a while to load the result.
  #' @section Warning: The following libraries must te loaded lastly, after all other models have been run 
  #' successfully and evaluated. 
  #' The reason behind such an action is because the following libraries will alter the properties of the
  #' previous dataframes (factor -> atomic vector), hence will jeopardise the performance of all other models.
  
  # Load libraries
  library(neuralnet)
  library(car)
  
  # Recode the isBushfire column
  train.data$isBushfire = recode(train.data$isBushfire," 'FALSE' = '0';'TRUE' = '1' ")
  test.data$isBushfire = recode(test.data$isBushfire," 'FALSE' = '0';'TRUE' = '1' ")
  
  # Set the seed to our team number
  set.seed(14)
  
  # Fit the ANN model
  bushfires.nn = neuralnet(isBushfire ~ Max.Temperature + Average.Temperature.in.the.Previous.5.Days +
                             Rainfall.mm + Sum.of.Rainfall.in.the.Previous.5.Days + Wind.Speed + Relative.Humidity,
                           train.data, hidden = 3, threshold = 0.01, stepmax = 1e+06, linear.output = FALSE)
  
  # Predict the bushfire occurrence 
  bushfires.nn.pred = compute(bushfires.nn, test.data[c(1:6)])
  result = bushfires.nn.pred$net.result
  
  # Binomial classification, classify the probabilty of greater than 0.3 as to raise the fire alert
  pred = ifelse(result>0.3, TRUE, FALSE)
  
  # Confusion matrix
  table(actual = test.data$isBushfire, predicted = pred[,2])
}

# Run the ANN model (This function will take a while to load the result)
bushfires.ANN.tuned(bushfires.train, bushfires.test) 
