###############################################

# FIT3164 Final Predictive Model Export
# Team 14
# Start Date: 09-10-2020
# Last Modified Date: 19-10-2020

# This file consists of a function which reads 
# the input json file, fits the tuned random
# forest model using the training data and 
# predicts the probabilities of having bushfire 
# occurrence using the input file.

###############################################
rm(list = ls())

# Load libraries
library(randomForest)
library(rjson)

# Read the sample input file
setwd("/Users/zheningwang/Downloads")

input.file = function() {
  #' Read the input json file and fit a random forest model
  #' @description This function first reads an input json file, converts the json file to a dataframe, 
  #' fits a random forest model using the training data and predicts the bushfire occurrence using the
  #' converted dataframe.
  #' @usage input.file()
  #' @return The predicted probabilities of having bushfire occurrence at each location
  
  file.name <- readline(prompt = "Please enter the file name: ")
  read.file <<- fromJSON(file = file.name)

  # Check if the input json file has all needed variables
  for(i in 1:length(read.file)) {
    if (length(read.file[[i]]) != 6) {
      # If the file does not have all needed variables, stop and raise an error
      stop("Meteorological variables are wrong! 
            The input file should include the following variables:
             1. Max Temperature
             2. Rainfall.mm
             3. Wind.Speed
             4. Relative.Humidity
             5. Average.
             6. Average.Temperature.in.the.Previous.5.Days 
             7. Sum.of.Rainfall.in.the.Previous.5.Days")
    }
  }
  
  # Convert the JSON file to a data frame and use this as the test set
  meteorological_variables <<- data.frame(matrix(0, ncol = 6, nrow = length(read.file)))
  colnames(meteorological_variables) <<- c("Max.Temperature", "Rainfall.mm", "Wind.Speed", "Relative.Humidity", "Average.Temperature.in.the.Previous.5.Days", "Sum.of.Rainfall.in.the.Previous.5.Days")
  
  # Assign each variable to a cell in the matrix
  for(i in 1:length(read.file)) {
    meteorological_variables[i,1:6] <<- rbind(read.file[[i]])
  }

  # Read the aggregated bushfire dataset
  bushfires <<- read.csv('https://raw.githubusercontent.com/Zhening-Wang/BushfireModel-Data-Aggregation-Modelling/master/Aggregated%20Datasets/bushfires_modelling_5days.csv')
  
  # Omit empty records in the dataframe
  bushfires <<- na.omit(bushfires)
  
  # Only keep real-valued attributes
  bushfires <<- bushfires[c(5:11)]
  
  # Convert the target variable to a factor
  bushfires$isBushfire <<- as.factor(bushfires$isBushfire)
  
  # Split the dataset into training set and test set
  set.seed(14)
  train.row = sample(1:nrow(bushfires), 0.7*nrow(bushfires))
  bushfires.train <<- bushfires[train.row,]
  bushfires.test <<- bushfires[-train.row,]
  
  # Fit the tuned Random Forest model and predict using the input file
  set.seed(14)
  bushfires.randomForest <<- randomForest(isBushfire~ ., data = bushfires.train, ntree = 600)
  bushfires.randomForest.predict <<- predict(bushfires.randomForest, meteorological_variables, type = "prob")
  
  # Print the predicted probabilities of having bushfire occurrence
  return(bushfires.randomForest.predict[,2])
}

# Run the input.file() function
input.file()
