############################################

# FIT3164 Data Preparation for Visualisation
# Fetch longitude and latitude explicitly
# Team 14
# Start Date: 25-08-2020
# Last Modified Date: 22-10-2020

# This file contains a function which 
# fetches the longitude and the latitude of
# bushfire locations using Google Maps 
# platform.
# Note: The API key might be expired by the 
# time you are working through this file,
# contact me at zwan0040@student.monash.edu
# for further assistance.

############################################

# Clean the environment, load the required library and API key
rm(list = ls())
library(ggmap)
register_google(key = "AIzaSyDQOXhrpiBfvUMxY1ahFaC-KQSV60DwsQ8")

# read the file
bushfires = read.csv("https://raw.githubusercontent.com/Zhening-Wang/BushfireModel-Data-Aggregation-Modelling/master/Aggregated%20Datasets/bushfires_temperature_exact.csv")

# add the country name after the location since there might be other places in the world with the same name
locations = bushfires$Location
locations = paste(locations, ", Australia")

# create the fetch function
fetch.long.lat = function(location){
  #' Fetch the longitude and the latitude.
  #' @description This function fetches the longitude and the latitude of the given location from the 
  #' Google Maps platform.
  #' @param location character. The name of the place where we want to know the longitude and latitude of.
  #' @usage fetch.long.lat(location)
  #' @return The fetched longitude and the latitude of the location.
  
  # fetch the longitude and the latitude of the provided location
  fetch = geocode(location, output = "all", messaging=TRUE, override_limit = TRUE)
  result = data.frame(longitude=NA, latitude=NA, status=NA)
  result$status = fetch$status
  
  # if something wrong happens, terminate the function and return the result
  if (fetch$status != "OK") {
    return(result)
  }
  
  # if all good, assign the fetched data to the return data
  result$longitude = fetch$results[[1]]$geometry$location$lng
  result$latitude = fetch$results[[1]]$geometry$location$lat
  
  # return the final data frame
  return(result)
  
}

# create a temporary data frame
fetched = data.frame()

# fetch the longitude and the latitude for the list of bushfire locations
for(i in 1:length(locations))
{
  result = fetch.long.lat(locations[i])
  result$index = i
  fetched = rbind(fetched, result)
}

# insert the fetched data to the dataset
bushfires$longitude = fetched$long
bushfires$latitude = fetched$lat

# export the final dataset
write.csv(bushfires, file = "temp.csv", row.names = FALSE)
