# FIT3164 Team 14 Bushfire Project Data Aggregation and Modelling

This repository contains three folders:

1.  Historical Datasets

    This folder contains all raw historical bushfire records from 1898 to 2015, along with their meteorological attributes.
    
2. Aggregated Datasets
  
    This folder contains aggregated bushfire records, some bushfire records have been deliberately omitted as some of their meteorological variables were beyond 
    our collection approach and could not be obtained within the budget.
    
    In addition to the csv files, several R files are also included. 
    
    The 'Fetch Long Lat.R' file fetches the longitude and latitude of all bushfire locations and writes the generated result to an CSV file.
    The 'Bushfire Project Data Aggregation.R' file and the 'Bushfire Project Data Aggregation V1.1.R' file are two R files that aggregate the preliminary 'max           temperature' and 'rainfall' data provided by the teaching team. 

    However, as the number of collected datasets increases, the automated aggeregation process becomes impossible to accomplish as these datasets come from multiple     sources and the name for each dataset varies hugely. Hence, we had to compromise and implement some manual aggregations using Excel.
    
3. Modelling
  
    This folder contains three R files:
    
    Bushfire Preliminary Modelling.R: This R file consists of different predictive models, all these models have been trained with default parameters.
    
    Bushfire Modelling Refinement.R: This R file consists of different refined predictive models, all these models have been trained with tuned parameters or have                                        been cross validated.
    
    Final Random Forest Model.R: This R file consists of the tuned random forest model that we will be using as our final predictive model.
    
If you come across any issues while working through this repository, please contact me at zwan0040@student.monash.edu
