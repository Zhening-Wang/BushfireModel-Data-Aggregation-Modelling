####################################################

# FIT3163_FIT3164 Project Data Aggregation 
# Written By Zhening Wang  
# Version: 1.0 
# Start Date: 08/08/2020 
# Last Modified: 10/08/2020 

####################################################

####################### Setup ######################
rm(list = ls())
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets")

####################################################

################## Melbourne #######################

# Melbourne Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
mel_temp = read.csv("melbourne-max-tmp.csv")
mel_temp = mel_temp[1:6]
mel_temp = na.omit(mel_temp)

# Melbourne Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
mel_rain = read.csv("melbourne-rainfall.csv")
mel_rain = na.omit(mel_rain)
mel_temp$isFire = FALSE

