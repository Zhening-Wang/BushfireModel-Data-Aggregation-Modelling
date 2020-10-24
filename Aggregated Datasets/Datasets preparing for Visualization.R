####################################################

# FIT3163_FIT3164 Project Data Aggregation 
# Written By Sijia Yu  
# Version: 1.0
# Start Date: 08/08/2020 
# Last Modified: 17/10/2020 

####################################################

### Read files ###
rm(list = ls())
setwd("~/Desktop/Finalized datasets")
df=read.csv("bushfire_temp_finalized.csv")

### Extract the records of the date that bushfire happened ###
### This file will be used in visualization to indicate the date of bushfires ###
Fire=df[df$isFire==TRUE,]
write.csv(Fire,"is_Fire_finalized.csv",row.names = FALSE)

## Get the coordinates for locations that had bushfires (merge coordinates data and temperture data) ###
## Make sure the information(bushfire name) match with the other 2 datasets ###
## This datasets will only have the records for bushfires after 1944 ###
cor=read.csv("bushfire_coordinates.csv")
cor=cor[,c(1:4,11,12)] # only get the column of Date, Nickname,State, Location,Latitude, longtitude
uniq=unique(df[,c(2:4)]) # extract the locations for each bushfire
cor=merge(cor,uniq,by.x=c("Nickname","State","Location"), by.y=c("Nickname","State","Location"))
write.csv(cor,"Fire_Coordinate_Finalized.csv",row.names = FALSE)



