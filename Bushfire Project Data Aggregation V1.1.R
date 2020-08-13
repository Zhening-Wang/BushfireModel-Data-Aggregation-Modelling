####################################################

# FIT3163_FIT3164 Project Data Aggregation 
# Written By Zhening Wang  
# Version: 1.1 
# Start Date: 08/08/2020 
# Last Modified: 13/08/2020 
# The first round of data aggregation is done mainly 
# for the purpose of visualising data

####################################################

####################### Setup ######################

rm(list = ls())
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets")
library(dplyr)

####################################################

################## Adelaide ########################

# Adelaide Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
ade_temp = read.csv("adelaide-max-tmp.csv")
ade_temp[is.na(ade_temp)] = 0

#ade_temp = na.omit(ade_temp)
ade_temp = ade_temp[2:6]
ade_temp = subset(ade_temp, ade_temp$Year < 2020)

# Adelaide Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
ade_rain = read.csv("adelaide-rainfall.csv")

ade_rain = subset(ade_rain, ade_rain$Year > 1886)
ade_rain = subset(ade_rain, ade_rain$Year < 2020)

ade_rain[is.na(ade_rain)] = 0
#ade_rain = na.omit(ade_rain)
ade_rain = ade_rain[3:6]

# Combine Adelaide Temperature and Rainfall together
ade = cbind(ade_rain, ade_temp)
ade = ade[c(5,1:4,9)]
ade$isFire = FALSE

ade = unite(ade, "Date", c("Year", "Month", "Day"), sep ="-")
ade$Date = as.Date(ade$Date, format = "%Y-%m-%d")

write.csv(ade, "/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Aggregated/Adelaide.csv", row.names = FALSE)

################## Brisbane ########################

# Brisbane Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
bri_temp = read.csv("brisbane-max-tmp.csv")

bri_temp[is.na(bri_temp)] = 0

bri_temp = bri_temp[2:6]

# Brisbane Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
bri_rain = read.csv("brisbane-rainfall.csv")

bri_rain[is.na(bri_rain)] = 0
bri_rain = bri_rain[2:6]

bri_rain = unite(bri_rain, "Date", c("Year", "Month", "Day"), sep = "-")
bri_rain$Date = as.Date(bri_rain$Date, format = "%Y-%m-%d")

bri_rain = bri_rain[as.Date(bri_rain$Date, "%Y-%m-%d") > as.Date("1886-12-31", "%Y-%m-%d"),]
bri_rain = bri_rain[as.Date(bri_rain$Date, "%Y-%m-%d") < as.Date("1986-04-01", "%Y-%m-%d"),]

# Combine Brisbane Temperature and Rainfall together
bri = cbind(bri_rain, bri_temp)
bri = bri[c(1:3,8)]
bri$isFire = FALSE

write.csv(bri, "/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Aggregated/Brisbane.csv", row.names = FALSE)

################## Canberra ########################

# Canberra Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
can_temp = read.csv("canberra-max-tmp.csv")
can_temp[is.na(can_temp)] = 0
can_temp = can_temp[2:6]

can_temp = unite(can_temp, "Date", c("Year", "Month", "Day"), sep = "-")
can_temp$Date = as.Date(can_temp$Date, format = "%Y-%m-%d")

# Canberra Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
can_rain = read.csv("canberra-rainfall.csv")
can_rain[is.na(can_rain)] = 0
can_rain = can_rain[2:6]

can_rain = unite(can_rain, "Date", c("Year", "Month", "Day"), sep = "-")
can_rain$Date = as.Date(can_rain$Date, format = "%Y-%m-%d")
can_rain = can_rain[as.Date(can_rain$Date,"%Y-%m-%d") < as.Date("2010-12-01", "%Y-%m-%d"),]

# Combine Canberra Temperature and Rainfall together
can = cbind(can_rain, can_temp)
can = can[c(1:3, 6)]
can$isFire = FALSE

write.csv(can, "/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Aggregated/Canberra.csv", row.names = FALSE)

################### Darwin ########################

# Darwin Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
dar_temp = read.csv("darwin-max-tmp.csv")
dar_temp[is.na(dar_temp)] = 0
dar_temp = dar_temp[2:6]

dar_temp = unite(dar_temp, "Date", c("Year", "Month", "Day"), sep = "-")
dar_temp$Date = as.Date(dar_temp$Date, format = "%Y-%m-%d")

# Darwin Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
dar_rain = read.csv("darwin-rainfall.csv")
dar_rain[is.na(dar_rain)] = 0

dar_rain = unite(dar_rain, "Date", c("Year", "Month", "Day"), sep = "-")
dar_rain$Date = as.Date(dar_rain$Date, format = "%Y-%m-%d")
dar_rain = dar_rain[as.Date(dar_rain$Date,"%Y-%m-%d") < as.Date("2020-01-06", "%Y-%m-%d"),]
dar_rain = dar_rain[3:4]

# Combine Darwin Temperature and Rainfall together
dar = cbind(dar_rain, dar_temp)
dar = dar[c(3, 1, 2, 5)]
dar$isFire = FALSE

write.csv(dar, "/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Aggregated/Darwin.csv", row.names = FALSE)

################### Hobart ########################

# Hobart Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
hob_temp = read.csv("hobart-max-tmp.csv")
hob_temp[is.na(hob_temp)] = 0

hob_temp = unite(hob_temp, "Date", c("Year", "Month", "Day"), sep = "-")
hob_temp$Date = as.Date(hob_temp$Date, format = "%Y-%m-%d")

hob_temp = hob_temp[2:4]

hob_temp = hob_temp[as.Date(hob_temp$Date,"%Y-%m-%d") > as.Date("1892-12-31", "%Y-%m-%d"),]

# Hobart Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
hob_rain = read.csv("hobart-rainfall.csv")
hob_rain[is.na(hob_rain)] = 0

hob_rain = unite(hob_rain, "Date", c("Year", "Month", "Day"), sep = "-")
hob_rain$Date = as.Date(hob_rain$Date, format = "%Y-%m-%d")

hob_rain = hob_rain[c(3,4)]

hob_rain = hob_rain[as.Date(hob_rain$Date,"%Y-%m-%d") < as.Date("2020-01-06", "%Y-%m-%d"),]

# Combine Hobart Temperature and Rainfall together
hob = cbind(hob_rain, hob_temp)
hob = hob[c(3, 1, 2, 5)]
hob$isFire = FALSE


write.csv(hob, "/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Aggregated/Hobart.csv", row.names = FALSE)

################## Melbourne #######################

# Melbourne Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
mel_temp = read.csv("melbourne-max-tmp.csv")
mel_temp[is.na(mel_temp)] = 0

mel_temp = unite(mel_temp, "Date", c("Year", "Month", "Day"), sep = "-")
mel_temp$Date = as.Date(mel_temp$Date, format = "%Y-%m-%d")

mel_temp = mel_temp[2:4]

# Melbourne Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
mel_rain = read.csv("melbourne-rainfall.csv")
mel_rain[is.na(mel_rain)] = 0

mel_rain = unite(mel_rain, "Date", c("Year", "Month", "Day"), sep = "-")
mel_rain$Date = as.Date(mel_rain$Date, format = "%Y-%m-%d")
mel_rain = mel_rain[as.Date(mel_rain$Date,"%Y-%m-%d") < as.Date("2015-01-06", "%Y-%m-%d"),]
mel_rain = mel_rain[c(3,4)]

# Combine Melbourne Temperature and Rainfall together
mel = cbind(mel_rain, mel_temp)
mel = mel[c(3, 1, 2, 5)]
mel$isFire = FALSE

write.csv(mel, "/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Aggregated/Melbourne.csv", row.names = FALSE)

################## Perth #######################

# Perth Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
per_temp = read.csv("perth-max-tmp.csv")
per_temp[is.na(per_temp)] = 0

per_temp = unite(per_temp, "Date", c("Year", "Month", "Day"), sep = "-")
per_temp$Date = as.Date(per_temp$Date, format = "%Y-%m-%d")

per_temp = per_temp[2:4]

# Perth Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
per_rain = read.csv("perth-rainfall.csv")
per_rain[is.na(per_rain)] = 0

per_rain = unite(per_rain, "Date", c("Year", "Month", "Day"), sep = "-")
per_rain$Date = as.Date(per_rain$Date, format = "%Y-%m-%d")
per_rain = per_rain[as.Date(per_rain$Date,"%Y-%m-%d") > as.Date("1896-12-31", "%Y-%m-%d"),]
per_rain = per_rain[as.Date(per_rain$Date,"%Y-%m-%d") < as.Date("1992-04-30", "%Y-%m-%d"),]

per_rain = per_rain[c(3,4)]

# Combine Perth Temperature and Rainfall together
per = cbind(per_rain, per_temp)
per = per[c(3, 1, 2, 5)]
per$isFire = FALSE

write.csv(per, "/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Aggregated/Perth.csv", row.names = FALSE)

################### Sydney ########################

# Sydney Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
syd_temp = read.csv("sydney-max-tmp.csv")
syd_temp[is.na(syd_temp)] = 0

syd_temp = unite(syd_temp, "Date", c("Year", "Month", "Day"), sep = "-")
syd_temp$Date = as.Date(syd_temp$Date, format = "%Y-%m-%d")

syd_temp = syd_temp[2:4]

# Sydney Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
syd_rain = read.csv("sydney-rainfall.csv")
syd_rain[is.na(syd_rain)] = 0

syd_rain = unite(syd_rain, "Date", c("Year", "Month", "Day"), sep = "-")
syd_rain$Date = as.Date(syd_rain$Date, format = "%Y-%m-%d")

syd_rain = syd_rain[as.Date(syd_rain$Date,"%Y-%m-%d") > as.Date("1858-12-31", "%Y-%m-%d"),]
syd_rain = syd_rain[as.Date(syd_rain$Date,"%Y-%m-%d") < as.Date("2020-01-06", "%Y-%m-%d"),]

syd_rain = syd_rain[c(3,4)]

# Combine Sydney Temperature and Rainfall together
syd = cbind(syd_rain, syd_temp)
syd = syd[c(3, 1, 2, 5)]
syd$isFire = FALSE

write.csv(syd, "/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Aggregated/Sydney.csv", row.names = FALSE)












