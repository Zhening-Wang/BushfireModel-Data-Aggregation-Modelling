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

################## Adelaide ########################

# Adelaide Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
ade_temp = read.csv("adelaide-max-tmp.csv")
ade_temp = na.omit(ade_temp)
ade_temp = ade_temp[2:6]

# Adelaide Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
ade_rain = read.csv("adelaide-rainfall.csv")
ade_rain = na.omit(ade_rain)
ade_rain = ade_rain[3:6]

# Combine Adelaide Temperature and Rainfall together
ade = merge(ade_temp, ade_rain, by = c("Year", "Month", "Day"))
ade = ade[c(4,1:3,5,6)]
ade$isFire = FALSE

################## Brisbane ########################

# Brisbane Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
bri_temp = read.csv("brisbane-max-tmp.csv")
bri_temp = na.omit(bri_temp)
bri_temp = bri_temp[2:6]

# Brisbane Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
bri_rain = read.csv("brisbane-rainfall.csv")
bri_rain = na.omit(bri_rain)
bri_rain = bri_rain[3:6]

# Combine Brisbane Temperature and Rainfall together
bri = merge(bri_temp, bri_rain, by = c("Year", "Month", "Day"))
bri = bri[c(4,1:3,5,6)]
bri$isFire = FALSE

################## Canberra ########################

# Canberra Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
can_temp = read.csv("canberra-max-tmp.csv")
can_temp = na.omit(can_temp)
can_temp = can_temp[2:6]

# Canberra Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
can_rain = read.csv("canberra-rainfall.csv")
can_rain = na.omit(can_rain)
can_rain = can_rain[3:6]

# Combine Canberra Temperature and Rainfall together
can = merge(can_temp, can_rain, by = c("Year", "Month", "Day"))
can = can[c(4,1:3,5,6)]
can$isFire = FALSE

################### Darwin ########################

# Darwin Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
dar_temp = read.csv("darwin-max-tmp.csv")
dar_temp = na.omit(dar_temp)
dar_temp = dar_temp[2:6]

# Darwin Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
dar_rain = read.csv("darwin-rainfall.csv")
dar_rain = na.omit(dar_rain)
dar_rain = dar_rain[3:6]

# Combine Darwin Temperature and Rainfall together
dar = merge(dar_temp, dar_rain, by = c("Year", "Month", "Day"))
dar = dar[c(4,1:3,5,6)]
dar$isFire = FALSE

################### Hobart ########################

# Hobart Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
hob_temp = read.csv("hobart-max-tmp.csv")
hob_temp = na.omit(hob_temp)
hob_temp = hob_temp[2:6]

# Hobart Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
hob_rain = read.csv("hobart-rainfall.csv")
hob_rain = na.omit(hob_rain)
hob_rain = hob_rain[3:6]

# Combine Hobart Temperature and Rainfall together
hob = merge(hob_temp, hob_rain, by = c("Year", "Month", "Day"))
hob = hob[c(4,1:3,5,6)]
hob$isFire = FALSE

################## Melbourne #######################

# Melbourne Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
mel_temp = read.csv("melbourne-max-tmp.csv")
mel_temp = na.omit(mel_temp)
mel_temp = mel_temp[2:6]

# Melbourne Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
mel_rain = read.csv("melbourne-rainfall.csv")
mel_rain = na.omit(mel_rain)
mel_rain = mel_rain[3:6]

# Combine Melbourne Temperature and Rainfall together
mel = merge(mel_temp, mel_rain, by = c("Year", "Month", "Day"))
mel = mel[c(4,1:3,5,6)]
mel$isFire = FALSE

################## Perth #######################

# Perth Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
per_temp = read.csv("perth-max-tmp.csv")
per_temp = na.omit(per_temp)
per_temp = per_temp[2:6]

# Perth Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
per_rain = read.csv("perth-rainfall.csv")
per_rain = na.omit(per_rain)
per_rain = per_rain[3:6]

# Combine Perth Temperature and Rainfall together
per = merge(per_temp, per_rain, by = c("Year", "Month", "Day"))
per = per[c(4,1:3,5,6)]
per$isFire = FALSE

################### Sydney ########################

# Sydney Temperature
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Temperature")
syd_temp = read.csv("sydney-max-tmp.csv")
syd_temp = na.omit(syd_temp)
syd_temp = syd_temp[2:6]

# Sydney Rainfall
setwd("/Users/zheningwang/Desktop/FIT3163_FIT3164/DS Project/Datasets/Rainfall")
syd_rain = read.csv("sydney-rainfall.csv")
syd_rain = na.omit(syd_rain)
syd_rain = syd_rain[3:6]

# Combine Sydney Temperature and Rainfall together
syd = merge(syd_temp, syd_rain, by = c("Year", "Month", "Day"))
syd = syd[c(4,1:3,5,6)]
syd$isFire = FALSE












