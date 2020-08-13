#####################################
rm(list = ls())
### read past bushfire dataset ###
setwd("~/Downloads/Datasets/Past Bushfires")
bushfire=read.csv("bushfires1.csv")

### read temperature datasets ###
setwd("~/Downloads/Datasets/Temperature")
ade_temp=read.csv("adelaide-max-tmp.csv")
ade_temp = ade_temp[2:6]
ade_temp = na.omit(ade_temp)


bri_temp=read.csv("brisbane-max-tmp.csv")
bri_temp = bri_temp[2:6]
bri_temp = na.omit(bri_temp)


can_temp=read.csv("canberra-max-tmp.csv")
can_temp = can_temp[2:6]
can_temp = na.omit(can_temp)


dar_temp=read.csv("darwin-max-tmp.csv")
dar_temp = dar_temp[2:6]
dar_temp = na.omit(dar_temp)


hob_temp=read.csv("hobart-max-tmp.csv")
hob_temp = hob_temp[2:6]
hob_temp = na.omit(hob_temp)


mel_temp=read.csv("melbourne-max-tmp.csv")
mel_temp = mel_temp[2:6]
mel_temp = na.omit(mel_temp)


per_temp=read.csv("perth-max-tmp.csv")
per_temp = per_temp[2:6]
per_temp = na.omit(per_temp)


syd_temp=read.csv("sydney-max-tmp.csv")
syd_temp = syd_temp[2:6]
syd_temp = na.omit(syd_temp)


### read rainfall datasets ###
setwd("~/Downloads/Datasets/Rainfall")
ade_rain=read.csv("adelaide-rainfall.csv")
ade_rain = ade_rain[3:6]
ade_rain = na.omit(ade_rain)


bri_rain=read.csv("brisbane-rainfall.csv")
bri_rain = bri_rain[3:6]
bri_rain = na.omit(bri_rain)


can_rain=read.csv("canberra-rainfall.csv")
can_rain = can_rain[3:6]
can_rain = na.omit(can_rain)


dar_rain=read.csv("darwin-rainfall.csv")
dar_rain = dar_rain[3:6]
dar_rain = na.omit(dar_rain)


hob_rain=read.csv("hobart-rainfall.csv")
hob_rain = hob_rain[3:6]
hob_rain = na.omit(hob_rain)


mel_rain=read.csv("melbourne-rainfall.csv")
mel_rain = mel_rain[3:6]
mel_rain = na.omit(mel_rain)


per_rain=read.csv("perth-rainfall.csv")
per_rain = per_rain[3:6]
per_rain = na.omit(per_rain)


syd_rain=read.csv("sydney-rainfall.csv")
syd_rain = syd_rain[3:6]
syd_rain = na.omit(syd_rain)


### combine rainfall and temperature data together for each city ###
ade = merge(ade_temp, ade_rain, by = c("Year", "Month", "Day"))
ade = ade[c(4,1:3,5,6)]
ade$FireStart=FALSE

bri = merge(bri_temp, bri_rain, by = c("Year", "Month", "Day"))
bri = bri[c(4,1:3,5,6)]
bri$FireStart=FALSE

can = merge(can_temp, can_rain, by = c("Year", "Month", "Day"))
can = can[c(4,1:3,5,6)]
can$FireStart=FALSE

dar = merge(dar_temp, dar_rain, by = c("Year", "Month", "Day"))
dar = dar[c(4,1:3,5,6)]
dar$FireStart=FALSE

hob = merge(hob_temp, hob_rain, by = c("Year", "Month", "Day"))
hob = hob[c(4,1:3,5,6)]
hob$FireStart=FALSE

mel = merge(mel_temp, mel_rain, by = c("Year", "Month", "Day"))
mel = mel[c(4,1:3,5,6)]
mel$FireStart=FALSE

per = merge(per_temp, per_rain, by = c("Year", "Month", "Day"))
per = per[c(4,1:3,5,6)]
per$FireStart=FALSE

syd = merge(syd_temp, syd_rain, by = c("Year", "Month", "Day"))
syd = syd[c(4,1:3,5,6)]
syd$FireStart=FALSE

### change Date format ###
ade_Date=paste(ade$Day,ade$Month,ade$Year,sep="/")
ade<-cbind(ade,ade_Date)
ade$Year<-NULL
ade$Month<-NULL
ade$Day<-NULL
colnames(ade)[5]<-"Date"

bri_Date=paste(bri$Day,bri$Month,bri$Year,sep="/")
bri<-cbind(bri,bri_Date)
bri$Year<-NULL
bri$Month<-NULL
bri$Day<-NULL
colnames(bri)[5]<-"Date"

can_Date=paste(can$Day,can$Month,can$Year,sep="/")
can<-cbind(can,can_Date)
can$Year<-NULL
can$Month<-NULL
can$Day<-NULL
colnames(can)[5]<-"Date"

dar_Date=paste(dar$Day,dar$Month,dar$Year,sep="/")
dar<-cbind(dar,dar_Date)
dar$Year<-NULL
dar$Month<-NULL
dar$Day<-NULL
colnames(dar)[5]<-"Date"

hob_Date=paste(hob$Day,hob$Month,hob$Year,sep="/")
hob<-cbind(hob,hob_Date)
hob$Year<-NULL
hob$Month<-NULL
hob$Day<-NULL
colnames(hob)[5]<-"Date"

mel_Date=paste(mel$Day,mel$Month,mel$Year,sep="/")
mel<-cbind(mel,mel_Date)
mel$Year<-NULL
mel$Month<-NULL
mel$Day<-NULL
colnames(mel)[5]<-"Date"

per_Date=paste(per$Day,per$Month,per$Year,sep="/")
per<-cbind(per,per_Date)
per$Year<-NULL
per$Month<-NULL
per$Day<-NULL
colnames(per)[5]<-"Date"

syd_Date=paste(syd$Day,syd$Month,syd$Year,sep="/")
syd<-cbind(syd,syd_Date)
syd$Year<-NULL
syd$Month<-NULL
syd$Day<-NULL
colnames(syd)[5]<-"Date"

### records for each location at the day that bushfire occured and two days before bushfire occured ###
### South Aus = Adelaide ### Queensland = Brisbane ### Australian Capital Territory = Canberra ###
### Northern Territory = Darwin ###Tasmania = Hobart ### Victoria = Melbourne ###ACT = Canberra### NSW = Sydney
SA_bf=bushfire[(bushfire$Location=="South Australia"),]
ade$FireStart[(as.Date(ade$Date,"%d/%m/%Y") %in% as.Date(SA_bf$Date,"%d/%m/%Y"))] <- TRUE

QLD_bf=bushfire[(bushfire$Location=="Queensland"),]
bri$FireStart[(as.Date(bri$Date,"%d/%m/%Y") %in% as.Date(QLD_bf$Date,"%d/%m/%Y"))] <- TRUE

ACT_bf=bushfire[(bushfire$Location=="Canberra"),]
can$FireStart[(as.Date(can$Date,"%d/%m/%Y") %in% as.Date(ACT_bf$Date,"%d/%m/%Y"))] <- TRUE

NT_bf=bushfire[(bushfire$Location=="Northern Territory"),]
dar$FireStart[(as.Date(dar$Date,"%d/%m/%Y") %in% as.Date(NT_bf$Date,"%d/%m/%Y"))] <- TRUE

Tas_bf=bushfire[(bushfire$Location=="Tasmania"),]
hob$FireStart[(as.Date(hob$Date,"%d/%m/%Y") %in% as.Date(Tas_bf$Date,"%d/%m/%Y"))] <- TRUE

Vic_bf=bushfire[(bushfire$Location=="Victoria"),]
mel$FireStart[(as.Date(mel$Date,"%d/%m/%Y") %in% as.Date(Vic_bf$Date,"%d/%m/%Y"))] <- TRUE

ACT_bf=bushfire[(bushfire$Location=="Australian Capital Territory"),]
can$FireStart[(as.Date(can$Date,"%d/%m/%Y") %in% as.Date(ACT_bf$Date,"%d/%m/%Y"))] <- TRUE

NSW_bf=bushfire[(bushfire$Location=="New South Wales"),]
syd$FireStart[(as.Date(syd$Date,"%d/%m/%Y") %in% as.Date(NSW_bf$Date,"%d/%m/%Y"))] <- TRUE