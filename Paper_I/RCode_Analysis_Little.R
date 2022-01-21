rm(list = ls())

library(data.table)
library(zoo)
library(caTools)
library(ggplot2)
library(ggpubr)
library(Rmixmod)
library(tidyverse)
library(lubridate)
library(viridis)

# d = as.data.table(list(1:6/2, 3:8/4))
# rollmean(d$V1, k = 5, fill = NA)
# runsd(d$V1, k = 5) #remove obs that are NA in rollmean

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

thr_depth<-1 #consider diving at 1 meter

##############################################################################################################
##############################################################################################################
##############################################################################################################

#### read file
# accData<-fread("/Volumes/Mari_MC1/LPPI2020/G3047FP16/G3047FP16_S1.csv",header=TRUE)
# head(accData)
# tail(accData)
# dim(accData)
# str(accData)
# 
# #remove row 1, odd ?
# accData<-accData[-1,]
# 
# #remove lat one too ?
# accData<-accData[-nrow(accData),]
# 
# accData[1:51,c("Timestamp","X","Y","Z")]
# accData[1:101,c("Timestamp","X","Y","Z")]
# 
# #### resample at 25 Hz
# rownames(accData)<-seq(from = 1, to = nrow(accData), by = 1)
# myFreq <- 4
# idx <- seq(from = 1, to = nrow(accData), by = myFreq)
# accData <- accData[idx,]
# accData[1:51,c("Timestamp","X","Y","Z")]
# 
# 
# write.csv(accData,"/Volumes/Mari_MC1/LPPI2020/G3047FP16/G3047FP16_S125Hz.csv", row.names = TRUE)

##############################################################################################################
##############################################################################################################
##############################################################################################################


# mac path: /Users/mariannachimienti/Dropbox/MarieCurie/DataLittle/PG3098FP12_S1Cut.csv
#path /Volumes/Mari_MC1/LPPI2020
#### read file 
accData<-fread("/Volumes/Mari_MC1/LP_2019_jazel/AXY/3. POSTGUARD/ACCELERATION/PG4016FP12.csv",header=TRUE)
head(accData)
tail(accData)
dim(accData)
str(accData)

#remove row 1, odd ?
#accData<-accData[-1,]
#remove lat one too ?
#accData<-accData[-nrow(accData),]
accData[1:51,c("Timestamp","X","Y","Z","Pressure")]
#calculate variables

hist(accData$Pressure,breaks=50) # - look at histogram and take mode
result <- getmode(accData$Pressure[which(!is.na(accData$Pressure))])
accData$depth<-(accData$Pressure-result)/100 
summary(accData$depth)
#summary(accData$depth[1:1500000])

######### ######### ######### ######### ######### ######### ######### ######### 
######### for LP_2019 look at sampling timing and cut the beginning
depth<-accData[-which(is.na(accData$depth)),] #### check when pressure starts at 1Hz
head(depth)
options(digits.secs=9)   
accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="UTC")
accData<-accData[which(accData$DatesPos>=as.POSIXct("2019-10-17 17:02:36.00", format='%Y-%m-%d %H:%M:%OS',tz="UTC")),]
accData[1:51,c("Timestamp","X","Y","Z","Pressure")]
######### ######### ######### ######### ######### ######### ######### ######### 
######### ######### ######### ######### ######### ######### ######### ######### 

depth<-accData$depth[-which(is.na(accData$depth))]
depth1<-c(depth[2:length(depth)],depth[length(depth)])
changeDepth<-depth1-depth

depth25Hz<-rep(depth,each=25)
changeDepth25Hz<-rep(changeDepth,each=25)
tail(accData)
accData$depth25Hz<-depth25Hz[1:nrow(accData)]


accData$changeDepth25Hz<-changeDepth25Hz[1:nrow(accData)]
hist(accData$depth25Hz,breaks=50)
head(accData)
tail(accData)
#accData[1:55,]

options(digits.secs=9)   
accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="UTC")

#it is beneficial to use an odd number of points so that the calculation is symmetric
accData$static_BF<-rollmean(accData$X, k = 51, fill = NA) #running mean over 2 seconds, sampling 25Hz
accData$static_LA<-rollmean(accData$Y, k = 51, fill = NA)
accData$static_DV<-rollmean(accData$Z, k = 51, fill = NA)

summary(accData$static_BF)
summary(accData$static_LA)
summary(accData$static_DV)


accData$SD_BF2<-runsd(accData$X, k = 51) #SD over running mean over 2 seconds, sampling 25Hz
accData$SD_LA2<-runsd(accData$Y, k = 51) 
accData$SD_DV2<-runsd(accData$Z, k = 51) 

accData$SD_BF10<-runsd(accData$X, k = 251) #SD over running mean over 10 seconds, sampling 25Hz
accData$SD_LA10<-runsd(accData$Y, k = 251) 
accData$SD_DV10<-runsd(accData$Z, k = 251) 

accData$SD_BF30<-runsd(accData$X, k = 751) #SD over running mean over 30 seconds, sampling 25Hz
accData$SD_LA30<-runsd(accData$Y, k = 751) 
accData$SD_DV30<-runsd(accData$Z, k = 751) 

accData$SD_BF60<-runsd(accData$X, k = 1501) #SD over running mean over 60 seconds, sampling 25Hz
accData$SD_LA60<-runsd(accData$Y, k = 1501) 
accData$SD_DV60<-runsd(accData$Z, k = 1501) 

accData<-accData[-which(is.na(accData$static_BF)),]#remove Nas
head(accData)
tail(accData)

# calculation of body Pitch 
accData$Pitch <- atan2((accData$static_BF), sqrt(accData$static_LA*accData$static_LA + accData$static_DV*accData$static_DV)) * 180/pi
summary(accData$Pitch)

#Calculation of Dynamic acceleration and ODBA 
accData$Dynamic_DorsoVentral <- accData$Z-accData$static_DV 
accData$Dynamic_Lateral <- accData$Y-accData$static_LA 
accData$Dynamic_BackForward <- accData$X-accData$static_BF 

accData$ODBA<-abs(accData$Dynamic_BackForward)+abs(accData$Dynamic_Lateral)+abs(accData$Dynamic_DorsoVentral)
# Z1<-c(accData$Z[2:length(accData$Z)],accData$Z[length(length(accData$Z))])
# X1<-c(accData$X[2:length(accData$X)],accData$X[length(length(accData$X))])
# 
# accData$jerkZ<-abs(accData$Z-Z1)/0.04
# accData$jerkX<-abs(accData$X-X1)/0.04
#accData$depth25Hz<-accData$depth25Hz*-1

###############################################################
##### cut when animal is at sea
###############################################################
#plot GPS points
xy<-accData[,c("DatesPos","location-lat","location-lon")]
colnames(xy)<-c("DateTime","Lat","Lon")
xy<-xy[which(!is.na(xy$Lat)),]
xy$Date<-as.Date(format(xy$DateTime, "%Y-%m-%d"))

plotGPS<-ggplot(xy) +
  geom_path(aes(x=Lon, y=Lat), colour="black",size=1)+
  geom_point(aes(x=Lon, y=Lat,colour=as.factor(Date)), size=2)

plotGPS

head(accData)
dim(accData)
summary(accData)
#start animal when in water, check with Pitch, check the end
ggplot(accData[50000:100000,]) +geom_line(aes(x=DatesPos, y=Pitch), size=1)#beginning
ggplot(accData[1:500000,]) +geom_line(aes(x=DatesPos, y=depth25Hz), size=1)#beginning

ggplot(accData[1500000:1600000,]) +geom_line(aes(x=DatesPos, y=Pitch), size=1)#middle
ggplot(accData[1400000:1500000,]) +geom_line(aes(x=DatesPos, y=depth25Hz), size=1)#middle
# 
# 
# ggplot(accData[(nrow(accData)-1000000):nrow(accData),]) +geom_line(aes(x=DatesPos, y=Pitch), size=1)#end
# 
# accData_an<-accData[115000:nrow(accData),]
accData_an<-accData[which(accData$DatesPos>=as.POSIXct("2019-10-17 17:50:00.00", format='%Y-%m-%d %H:%M:%OS',tz="UTC")),]
accData_an<-accData_an[which(accData_an$DatesPos<as.POSIXct("2019-10-18 09:45:00.00", format='%Y-%m-%d %H:%M:%OS',tz="UTC")),]

dim(accData_an)
head(accData_an)
tail(accData_an)

# # #ggplot(accData_an[100000:150000,]) +geom_line(aes(x=DatesPos, y=Pitch), size=1)
plotdepth<-ggplot(accData_an) +
  geom_line(aes( x=DatesPos, y=depth25Hz), size=0.7)

plotPitch<-ggplot(accData_an) +
  geom_line(aes( x=DatesPos, y=Pitch), size=0.7)+ geom_hline(yintercept = 0)

dev.new()
ggarrange(plotdepth,plotPitch,ncol=1)

###############################################################
##### check data at surface - pitch around 0 and depth 0
###############################################################
#hist(accData_an$depth25Hz,breaks=50)

summary(accData_an$depth25Hz)
accData_an$depth25Hz<-accData_an$depth25Hz-min(summary(accData_an$depth25Hz))

#hist(accData_an$depth25Hz,breaks=50)

summary(accData_an$depth25Hz)

###############################################################
#####  calculate extra variables
###############################################################

#### re-calculate change in depth from adj depth
depth <- accData_an$depth25Hz
myFreq <- 25
idx <- seq(from = 1, to = nrow(accData_an), by = myFreq)
depth <- depth[idx]
depth1 <- c(depth[2:length(depth)], depth[length(depth)])
changeDepth <- depth1 - depth
changeDepth25Hz<-rep(changeDepth,each=25)
accData_an$changeDepth25Hz<-changeDepth25Hz[1:nrow(accData_an)]
summary(changeDepth25Hz)

# calculation of Roll
accData_an$Roll <-
  atan2((accData_an$static_LA),
        sqrt(
          accData_an$static_BF * accData_an$static_BF + accData_an$static_DV * accData_an$static_DV
        )
  ) * 180 / pi
accData_an$SD_Roll <-
  runsd(accData_an$Roll, k = 751) #SD over Roll

accData_an$SD_Pitch <-
  runsd(accData_an$Pitch, k = 1501) #SD over Pitch

#VeDBA
accData_an$VeDBA <- sqrt((accData_an$Dynamic_DorsoVentral^2)+(accData_an$Dynamic_Lateral^2)+(accData_an$Dynamic_BackForward^2))

#SD SD_VeDBA
accData_an$SD_VeDBA <- runsd(accData_an$VeDBA, k = 1501)


# Sur - Not Sur variable
#hist(accData$depth25Hz,breaks=50)
Sur_NotSur <- ifelse(accData_an$depth25Hz <= thr_depth, 0, 1) ##### look at this
accData_an$Sur_NotSur <- as.factor(Sur_NotSur)


#Fix colnames
accData_an<-accData_an[,-c("V1")]
names(accData_an) <- gsub(x = names(accData_an), pattern = "\\.", replacement = "_")  
names(accData_an) <- gsub(x = names(accData_an), pattern = "\\-", replacement = "_") 
names(accData_an) <- gsub(x = names(accData_an), pattern = "\\ ", replacement = "_") 
names(accData_an) <- gsub(x = names(accData_an), pattern = "\\(", replacement = "_") 
names(accData_an) <- gsub(x = names(accData_an), pattern = "\\)", replacement = "_") 
names(accData_an) <- gsub(x = names(accData_an), pattern = "\\?", replacement = "_") 

###############################################################
##### write new file
###############################################################

accData_an<-within(accData_an,rm("DatesPos")) # remove datesPos might create problems when loading
write.csv(accData_an,"/Volumes/Mari_MC1/LP_2019_jazel/LittlePenguin_2019_Trips/PG4016FP12_Trip1.csv", row.names = TRUE)

#####################################################
