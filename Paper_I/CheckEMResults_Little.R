
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
library(plyr)
library(smoother)
library("ggsci")


################### ################### ###################
################### ################### ###################
##################### parameters
################### ################### ###################
################### ################### ###################

thr_depth<-1 #threshold for surface not surface variable

# Create the function for mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

################### ################### ###################
################### ################### ###################
##################### read files
################### ################### ###################
################### ################### ###################


filenamesTrips <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/LittlePenguin_2019_Trips/",
    pattern = "*.csv",
    full.names = TRUE
  )


filenamesEM <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/EM_Results_Little_2019/",
    pattern = "*.csv",
    full.names = TRUE
  )


#### read file
#accData<-fread("/Users/mariannachimienti/Dropbox/MarieCurie/DataLittle/G3002MP19_S1Cut.csv",header=TRUE)
TripID<-40
filenamesTrips[TripID]
accData <- fread(filenamesTrips[TripID], header = TRUE)

accData<-accData[,-c("V1")]
names(accData) <- gsub(x = names(accData), pattern = "\\.", replacement = "_")  
names(accData) <- gsub(x = names(accData), pattern = "\\-", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\ ", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\(", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\)", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\?", replacement = "_") 

options(digits.secs=9)   
accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="UTC")

# #### calculate depth
# result <- getmode(accData$Pressure[which(!is.na(accData$Pressure))])
# accData$depth<-(accData$Pressure-result)/100
# depth<-accData$depth[-which(is.na(accData$depth))]
# depth25Hz<-rep(depth,each=25)
# accData$depth25Hz<-depth25Hz[1:nrow(accData)]
# accData$depth25Hz<-accData$depth25Hz-min(summary(accData$depth25Hz))
# summary(accData$depth25Hz)
# 
# #### calculate change in depth
# depth <- accData$depth25Hz
# myFreq <- 25
# idx <- seq(from = 1, to = nrow(accData), by = myFreq)
# depth <- depth[idx]
# depth1 <- c(depth[2:length(depth)], depth[length(depth)])
# changeDepth <- depth1 - depth
# changeDepth25Hz<-rep(changeDepth,each=25)
# accData$changeDepth25Hz<-changeDepth25Hz[1:nrow(accData)]
# 
# 
# # calculation of Roll
# accData$Roll <-
#   atan2((accData$static_LA),
#         sqrt(
#           accData$static_BF * accData$static_BF + accData$static_DV * accData$static_DV
#         )
#   ) * 180 / pi
# 
# accData$SD_Roll <-
#   runsd(accData$Roll, k = 751) #SD over running mean over 30 sec, sampling 25Hz, if even number k, it will be centered
# 
# 
# accData$SD_Pitch <-
#   runsd(accData$Pitch, k = 1501) #SD over running mean over 1min, sampling 25Hz
# 
# #VeDBA
# accData$VeDBA <- sqrt((accData$Dynamic_DorsoVentral^2)+(accData$Dynamic_Lateral^2)+(accData$Dynamic_BackForward^2))
# 
# #SD SD_VeDBA
# accData$SD_VeDBA <- runsd(accData$VeDBA, k = 1501)


# Sur - Not Sur variable
#hist(accData$depth25Hz,breaks=50)
Sur_NotSur <- ifelse(accData$depth25Hz <= thr_depth, 0, 1) ##### look at this
accData$Sur_NotSur <- as.factor(Sur_NotSur)

#### #### #### #### #### #### #### #### #### #### 
# add EM results  
#### #### #### #### #### #### #### #### #### #### 

EM_ID<-TripID*3
EM_Partition<- fread(filenamesEM[EM_ID], header = TRUE)
head(EM_Partition)
head(accData)
################### ################### ###################
################### ################### ###################
################### assign states
################### ################### ###################
################### ################### ###################


plotdepth<-ggplot(accData) +
  geom_line(aes( x=DatesPos, y=depth25Hz), size=0.7)

plotPitch<-ggplot(accData) +
  geom_line(aes( x=DatesPos, y=Pitch), size=0.7)+ geom_hline(yintercept = 0)

ggarrange(plotdepth,plotPitch,ncol=1)



accData$States<-EM_Partition$StatesComb_9
accData$States1<-EM_Partition$StatesComb_10
accData$States2<-EM_Partition$StatesComb_11
accData$States3<-EM_Partition$StatesComb_17

sort(unique(accData$States))
sort(unique(accData$States2))
################### ################### ###################
################### ################### ###################
############ divide dataset and check results
################### ################### ###################
################### ################### ###################

subDataDive <- accData[which(accData$Sur_NotSur == 1), ]
subDataUW <- accData[which(accData$Sur_NotSur == 0), ]

dim(subDataUW)
dim(subDataDive)

################### ################### ###################
################### ################### ###################
##################### plots Dives
################### ################### ###################
################### ################### ###################
BIC_Dive<- fread(filenamesEM[1], header = TRUE)
BIC_Dive<-BIC_Dive[,-c("V1")]

plot(BIC_Dive~nStatesDive,data=BIC_Dive,type="b",pch=19)

unique(subDataDive$States)
head(subDataDive$States)
unique(subDataDive$States1)
head(subDataDive$States1)
#subDataDive$States_lev<-factor(subDataDive$States3, levels = c("1_Dive","2_Dive","3_Dive"))
subDataDive$States_lev<-factor(subDataDive$States, levels = c("1_Dive","2_Dive","3_Dive","4_Dive"))
#subDataDive$States_lev<-factor(subDataDive$States3, levels = c("1_Dive","2_Dive","3_Dive","4_Dive","5_Dive"))

# #
start<-100000
end<-300000

plot1<-ggplot(subDataDive[start:end,]) +
  geom_line(aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=(depth25Hz*-1),colour=States_lev), size=2)+
  scale_colour_jco()+xlab("")+ylab("Depth (m)")
#plot1

plot3<-ggplot(subDataDive[start:end,]) +
  geom_line(aes( x=DatesPos, y=VeDBA),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=VeDBA,colour=States_lev), size=2)+
  scale_colour_jco()+xlab("")+ylab("VeDBA")

plot4<-ggplot(subDataDive[start:end,]) +
  geom_line(aes( x=DatesPos, y=SD_DV2),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=SD_DV2,colour=States_lev), size=2)+
  scale_colour_jco()+xlab("")+ylab("SD_DV2")

plot5<-ggplot(subDataDive[start:end,]) +
  geom_line(aes( x=DatesPos, y=Pitch),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=Pitch,colour=States_lev), size=2)+
  scale_colour_jco()+xlab("")+ylab("Pitch")
dev.new()
ggarrange(plot1,plot3,plot5, ncol=1,common.legend = TRUE)
# 
################### ################### ###################

box1<-ggplot(subDataDive) +
  geom_boxplot(aes( x=States_lev, y=(depth25Hz*-1),fill=States_lev),colour="gray25")+
  scale_fill_jco()+xlab("")+ylab("Depth (m)")

box2<-ggplot(subDataDive) +
  geom_boxplot(aes( x=States_lev, y=changeDepth25Hz,fill=States_lev),colour="gray25")+
  scale_fill_jco()+xlab("")

box3<-ggplot(subDataDive) +
  geom_boxplot(aes( x=States_lev, y=SD_DV2,fill=States_lev),colour="gray25")+
  scale_fill_jco()+xlab("")

box4<-ggplot(subDataDive) +
  geom_boxplot(aes( x=States_lev, y=Pitch,fill=States_lev),colour="gray25")+
  scale_fill_jco() +xlab("")

box5<-ggplot(subDataDive) +
  geom_boxplot(aes( x=States_lev, y=VeDBA,fill=States_lev),colour="gray25")+
  scale_fill_jco() +xlab("")

# dev.new()
# ggarrange(box1,box2,box3,box4,box5,box6,ncol=2,nrow=3,common.legend = TRUE)

dev.new()
ggarrange(box1,box2,box3,box4,box5,ncol=3,nrow=2,common.legend = TRUE)


################### ################### ###################
################### create catching DF
################### ################### ###################

# 
# ####### divide Dives in events and extrat time spent in different states.
# subDataDive$gap <- c(0, diff(subDataDive$DatesPos,units = "secs") > 0.0500)
# subDataDive$group <- cumsum(subDataDive$gap) + 1
# #plot(subDataDive$depth25Hz[which(subDataDive$group==200)], type="b")
# 
# CatchingDF<-data.frame(NA,NA,NA,NA,NA,NA)# empty dataframe for storing results
# rowCount<-1
# 
# for (DiveEv in 1:length(unique(subDataDive$group))) {
#   ###### divide each Dives in catching events and extrat time spent in different catching events
#   dive_ev <- subDataDive[which(subDataDive$group == DiveEv &
#                                  subDataDive$States_lev == "2_Dive"), ]
#   
#   dive_ev$gapCatch <- c(0, diff(dive_ev$DatesPos, units = "secs") > 0.0500)
#   dive_ev$groupCatch <- cumsum(dive_ev$gapCatch) + 1
#   #unique(dive_ev$groupCatch)
#   
#   if (nrow(dive_ev) > 2) {
#     for (catchEv in 1:length(unique(dive_ev$groupCatch))) {
#       dive_evSub <- dive_ev[which(dive_ev$groupCatch == catchEv), ]
#       
#       if(nrow(dive_evSub)>1){ #### see if you want to discard or not just 1 data point
#         CatchingDF[rowCount, 1] <- dive_evSub$Timestamp[1] #start
#         CatchingDF[rowCount, 2] <-
#           dive_evSub$Timestamp[nrow(dive_evSub)] #end
#         
#         duration <-
#           difftime(dive_evSub$DatesPos[nrow(dive_evSub)], dive_evSub$DatesPos[1], units = "secs")
#         CatchingDF[rowCount, 3] <- as.numeric(duration)#duration
#         
#         CatchingDF[rowCount, 4] <- mean(dive_evSub$depth25Hz)#depth
#         CatchingDF[rowCount, 5] <- mean(dive_evSub$VeDBA)#VeDBA
#         CatchingDF[rowCount, 6] <- unique(dive_ev$group)
#         
#         rowCount <- rowCount + 1
#       }
#     }
#   }
# }
# 
# 
# colnames(CatchingDF)<- c("DateTime_start","DateTime_end","duration_sec","depth_m","VeDBA","diveID")
# head(CatchingDF)
# options(digits.secs=9)  
# CatchingDF$Animal_ID<-as.factor(unique(accData$TagID))
# CatchingDF$DatesPos<-as.POSIXct(CatchingDF$DateTime_start, format='%d/%m/%Y %H:%M:%OS',tz="UTC")
# 
# # CatchingDF$diveID<-as.factor(CatchingDF$diveID)
# # 
# #CatchingDF<-CatchingDF[which(CatchingDF$depth_m>=2),]
# # 
# # hist(CatchingDFLong$VeDBA)
# # 
# # ggplot(CatchingDFLong, aes(x=diveID, y=VeDBA)) + 
# #   geom_boxplot(fill="slateblue", alpha=0.2) + 
# #   xlab("")
# # 
# # plot(CatchingDFLong$duration_sec)
# 
# 
# library(hrbrthemes)
# library(viridis)
# 
# # ViolingP <- ggplot(CatchingDF, aes(x=Animal_ID, y=depth_m)) + 
# #   geom_violin(fill="dodgerblue2")+
# #   geom_boxplot(width=0.1, color="grey", alpha=0.2) +
# # #  scale_fill_viridis(discrete = TRUE) +
# #   theme_bw() +
# #   theme(
# #     legend.position="none",
# #     plot.title = element_text(size=20)
# #   ) +ylab("depth (m)")+xlab("")
# # 
# # ViolingP
# # 
# 
# 
# 
# summary(CatchingDF$depth_m)
# plotCatching<-ggplot() +
#   geom_line(data=accData,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
#   geom_point(data=CatchingDF,aes( x=DatesPos, y=(depth_m*-1),colour=depth_m), size=2)+ylab("depth (m)")+xlab("")+
#   scale_colour_viridis_c(name="depth (m)")
# 
# #plotCatching
# 
# summary(CatchingDF$duration_sec)
# MinVal<-round(min(CatchingDF$duration_sec))
# MaxVal<-round(max(CatchingDF$duration_sec))
# 
# library("scales")
# plotCatchingDur<-ggplot() +
#   geom_line(data=accData,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
#   geom_point(data=CatchingDF,aes( x=DatesPos, y=(depth_m*-1),colour=duration_sec), size=2)+ylab("depth (m)")+xlab("")+
#   scale_colour_gradientn(colours = c("papayawhip","peachpuff3","peachpuff4","salmon3","salmon4"), 
#                          values = rescale(c(MinVal,2.5,5,MaxVal)),
#                          guide = "colorbar", limits=c(0,MaxVal), name= "duration (sec)")
# 
# 
# #plotCatchingDur
# 
# 
# plotCatchingVedBA<-ggplot() +
#   geom_line(data=accData,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=0.5)+
#   geom_point(data=CatchingDF,aes( x=DatesPos, y=(depth_m*-1),colour=VeDBA), size=2)+ylab("depth (m)")+xlab("")+
#   scale_colour_viridis_c(option = "plasma", name= "VeDBA (g)")
# 
# #plotCatchingVedBA
# 
# dev.new()
# ggarrange(plotCatching,plotCatchingDur,plotCatchingVedBA,ncol=1,nrow=3,common.legend = FALSE)
# 
# 

# plotCatchDur<-ggplot() +
#   geom_point(data=CatchingDF,aes( x=duration_sec, y=VeDBA),colour="black", size=2)+ylab("VeDBA (g)")+xlab("depth (m)")
# dev.new()
# plotCatchDur

################### ################### ###################
################### ################### ###################
##################### plots UW swim
################### ################### ###################
################### ################### ###################

unique(subDataUW$States)
subDataUW$States_lev<-factor(subDataUW$States, levels = c("1_UW","2_UW","3_UW"))

unique(subDataUW$States1)
subDataUW$States_lev<-factor(subDataUW$States1, levels = c("1_UW","2_UW","3_UW","4_UW"))
# 
# unique(subDataUW$States2)
# subDataUW$States_lev<-factor(subDataUW$States2, levels = c("1_UW","2_UW","3_UW","4_UW","5_UW"))

myColors <- pal_jama()(5)
names(myColors) <- levels(unique(subDataUW$States_lev))
custom_colors <- scale_fill_manual(name = "behavioural states below water surface", values = myColors)#,
# labels=c("1_UWNotLand"="Rest", "2_UWNotLand"="Preen/high flap", "3_UWNotLand"="Swim"))

custom_colors2 <- scale_colour_manual(name = "behavioural states below water surface", values = myColors)#,
# labels=c("1_UWNotLand"="Rest", "2_UWNotLand"="Preen/high flap", "3_UWNotLand"="Swim"))


box2<-ggplot(subDataUW) +
  geom_boxplot(aes( x=States_lev, y=Pitch,fill=States_lev),colour="gray25")+
  custom_colors+#scale_x_discrete(labels=c("1_UWNotLand"="Rest", "2_UWNotLand"="Preen/high flap", "3_UWNotLand"="Swim"))+
  xlab("")+ylab("Pitch (degrees)")+theme(text = element_text(size=15))

box3<-ggplot(subDataUW) +
  geom_boxplot(aes( x=States_lev, y=VeDBA,fill=States_lev),colour="gray25")+
  custom_colors+#scale_x_discrete(labels=c("1_UWNotLand"="Rest", "2_UWNotLand"="Preen/high flap", "3_UWNotLand"="Swim"))+
  xlab("")+ylab("VeDBA (g)")+theme(text = element_text(size=15))

box4<-ggplot(subDataUW) +
  geom_boxplot(aes( x=States_lev, y=SD_Roll,fill=States_lev),colour="gray25") +
  custom_colors+#scale_x_discrete(labels=c("1_UWNotLand"="Rest", "2_UWNotLand"="Preen/high flap", "3_UWNotLand"="Swim"))+
  xlab("")+ylab("SD Roll (degrees)")+theme(text = element_text(size=15))


dev.new()
ggarrange(box2,box3,box4,ncol=3,nrow=1,common.legend = TRUE)


dim(subDataUW)
start<-1
end<-nrow(subDataUW)

plot1<-ggplot(subDataUW[start:end,]) +
  geom_line(aes( x=DatesPos, y=Pitch),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=Pitch,colour=States_lev), size=2)+
  custom_colors2+xlab("")+ylab("Pitch (degrees)")
#plot1

plot2<-ggplot(subDataUW[start:end,]) +
  geom_line(aes( x=DatesPos, y=VeDBA),colour="gray50", size=0.5)+
  geom_point(aes( x=DatesPos, y=VeDBA,colour=States_lev), size=2)+
  custom_colors2+xlab("")+ylab("VeDBA")

dev.new()
ggarrange(plot1,plot2,ncol=1,common.legend = TRUE)


#################################################################################
####################################### label states and write file
#################################################################################

head(accData)
accData<-accData[,-c("States1","States2","States3","DatesPos")] #choose col States

StatesNames<-rep(NA,nrow(accData))

unique(accData$States)
StatesID<-accData$States
StatesNames<-ifelse(StatesID=="3_Dive",StatesNames<-"Descending",
                    ifelse(StatesID=="2_Dive",StatesNames<-"Hunting",
                           ifelse(StatesID=="1_Dive",StatesNames<-"Ascending",
                                  ifelse(StatesID=="4_Dive",StatesNames<-"Swimming",
                           #              ifelse(StatesID=="3_Dive",StatesNames<-"Swimming_2",
                                                    
                                       #  ifelse(StatesID=="3_UW",StatesNames<-"Still1",    
                                          #          ifelse(StatesID=="4_UW",StatesNames<-"Still",
                                                        ifelse(StatesID=="3_UW",StatesNames<-"Rest",
                                                              ifelse(StatesID=="2_UW",StatesNames<-"Swim/Porpoise",
                                                                     "Preen/highFlap_W"))))))#)#)#)

unique(StatesNames)
accData$StatesNames<-StatesNames

accData[1:20,c("States","StatesNames")]
head(accData)


ID_Ind <- basename(filenamesTrips[TripID])
ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
ID_Ind <- paste(ID_Ind, "_EM_Sel", ".csv", sep = "")
pathRes <-
  paste("/Users/mariannachimienti/MarieCurie/Little_2019EM/",
        ID_Ind,
        sep = "")

pathRes
write.csv(accData, pathRes, row.names = TRUE)

#################################################################################
####################################### general big plots 
#################################################################################
library("ggsci")
library("wesanderson")
library(lisa) # https://cran.r-project.org/web/packages/lisa/readme/README.html

filenamesTrips <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/Little_2020EM/",
    pattern = "*.csv",
    full.names = TRUE
  )

#### read file
#accData<-fread("/Users/mariannachimienti/Dropbox/MarieCurie/DataLittle/G3002MP19_S1Cut.csv",header=TRUE)
TripID<-53
filenamesTrips[TripID]
accData <- fread(filenamesTrips[TripID], header = TRUE)

accData<-accData[,-c("V1")]
names(accData) <- gsub(x = names(accData), pattern = "\\.", replacement = "_")  
names(accData) <- gsub(x = names(accData), pattern = "\\-", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\ ", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\(", replacement = "_") 
names(accData) <- gsub(x = names(accData), pattern = "\\)", replacement = "_") 

options(digits.secs=9)   
accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="UTC")
head(accData)



plot1<-ggplot() + theme_bw()+
  geom_line(data=accData,aes( x=DatesPos, y=Pitch),colour="gray50", size=0.5)



unique(accData$States1)
#accData$depth<-accData$depth-min(summary(accData$depth))
# #subDataUW_Land$States_lev<-factor(subDataUW_Land$States, levels = c("1_UWLand","2_UWLand","3_UWLand"))
# subDataUW_Land$States_lev<-factor(subDataUW_Land$States1, levels = c("1_UWLand","2_UWLand","3_UWLand","4_UWLand"))
# subDataUW_NotLand$States_lev<-factor(subDataUW_NotLand$States1, levels = c("1_UWNotLand","2_UWNotLand","3_UWNotLand"))
# subDataDive$States_lev<-factor(subDataDive$States1, levels = c("1_Dive","2_Dive","3_Dive","4_Dive"))

accData$States_lev<-factor(accData$States1, levels = c("1_Dive","2_Dive","3_Dive","4_Dive",
                                                       "1_UW","2_UW","3_UW","4_UW"))
head(accData)
lineS<-0.7
pointS<-6
textS<-20


###################### below surface

subAcc<-accData[80000:100000]
subAccPoint<-subAcc[which(subAcc$States_lev=="1_UW"|subAcc$States_lev=="2_UW"|subAcc$States_lev=="3_UW"|subAcc$States_lev=="4_UW"),]
subAccPoint$States_lev<-as.factor(as.character(subAccPoint$States_lev))

#define custom color scale
myColors <- pal_jama()(4)
names(myColors) <- levels(unique(subAccPoint$States_lev))
custom_colors <- scale_colour_manual(name = "behavioural states below water surface", values = myColors,
                                     labels=c("1_UW"="Preen/Flap on water", "2_UW"="Swim/Porpoise", "3_UW"="Still","4_UW"="Slow surface swim"))

plot0<-ggplot() + theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=SD_Roll),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=SD_Roll,colour=States_lev), size=pointS)+
  custom_colors+xlab("")+ylab("sd Roll (degrees)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))


plot1<-ggplot() + theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=Pitch),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=Pitch,colour=States_lev), size=pointS)+
  custom_colors+xlab("")+ylab("Pitch (degrees)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

plot2<-ggplot() +theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=VeDBA),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=VeDBA,colour=States_lev), size=pointS)+
  custom_colors+
  xlab("time (utc)")+ylab("VeDBA (g)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

ggarrange(plot1,plot0,plot2,ncol=1,common.legend = TRUE)


########################### diving 
subAcc<-accData[760000:769000]
subAccPoint<-subAcc[which(subAcc$States_lev=="1_Dive"|subAcc$States_lev=="2_Dive"|subAcc$States_lev=="3_Dive"|subAcc$States_lev=="4_Dive"),]
subAccPoint$States_lev<-as.factor(as.character(subAccPoint$States_lev))
#define custom color scale

myColors <-  lisa_palette("KarlZerbe")[c(1,3,4,2)]
names(myColors) <- levels(unique(subAccPoint$States_lev))
custom_colors <- scale_colour_manual(name = "diving behavioural states", values = myColors,
                                     labels=c("1_Dive"="Hunt", "2_Dive"="Swim/Cruise while diving", "3_Dive"="Descend", "4_Dive"="Ascend"),
                                     breaks = c("3_Dive", "2_Dive","1_Dive", "4_Dive"))

plot0<-ggplot() + theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=(depth25Hz*-1)),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint, aes(x=DatesPos, y=(depth25Hz*-1),colour=States_lev), size=pointS)+
  # scale_colour_igv()+
  custom_colors+
  xlab("")+ylab("Depth (m)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

plot1<-ggplot() + theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=Pitch),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=Pitch,colour=States_lev), size=pointS)+
  # scale_colour_igv()+
  custom_colors+
  xlab("")+ylab("Pitch (degrees)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

plot2<-ggplot() +theme_bw()+
  geom_line(data=subAcc,aes( x=DatesPos, y=VeDBA),colour="gray50", size=lineS)+
  geom_point(data=subAccPoint,aes( x=DatesPos, y=VeDBA,colour=States_lev), size=pointS)+
  # scale_colour_igv()+
  custom_colors+
  xlab("time (utc)")+ylab("VeDBA (g)")+
  theme(text = element_text(size=textS))+ theme(legend.text=element_text(size=textS))

ggarrange(plot0,plot1,plot2,ncol=1,common.legend = TRUE)

















