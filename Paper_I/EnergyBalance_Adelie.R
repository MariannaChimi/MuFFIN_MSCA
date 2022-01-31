
############# VeDBA Model Hicks et al 2020 formula is in Kj/g*day
# DEE= 4.64*(10^-1)±54.01*(10^-2) + (-3.46*(10^-5)±2.83*(10^-5))*VeDBAPreen  
#      +(2.06*(10^-5)±1.84*(10^−6))*VeDBAPorpoise+Dive+SeaSurface 
#      +(-1.05*(10^-5)±5.15*(10^-6))*VeDBALandPreen+Rest+Walk
#      + (−3.14*(10^−2)±2.04*(10^−2)*Sex

#              


########################


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
library(compositions)
library(BBmisc)
library("wesanderson")
library(gridExtra)
library(performance)

# filenamesTrips <-
#   list.files(
#     "/Users/mariannachimienti/MarieCurie/Adelie_2019_2020EM",
#     pattern = "*.csv",
#     full.names = TRUE
#   )

AllBudgetsDF<-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsAdelie2019_2020_All.csv",header=TRUE)
head(AllBudgetsDF)
str(AllBudgetsDF)
UniqueID<-AllBudgetsDF$ID_Ind
UniqueID<-substr(UniqueID,1,3)
AllBudgetsDF$AnimalID<-gsub('\\-', '', UniqueID)

MF_Data<-fread("/Users/mariannachimienti/MarieCurie/MF_Sheet_Adelie2019_2020.csv",header=TRUE)

AllBudgetsDF$Sex<-NA

for(i in 1:nrow(AllBudgetsDF)){
  
  AllBudgetsDF$Sex[i]<-MF_Data$Sex[which(MF_Data$ID==AllBudgetsDF$AnimalID[i])]
  
}

AllBudgetsDF$Sex_MF<-ifelse(AllBudgetsDF$Sex=="M",1,0) #if Female 0
#reorder dataset
EMPart<-AllBudgetsDF[which(AllBudgetsDF$Method=="EM"),]
RFPart<-AllBudgetsDF[which(AllBudgetsDF$Method=="RF"),]

AllBudgetsDF<-rbind(EMPart,RFPart)
head(AllBudgetsDF)
AllBudgetsDF$Sex<-as.factor(AllBudgetsDF$Sex)

summary(AllBudgetsDF)
AllBudgetsDF_Complete<-AllBudgetsDF[which(!is.na(AllBudgetsDF$Sex)),]
summary(AllBudgetsDF_Complete)
############# define empty datasets in which store results
 
# DF_Hunt<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
# DailyDEE<-data.frame(NA,NA,NA,NA,NA,NA,NA)
#tripLength<-data.frame(NA)

############# incorporate error in model
# DEE= 4.64*(10^-1)±54.01*(10^-2) + (-3.46*(10^-5)±2.83*(10^-5))*VeDBAPreen  
#      +(2.06*(10^-5)±1.84*(10^−6))*VeDBAPorpoise+Dive+SeaSurface 
#      +(-1.05*(10^-5)±5.15*(10^-6))*VeDBALandPreen+Rest+Walk
#      + (−3.14*(10^−2)±2.04*(10^−2)*Sex

errorTerms<-c(54.01*(10^-2), 2.83*(10^-5),1.84*(10^-6),5.15*(10^-6),2.04*(10^-2))
modelSE<-mean(errorTerms)
modelSD<-modelSE*sqrt(47) #58 is the sample size of Hicks et al 2020, but 47 used
errorDist<-rnorm(1000,0,modelSD)
hist(errorDist)


# ############# load data and calculate energetics
# for (TripID in 1:length(filenamesTrips)){ #:length(filenamesTrips)){
# 
# #TripID<-1
# accData <- fread(filenamesTrips[TripID], header = TRUE)
# #rownames(accData)<-seq(1,nrow(accData),1)
# #head(accData)
# tripLength[TripID]<-nrow(accData)/25
# }
# 
# AllBudgetsDF$tripLength<-as.numeric(rep(tripLength,2))
# 

# #Trip DEE
# #DEE = (0.27 ± 0.05) + (4.02 ± 0.38)meanVeDBA + (-0.05 ± 0.02)Sex, 
# 
# tripLength<-nrow(accData)/25
# HrPer<-24*60*60
# 
# meanVeDBA<-mean(accData$VeDBA)
# k_int<-0.27
# k_slope<-4.02
# 
# DEE <- k_int+ k_slope*meanVeDBA
# DEE<-(DEE/HrPer)*tripLength
# print(DEE)
# 
# tripLength<-(((nrow(accData)/25)/60)/60)/24
# 
# meanVeDBA<-mean(accData$VeDBA)
# k_int<-0.27
# k_slope<-4.02
# 
# DEE <- k_int+ k_slope*meanVeDBA
# DEE<-(DEE)*tripLength
# print(DEE)

#  # # ############ Trip EE with totVeDBA
# DEE= 4.64*(10^-1)±54.01*(10^-2) + (-3.46*(10^-5)±2.83*(10^-5))*VeDBAPreen  
#      +(2.06*(10^-5)±1.84*(10^−6))*VeDBAPorpoise+Dive+SeaSurface 
#      +(-1.05*(10^-5)±5.15*(10^-6))*VeDBALandPreen+Rest+Walk
#      + (−3.14*(10^−2)±2.04*(10^−2)*Sex


# VeDBA_Preen<- mean(accData$VeDBA[which(accData$StatesNames=="Preen/highFlap_W")])
# Time_Preen<- length(accData$VeDBA[which(accData$StatesNames=="Preen/highFlap_W")])
# 
# Time_Preen<-Time_Preen/25  #time spent in sec
#Time_Preen<-0.74*60*60

#VeDBA_Water*Time_Water
AllBudgetsDF_Complete$Preen_Comp<-AllBudgetsDF_Complete$VeDBA_Preen*AllBudgetsDF_Complete$Time_Preen_s

# VeDBA_Water<- mean(accData$VeDBA[which(accData$StatesNames=="Descending" |
#                                          accData$StatesNames=="Hunting" |
#                                          accData$StatesNames=="Ascending" |
#                                          accData$StatesNames=="Swimming" |
#                                          accData$StatesNames=="Rest" |
#                                          accData$StatesNames=="Swim/Porpoise")])
# 
# 
# Time_Water<- length(accData$VeDBA[which(accData$StatesNames=="Descending" |
#                                           accData$StatesNames=="Hunting" |
#                                           accData$StatesNames=="Ascending" |
#                                           accData$StatesNames=="Swimming" |
#                                           accData$StatesNames=="Rest" |
#                                           accData$StatesNames=="Swim/Porpoise")])
# 
# 
# Time_Water<-Time_Water/25  #time spent in sec
#Time_Water<-4.05*60*60

#VeDBA_Water*Time_Water
AllBudgetsDF_Complete$water_Comp<-AllBudgetsDF_Complete$VeDBA_Water*AllBudgetsDF_Complete$Time_Water_s

# VeDBA_Land<-mean(accData$VeDBA[which(accData$StatesNames=="Preen/highFlap_L"|
#                                        accData$StatesNames=="Walk" |
#                                        accData$StatesNames=="LieDown" |
#                                        accData$StatesNames=="Stand" )])
# 
# Time_Land<-length(accData$VeDBA[which(accData$StatesNames=="Preen/highFlap_L"|
#                                         accData$StatesNames=="Walk" |
#                                         accData$StatesNames=="LieDown" |
#                                         accData$StatesNames=="Stand" )])
# 
# Time_Land<-Time_Land/25#
#Time_Land<-19.93*60*60
#VeDBA_Water*Time_Water
AllBudgetsDF_Complete$Land_Comp<-AllBudgetsDF_Complete$VeDBA_Land*AllBudgetsDF_Complete$Time_Land_s


#variance in k
k_int<- 4.64*(10^-1)
k_VeDBA_Preen<- -3.46*(10^-5)
k_VeDBA_Water<- 2.06*(10^-5)
k_VeDBA_Land<- -1.05*(10^-5)

#

#Error for VeDBA model

# DEE= 4.64*(10^-1)±54.01*(10^-2) + (-3.46*(10^-5)±2.83*(10^-5))*VeDBAPreen
#      +(2.06*(10^-5)±1.84*(10^−6))*VeDBAPorpoise+Dive+SeaSurface
#      +(-1.05*(10^-5)±5.15*(10^-6))*VeDBALandPreen+Rest+Walk
#      + (−3.14*(10^−2)±2.04*(10^−2)


# Dive (h)	Rest (h)	Float (h)	Walk (h)	Porpoise (h)	Preen (h)
# 2.64	19.44	0.04	0.49	0.92	0.74
#k_int + (k_VeDBA_Water*water_Comp) 
#

for(bud_i in 1:nrow(AllBudgetsDF_Complete)){

EE_VeDBA_Sim<-rep(NA,length(errorDist))

for(errorN in 1:length(errorDist)){
  k_Sex<- -3.14*(10^-2)*AllBudgetsDF_Complete$Sex_MF[bud_i] #if female it will be 0
  
  EE_VeDBA_Sim[errorN]<- k_int + (k_VeDBA_Preen*AllBudgetsDF_Complete$Preen_Comp[bud_i]) +
  (k_VeDBA_Water*AllBudgetsDF_Complete$water_Comp[bud_i]) +
  (k_VeDBA_Land*AllBudgetsDF_Complete$Land_Comp[bud_i])+k_Sex+errorDist[errorN]
  
  

   }

EE_VeDBA<-mean(EE_VeDBA_Sim)
#tripLength<-nrow(accData)/25
HrPer<-24*60*60
#AllBudgetsDF$EE_VeDBA[bud_i]<-(EE_VeDBA/HrPer)*AllBudgetsDF$tripLength[bud_i]
AllBudgetsDF_Complete$EE_VeDBA[bud_i]<-EE_VeDBA
#print(EE_VeDBA)
  }


write.csv(AllBudgetsDF_Complete, "/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsAdelie2019_2020_All_E.csv", row.names = FALSE)

#plots
##############################################################################
##############################################################################
##############################################################################

lineS<-0.7
pointS<-6
textS<-20


AllBudgetsDF_Complete<-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsAdelie2019_2020_All_E.csv",header=TRUE)
dim(AllBudgetsDF_Complete)

boxplot(AllBudgetsDF_Complete$EE_VeDBA~AllBudgetsDF_Complete$Method)
tapply(AllBudgetsDF_Complete$EE_VeDBA,AllBudgetsDF_Complete$Method,summary)
AllBudgetsDF_Complete$ID_Ind<-as.factor(AllBudgetsDF_Complete$ID_Ind)


plot1<-ggplot(AllBudgetsDF_Complete, aes(x=Method, y=EE_VeDBA,fill=Method),color="gray35") + 
  geom_boxplot(outlier.size = pointS)+
 # geom_jitter(aes(color=ID_Ind), size=3, alpha=0.7) +
 # scale_color_viridis(discrete = TRUE, alpha=0.6,guide=FALSE) +
  theme_bw() +theme(text = element_text(size=textS))+
  scale_fill_manual(name="",values=c("white","white"),labels = c("EM: Expectation Maximisation", "RF: Random Forest"))+
#  xlab("")+ylab(expression(paste("Energy Expenditure (Kj ",g^-1," trip ", duration^-1,")")))+
  xlab("")+ylab(expression(paste("Energy Expenditure (Kj ",g^-1," d", ay^-1,")")))+
  theme(legend.text=element_text(size=textS)) + theme(legend.position="top")

plot1

##############################################################################

AllBudgetsDF_Complete$TotTime<-AllBudgetsDF_Complete$Time_Preen_s+AllBudgetsDF_Complete$Time_Water_s+AllBudgetsDF_Complete$Time_Land_s #remember this tot time is less than trip length as obs are missing as they were put in the training.
AllBudgetsDF_Complete$PropPreen<-AllBudgetsDF_Complete$Time_Preen_s/AllBudgetsDF_Complete$TotTime
AllBudgetsDF_Complete$PropWater<-AllBudgetsDF_Complete$Time_Water_s/AllBudgetsDF_Complete$TotTime
AllBudgetsDF_Complete$PropLand<-AllBudgetsDF_Complete$Time_Land_s/AllBudgetsDF_Complete$TotTime

Prop_DF_Preen<-AllBudgetsDF_Complete[,c("PropPreen","Method","ID_Ind")]
colnames(Prop_DF_Preen)[1]<-"val"
Prop_DF_Preen$Beh<-"Preen/highFlap_W"

Prop_DF_Water<-AllBudgetsDF_Complete[,c("PropWater","Method","ID_Ind")]
colnames(Prop_DF_Water)[1]<-"val"
Prop_DF_Water$Beh<-"Water"

Prop_DF_Land<-AllBudgetsDF_Complete[,c("PropLand","Method","ID_Ind")]
colnames(Prop_DF_Land)[1]<-"val"
Prop_DF_Land$Beh<-"Land/Ice"

Prop_DF_All<-rbind(Prop_DF_Preen,Prop_DF_Water,Prop_DF_Land)


plot2<-ggplot(Prop_DF_All, aes(x=Method, y=val,fill=Beh)) + 
  geom_boxplot(outlier.size = pointS)+
  scale_fill_manual(name="",values=wes_palette(name="Royal1"),labels = c("Land/Ice", "Preen/Flap on water","Water"))+
  theme_bw() +theme(text = element_text(size=textS))+
  xlab("")+ylab("Proportion")+ylim(0,1)+
  theme(legend.text=element_text(size=textS)) + theme(legend.position="top")

plot2

##############################################################################

EMPart<-AllBudgetsDF_Complete[which(AllBudgetsDF_Complete$Method=="EM"),c("EE_VeDBA","ID_Ind")]
RFPart<-AllBudgetsDF_Complete[which(AllBudgetsDF_Complete$Method=="RF"),c("EE_VeDBA","ID_Ind")]
DiffPropWater<-AllBudgetsDF_Complete$PropWater[which(AllBudgetsDF_Complete$Method=="EM")]-AllBudgetsDF_Complete$PropWater[which(AllBudgetsDF_Complete$Method=="RF")]
DiffPropPreen<-AllBudgetsDF_Complete$PropPreen[which(AllBudgetsDF_Complete$Method=="EM")]-AllBudgetsDF_Complete$PropPreen[which(AllBudgetsDF_Complete$Method=="RF")]
DiffPropLand<-AllBudgetsDF_Complete$PropLand[which(AllBudgetsDF_Complete$Method=="EM")]-AllBudgetsDF_Complete$PropLand[which(AllBudgetsDF_Complete$Method=="RF")]

colnames(EMPart)[1]<-"EE_VeDBA_EM"
colnames(RFPart)[1]<-"EE_VeDBA_RF"

colnames(EMPart)[2]<-"ID_Ind_EM"
colnames(RFPart)[2]<-"ID_Ind_RF"

Meth_DF<-cbind(EMPart,RFPart)
Meth_DF$DiffPropWater<-DiffPropWater
Meth_DF$DiffPropPreen<-DiffPropPreen
Meth_DF$DiffPropLand<-DiffPropLand


mod1<-lm(EE_VeDBA_EM~EE_VeDBA_RF,data=Meth_DF)
summary(mod1)

par(mfrow=c(2,2))
plot(mod1)


mod2<-lm(EE_VeDBA_EM~EE_VeDBA_RF+DiffPropWater,data=Meth_DF)
summary(mod2)
check_collinearity(mod2)

par(mfrow=c(2,2))
plot(mod2)

mod3<-lm(EE_VeDBA_EM~EE_VeDBA_RF+DiffPropPreen,data=Meth_DF)
summary(mod3)
check_collinearity(mod3)

par(mfrow=c(2,2))
plot(mod3)

mod4<-lm(EE_VeDBA_EM~EE_VeDBA_RF+DiffPropLand,data=Meth_DF)
summary(mod4)
check_collinearity(mod4)

par(mfrow=c(2,2))
plot(mod4)


# For model comparison, the model with the lowest AIC score is preferred. 
#The absolute values of the AIC scores do not matter. These scores can be negative or positive.
# In your example, the model with AIC=−237.847 is preferred over the model with AIC=−201.928.
AIC(mod1,mod2,mod3,mod4)

# predlm = as.data.frame(predict(mod1, interval = "confidence"))
# Meth_DF$fit<-predlm$fit
# Meth_DF$lwr<-predlm$lwr
# Meth_DF$upr<-predlm$upr


newdat.lme = data.frame(EE_VeDBA_RF = Meth_DF$EE_VeDBA_RF,
                        DiffPropPreen = mean(Meth_DF$DiffPropPreen))
head(newdat.lme)
predlm = as.data.frame(predict(mod3, newdata = newdat.lme,interval = "confidence"))
Meth_DF$fit<-predlm$fit
Meth_DF$lwr<-predlm$lwr
Meth_DF$upr<-predlm$upr

plot3<-ggplot(Meth_DF) + 
  geom_point(aes(x=EE_VeDBA_RF, y=EE_VeDBA_EM,color=ID_Ind_EM),color="black", size=pointS) +
  #scale_color_viridis(discrete = TRUE, alpha=0.6,guide=FALSE) +
  geom_line(aes(x=EE_VeDBA_RF,y = fit), size = 1,color = "gray45")+
  geom_ribbon(aes(x=EE_VeDBA_RF,ymin = lwr, ymax = upr),  color = "gray45",alpha = .15,linetype="dashed")+
  theme_bw() +theme(text = element_text(size=textS))+
  ylim(0.30,1.15)+xlim(0.30,1.15)+
  geom_abline(slope=1, intercept=0,  color = "dodgerblue3",linetype="dashed", size = 1)+
  #  xlab("")+ylab(expression(paste("Energy Expenditure (Kj ",g^-1," trip ", duration^-1,")")))+
  xlab(expression(paste("Energy Expenditure by RF (Kj ",g^-1," d", ay^-1,")")))+
  ylab(expression(paste("Energy Expenditure by EM (Kj ",g^-1," d", ay^-1,")")))+
  theme(legend.text=element_text(size=textS)) + theme(legend.position="top")

plot3


#ggarrange(plot1,plot2,plot3, nrow=2,ncol=2,common.legend = FALSE)

grid.arrange(plot1, plot2,                                      # bar plot spaning two columns
             plot3,                              # box plot and scatter plot
             ncol = 2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)))                                             








