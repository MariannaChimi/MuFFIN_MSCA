rm(list=ls())
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
library(dplyr)
library("wesanderson")

lineS<-0.7
pointS<-6
textS<-20

############## ############## ############## ############## ############## ############## 
############## ############## ############## ############## ############## ############## 
############## plot all seasons and compare
#### Little
############## ############## ############## ############## ############## ############## 
############## ############## ############## ############## ############## ############## 

AccuracyDFLittle_2019 <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsLittle_2019_All.csv",header=TRUE)
AccuracyDFLittle_2019$Season<-2
AccuracyDFLittle_2019$Train_Test<-"In Predict"
AccuracyDFLittle_2019$RFType<-"1Season"
#AccuracyDFLittle_2019<-AccuracyDFLittle_2019[,-c(11,12)]
summary(AccuracyDFLittle_2019)


AccuracyDFLittle_2020_MixNotIn <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsLittle_2020_MixNotInTraining.csv",header=TRUE)
AccuracyDFLittle_2020_MixNotIn$Season<-1
AccuracyDFLittle_2020_MixNotIn$Train_Test<-"In Predict"
AccuracyDFLittle_2020_MixNotIn$RFType<-"MixSeason"
summary(AccuracyDFLittle_2020_MixNotIn)


AccuracyDFLittle_2019_MixNotIn <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsLittle_2019_MixNotInTraining.csv",header=TRUE)
AccuracyDFLittle_2019_MixNotIn$Season<-2
AccuracyDFLittle_2019_MixNotIn$Train_Test<-"In Predict"
AccuracyDFLittle_2019_MixNotIn$RFType<-"MixSeason"
summary(AccuracyDFLittle_2019_MixNotIn)


#attach individual ID
AccuracyDFLittle_2019_Ag <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFLittle_2019_All.csv",header=TRUE)
AccuracyDFLittle_2020_MixNotIn_Ag <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFLittle_2020_MixNotInTraining.csv",header=TRUE)
AccuracyDFLittle_2019_MixNotIn_Ag <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFLittle_2019_MixNotInTraining.csv",header=TRUE)

############## ############## ############## ############## ############## ############## 
############## ############## ############## ############## ############## ############## 
BudgetLittle_2019_RF<-AccuracyDFLittle_2019[which(AccuracyDFLittle_2019$Method=="RF"),]
BudgetLittle_2019_RF$ID_Ind<-AccuracyDFLittle_2019_Ag$ID_Ind

Still<-as.data.frame(BudgetLittle_2019_RF[,c(1,13)])
Still$beh<-"Still"
Still$Method<-"RF"
colnames(Still)[1]<-"bud_value"

# Still1<-as.data.frame(BudgetLittle_2019_RF[,2])
# Still1$beh<-"Still1"
# Still1$Method<-"RF"
# colnames(Still1)[1]<-"bud_value"

Swim_Porpoise<-as.data.frame(BudgetLittle_2019_RF[,c(2,13)])
Swim_Porpoise$beh<-"Swim/Porpoise"
Swim_Porpoise$Method<-"RF"
colnames(Swim_Porpoise)[1]<-"bud_value"

PreenHighFlap_W<-as.data.frame(BudgetLittle_2019_RF[,c(3,13)])
PreenHighFlap_W$beh<-"Preen/highFlap_W"
PreenHighFlap_W$Method<-"RF"
colnames(PreenHighFlap_W)[1]<-"bud_value"

Rest<-as.data.frame(BudgetLittle_2019_RF[,c(4,13)])
Rest$beh<-"Rest"
Rest$Method<-"RF"
colnames(Rest)[1]<-"bud_value"

Hunting<-as.data.frame(BudgetLittle_2019_RF[,c(5,13)])
Hunting$beh<-"Hunting"
Hunting$Method<-"RF"
colnames(Hunting)[1]<-"bud_value"

Ascending<-as.data.frame(BudgetLittle_2019_RF[,c(6,13)])
Ascending$beh<-"Ascending"
Ascending$Method<-"RF"
colnames(Ascending)[1]<-"bud_value"

Swimming<-as.data.frame(BudgetLittle_2019_RF[,c(7,13)])
Swimming$beh<-"Swimming"
Swimming$Method<-"RF"
colnames(Swimming)[1]<-"bud_value"

Descending<-as.data.frame(BudgetLittle_2019_RF[,c(8,13)])
Descending$beh<-"Descending"
Descending$Method<-"RF"
colnames(Descending)[1]<-"bud_value"


RFBud<-rbind(PreenHighFlap_W,Swim_Porpoise,Still,Rest,Hunting,Ascending,Swimming,Descending)

BudgetLittle_2019_EM<-AccuracyDFLittle_2019[which(AccuracyDFLittle_2019$Method=="EM"),]
BudgetLittle_2019_EM$ID_Ind<-AccuracyDFLittle_2019_Ag$ID_Ind


Still<-as.data.frame(BudgetLittle_2019_EM[,c(1,13)])
Still$beh<-"Still"
Still$Method<-"EM"
colnames(Still)[1]<-"bud_value"

# Still1<-as.data.frame(BudgetLittle_2019_EM[,2])
# Still1$beh<-"Still1"
# Still1$Method<-"EM"
# colnames(Still1)[1]<-"bud_value"

Swim_Porpoise<-as.data.frame(BudgetLittle_2019_EM[,c(2,13)])
Swim_Porpoise$beh<-"Swim/Porpoise"
Swim_Porpoise$Method<-"EM"
colnames(Swim_Porpoise)[1]<-"bud_value"

PreenHighFlap_W<-as.data.frame(BudgetLittle_2019_EM[,c(3,13)])
PreenHighFlap_W$beh<-"Preen/highFlap_W"
PreenHighFlap_W$Method<-"EM"
colnames(PreenHighFlap_W)[1]<-"bud_value"

Rest<-as.data.frame(BudgetLittle_2019_EM[,c(4,13)])
Rest$beh<-"Rest"
Rest$Method<-"EM"
colnames(Rest)[1]<-"bud_value"

Hunting<-as.data.frame(BudgetLittle_2019_EM[,c(5,13)])
Hunting$beh<-"Hunting"
Hunting$Method<-"EM"
colnames(Hunting)[1]<-"bud_value"

Ascending<-as.data.frame(BudgetLittle_2019_EM[,c(6,13)])
Ascending$beh<-"Ascending"
Ascending$Method<-"EM"
colnames(Ascending)[1]<-"bud_value"

Swimming<-as.data.frame(BudgetLittle_2019_EM[,c(7,13)])
Swimming$beh<-"Swimming"
Swimming$Method<-"EM"
colnames(Swimming)[1]<-"bud_value"

Descending<-as.data.frame(BudgetLittle_2019_EM[,c(8,13)])
Descending$beh<-"Descending"
Descending$Method<-"EM"
colnames(Descending)[1]<-"bud_value"


EMBud<-rbind(PreenHighFlap_W,Swim_Porpoise,Still,Rest,Hunting,Ascending,Swimming,Descending)

BudgetAllLittle_2019<-rbind(RFBud,EMBud)

BudgetAllLittle_2019$Method<-as.factor(BudgetAllLittle_2019$Method)
BudgetAllLittle_2019$beh<-as.factor(BudgetAllLittle_2019$beh)


BudgetAllLittle_2019$Beh<-factor(BudgetAllLittle_2019$beh, levels = c("Rest","Preen/highFlap_W","Swim/Porpoise","Still",
                                              "Descending","Swimming","Hunting","Ascending"))

BudgetAllLittle_2019$Beh<-ifelse(BudgetAllLittle_2019$Beh=="Rest","Slow surface swim",
                     ifelse(BudgetAllLittle_2019$Beh=="Preen/highFlap_W","Preen/Flap on water",
                            ifelse(BudgetAllLittle_2019$Beh=="Descending","Descend",
                                   ifelse(BudgetAllLittle_2019$Beh=="Swimming","Swim/Cruise while diving",
                                          ifelse(BudgetAllLittle_2019$Beh=="Hunting","Hunt",
                                                 ifelse(BudgetAllLittle_2019$Beh=="Ascending","Ascend",as.character(BudgetAllLittle_2019$Beh)))))))

plot1<- ggplot() + theme_bw() + 
        geom_boxplot(aes(y = bud_value, x = Beh, fill = Method), data = BudgetAllLittle_2019)+
        ylab("proportion")+xlab("")+theme(text = element_text(size=15))+ylim(0,1)+
        scale_fill_manual(values=wes_palette(n=2, name="Darjeeling2"),name="")+
        ggtitle("Training from season 1, prediction on season 2")


BudgetAllLittle_2019_EM<-BudgetAllLittle_2019[which(BudgetAllLittle_2019$Method=="EM"),]
colnames(BudgetAllLittle_2019_EM)[1]<-"bud_value_EM"
BudgetAllLittle_2019_RF<-BudgetAllLittle_2019[which(BudgetAllLittle_2019$Method=="RF"),]
colnames(BudgetAllLittle_2019_RF)[1]<-"bud_value_RF"

BudgetAllLittle_2019_Comp<-cbind(BudgetAllLittle_2019_EM,BudgetAllLittle_2019_RF[,c("bud_value_RF")])
colnames(BudgetAllLittle_2019_Comp)[6]<-"bud_value_RF"


# ascend<-BudgetAllLittle_2019_Comp[which(BudgetAllLittle_2019_Comp$beh=="Ascending"),]
# 
# m<-lm(bud_value_EM~bud_value_RF,data=ascend)

plot1a<-ggplot(BudgetAllLittle_2019_Comp) + 
  geom_point(aes(x=bud_value_RF, y=bud_value_EM,color=ID_Ind), size=pointS,alpha=0.6) +
#  geom_smooth(aes(x=bud_value_RF, y=bud_value_EM),method = "lm", se=TRUE, color="gray35", formula = y ~ x) +
  scale_color_viridis(discrete = TRUE, alpha=0.6,guide=FALSE) +
  geom_abline(slope=1, intercept=0,  color = "gray35",linetype="dashed",size=lineS)+
  ggtitle("Training from season 1, prediction on season 2")+
  # geom_line(aes(x=EE_VeDBA_RF,y = fit), size = 1,color = "gray45")+
  # geom_ribbon(aes(x=EE_VeDBA_RF,ymin = lwr, ymax = upr),  color = "gray45",alpha = .15,linetype="dashed")+
  theme_bw() +theme(text = element_text(size=textS))+
  ylim(0,0.7)+xlim(0,0.7)+
  #  xlab("")+ylab(expression(paste("Energy Expenditure (Kj ",g^-1," trip ", duration^-1,")")))+
  xlab("Budget predicted by RF")+
  ylab("Budget predicted by EM")+ facet_wrap(~Beh)+
 # geom_text(data=eq,aes(x = 0.2, y = 0.6,label=V1), parse = TRUE, inherit.aes=FALSE)+
  theme(legend.text=element_text(size=textS)) + theme(legend.position="top")

plot1a

############## ############## ############## ############## ############## ############## 
############## ############## ############## ############## ############## ############## 

BudgetLittle_2020_MixNotIn_RF<-AccuracyDFLittle_2020_MixNotIn[which(AccuracyDFLittle_2020_MixNotIn$Method=="RF"),]
BudgetLittle_2020_MixNotIn_RF$ID_Ind<-AccuracyDFLittle_2020_MixNotIn_Ag$ID_Ind


Still<-as.data.frame(BudgetLittle_2020_MixNotIn_RF[,c(1,13)])
Still$beh<-"Still"
Still$Method<-"RF"
colnames(Still)[1]<-"bud_value"

# Still1<-as.data.frame(BudgetLittle_2020_MixNotIn_RF[,2])
# Still1$beh<-"Still1"
# Still1$Method<-"RF"
# colnames(Still1)[1]<-"bud_value"

Swim_Porpoise<-as.data.frame(BudgetLittle_2020_MixNotIn_RF[,c(2,13)])
Swim_Porpoise$beh<-"Swim/Porpoise"
Swim_Porpoise$Method<-"RF"
colnames(Swim_Porpoise)[1]<-"bud_value"

PreenHighFlap_W<-as.data.frame(BudgetLittle_2020_MixNotIn_RF[,c(3,13)])
PreenHighFlap_W$beh<-"Preen/highFlap_W"
PreenHighFlap_W$Method<-"RF"
colnames(PreenHighFlap_W)[1]<-"bud_value"

Rest<-as.data.frame(BudgetLittle_2020_MixNotIn_RF[,c(4,13)])
Rest$beh<-"Rest"
Rest$Method<-"RF"
colnames(Rest)[1]<-"bud_value"

Hunting<-as.data.frame(BudgetLittle_2020_MixNotIn_RF[,c(5,13)])
Hunting$beh<-"Hunting"
Hunting$Method<-"RF"
colnames(Hunting)[1]<-"bud_value"

Ascending<-as.data.frame(BudgetLittle_2020_MixNotIn_RF[,c(6,13)])
Ascending$beh<-"Ascending"
Ascending$Method<-"RF"
colnames(Ascending)[1]<-"bud_value"

Swimming<-as.data.frame(BudgetLittle_2020_MixNotIn_RF[,c(7,13)])
Swimming$beh<-"Swimming"
Swimming$Method<-"RF"
colnames(Swimming)[1]<-"bud_value"

Descending<-as.data.frame(BudgetLittle_2020_MixNotIn_RF[,c(8,13)])
Descending$beh<-"Descending"
Descending$Method<-"RF"
colnames(Descending)[1]<-"bud_value"


RFBud<-rbind(PreenHighFlap_W,Swim_Porpoise,Still,Rest,Hunting,Ascending,Swimming,Descending)

BudgetLittle_2020_MixNotIn_EM<-AccuracyDFLittle_2020_MixNotIn[which(AccuracyDFLittle_2020_MixNotIn$Method=="EM"),]
BudgetLittle_2020_MixNotIn_EM$ID_Ind<-AccuracyDFLittle_2020_MixNotIn_Ag$ID_Ind


Still<-as.data.frame(BudgetLittle_2020_MixNotIn_EM[,c(1,13)])
Still$beh<-"Still"
Still$Method<-"EM"
colnames(Still)[1]<-"bud_value"

# Still1<-as.data.frame(BudgetLittle_2020_MixNotIn_EM[,2])
# Still1$beh<-"Still1"
# Still1$Method<-"EM"
# colnames(Still1)[1]<-"bud_value"

Swim_Porpoise<-as.data.frame(BudgetLittle_2020_MixNotIn_EM[,c(2,13)])
Swim_Porpoise$beh<-"Swim/Porpoise"
Swim_Porpoise$Method<-"EM"
colnames(Swim_Porpoise)[1]<-"bud_value"

PreenHighFlap_W<-as.data.frame(BudgetLittle_2020_MixNotIn_EM[,c(3,13)])
PreenHighFlap_W$beh<-"Preen/highFlap_W"
PreenHighFlap_W$Method<-"EM"
colnames(PreenHighFlap_W)[1]<-"bud_value"

Rest<-as.data.frame(BudgetLittle_2020_MixNotIn_EM[,c(4,13)])
Rest$beh<-"Rest"
Rest$Method<-"EM"
colnames(Rest)[1]<-"bud_value"

Hunting<-as.data.frame(BudgetLittle_2020_MixNotIn_EM[,c(5,13)])
Hunting$beh<-"Hunting"
Hunting$Method<-"EM"
colnames(Hunting)[1]<-"bud_value"

Ascending<-as.data.frame(BudgetLittle_2020_MixNotIn_EM[,c(6,13)])
Ascending$beh<-"Ascending"
Ascending$Method<-"EM"
colnames(Ascending)[1]<-"bud_value"

Swimming<-as.data.frame(BudgetLittle_2020_MixNotIn_EM[,c(7,13)])
Swimming$beh<-"Swimming"
Swimming$Method<-"EM"
colnames(Swimming)[1]<-"bud_value"

Descending<-as.data.frame(BudgetLittle_2020_MixNotIn_EM[,c(8,13)])
Descending$beh<-"Descending"
Descending$Method<-"EM"
colnames(Descending)[1]<-"bud_value"


EMBud<-rbind(PreenHighFlap_W,Swim_Porpoise,Still,Rest,Hunting,Ascending,Swimming,Descending)

BudgetAllLittle_2020_MixNotIn<-rbind(RFBud,EMBud)

BudgetAllLittle_2020_MixNotIn$Method<-as.factor(BudgetAllLittle_2020_MixNotIn$Method)
BudgetAllLittle_2020_MixNotIn$beh<-as.factor(BudgetAllLittle_2020_MixNotIn$beh)

BudgetAllLittle_2020_MixNotIn$Beh<-factor(BudgetAllLittle_2020_MixNotIn$beh, levels = c("Rest","Preen/highFlap_W","Swim/Porpoise","Still",
                                                                      "Descending","Swimming","Hunting","Ascending"))

BudgetAllLittle_2020_MixNotIn$Beh<-ifelse(BudgetAllLittle_2020_MixNotIn$Beh=="Rest","Slow surface swim",
                                 ifelse(BudgetAllLittle_2020_MixNotIn$Beh=="Preen/highFlap_W","Preen/Flap on water",
                                        ifelse(BudgetAllLittle_2020_MixNotIn$Beh=="Descending","Descend",
                                               ifelse(BudgetAllLittle_2020_MixNotIn$Beh=="Swimming","Swim/Cruise while diving",
                                                      ifelse(BudgetAllLittle_2020_MixNotIn$Beh=="Hunting","Hunt",
                                                             ifelse(BudgetAllLittle_2020_MixNotIn$Beh=="Ascending","Ascend",as.character(BudgetAllLittle_2020_MixNotIn$Beh)))))))

plot2<-ggplot() + theme_bw() +  
  geom_boxplot(aes(y = bud_value, x = Beh, fill = Method), data = BudgetAllLittle_2020_MixNotIn)+
  ylab("proportion")+xlab("")+theme(text = element_text(size=15))+ylim(0,1)+
  scale_fill_manual(values=wes_palette(n=2, name="Darjeeling2"),name="")+ 
  ggtitle("Training from both seasons, prediction on season 1")


BudgetAllLittle_2020_MixNotIn_EM<-BudgetAllLittle_2020_MixNotIn[which(BudgetAllLittle_2020_MixNotIn$Method=="EM"),]
colnames(BudgetAllLittle_2020_MixNotIn_EM)[1]<-"bud_value_EM"
BudgetAllLittle_2020_MixNotIn_RF<-BudgetAllLittle_2020_MixNotIn[which(BudgetAllLittle_2020_MixNotIn$Method=="RF"),]
colnames(BudgetAllLittle_2020_MixNotIn_RF)[1]<-"bud_value_RF"

BudgetAllLittle_2020_MixNotIn_Comp<-cbind(BudgetAllLittle_2020_MixNotIn_EM,BudgetAllLittle_2020_MixNotIn_RF[,c("bud_value_RF")])
colnames(BudgetAllLittle_2020_MixNotIn_Comp)[6]<-"bud_value_RF"


ascend<-BudgetAllLittle_2020_MixNotIn_Comp[which(BudgetAllLittle_2020_MixNotIn_Comp$beh=="Ascending"),]

m<-lm(bud_value_EM~bud_value_RF,data=ascend)

plot2a<-ggplot(BudgetAllLittle_2020_MixNotIn_Comp) + 
  geom_point(aes(x=bud_value_RF, y=bud_value_EM,color=ID_Ind), size=pointS,alpha=0.6) +
 # geom_smooth(aes(x=bud_value_RF, y=bud_value_EM),method = "lm", se=TRUE, color="gray35", formula = y ~ x) +
  scale_color_viridis(discrete = TRUE, alpha=0.6,guide="none") +
  geom_abline(slope=1, intercept=0,  color = "gray35",linetype="dashed",size=lineS)+
  # geom_line(aes(x=EE_VeDBA_RF,y = fit), size = 1,color = "gray45")+
  # geom_ribbon(aes(x=EE_VeDBA_RF,ymin = lwr, ymax = upr),  color = "gray45",alpha = .15,linetype="dashed")+
  theme_bw() +theme(text = element_text(size=textS))+
  ylim(0,0.7)+xlim(0,0.7)+
  #  xlab("")+ylab(expression(paste("Energy Expenditure (Kj ",g^-1," trip ", duration^-1,")")))+
  xlab("Budget predicted by RF")+
  ylab("Budget predicted by EM")+ facet_wrap(~Beh)+
  # geom_text(data=eq,aes(x = 0.2, y = 0.6,label=V1), parse = TRUE, inherit.aes=FALSE)+
  theme(legend.text=element_text(size=textS)) + theme(legend.position="top")+ 
  ggtitle("Training from both seasons, prediction on season 1")

plot2a

############## ############## ############## ############## ############## ############## 
############## ############## ############## ############## ############## ############## 

BudgetLittle_2019MixNotIn_RF<-AccuracyDFLittle_2019_MixNotIn[which(AccuracyDFLittle_2019_MixNotIn$Method=="RF"),]
BudgetLittle_2019MixNotIn_RF$ID_Ind<-AccuracyDFLittle_2019_MixNotIn_Ag$ID_Ind

Still<-as.data.frame(BudgetLittle_2019MixNotIn_RF[,c(1,13)])
Still$beh<-"Still"
Still$Method<-"RF"
colnames(Still)[1]<-"bud_value"

# Still1<-as.data.frame(BudgetLittle_2019MixNotIn_RF[,2])
# Still1$beh<-"Still1"
# Still1$Method<-"RF"
# colnames(Still1)[1]<-"bud_value"

Swim_Porpoise<-as.data.frame(BudgetLittle_2019MixNotIn_RF[,c(2,13)])
Swim_Porpoise$beh<-"Swim/Porpoise"
Swim_Porpoise$Method<-"RF"
colnames(Swim_Porpoise)[1]<-"bud_value"

PreenHighFlap_W<-as.data.frame(BudgetLittle_2019MixNotIn_RF[,c(3,13)])
PreenHighFlap_W$beh<-"Preen/highFlap_W"
PreenHighFlap_W$Method<-"RF"
colnames(PreenHighFlap_W)[1]<-"bud_value"

Rest<-as.data.frame(BudgetLittle_2019MixNotIn_RF[,c(4,13)])
Rest$beh<-"Rest"
Rest$Method<-"RF"
colnames(Rest)[1]<-"bud_value"

Hunting<-as.data.frame(BudgetLittle_2019MixNotIn_RF[,c(5,13)])
Hunting$beh<-"Hunting"
Hunting$Method<-"RF"
colnames(Hunting)[1]<-"bud_value"

Ascending<-as.data.frame(BudgetLittle_2019MixNotIn_RF[,c(6,13)])
Ascending$beh<-"Ascending"
Ascending$Method<-"RF"
colnames(Ascending)[1]<-"bud_value"

Swimming<-as.data.frame(BudgetLittle_2019MixNotIn_RF[,c(7,13)])
Swimming$beh<-"Swimming"
Swimming$Method<-"RF"
colnames(Swimming)[1]<-"bud_value"

Descending<-as.data.frame(BudgetLittle_2019MixNotIn_RF[,c(8,13)])
Descending$beh<-"Descending"
Descending$Method<-"RF"
colnames(Descending)[1]<-"bud_value"


RFBud<-rbind(PreenHighFlap_W,Swim_Porpoise,Still,Rest,Hunting,Ascending,Swimming,Descending)

BudgetLittle_2019MixNotIn_EM<-AccuracyDFLittle_2019_MixNotIn[which(AccuracyDFLittle_2019_MixNotIn$Method=="EM"),]
BudgetLittle_2019MixNotIn_EM$ID_Ind<-AccuracyDFLittle_2019_MixNotIn_Ag$ID_Ind


Still<-as.data.frame(BudgetLittle_2019MixNotIn_EM[,c(1,13)])
Still$beh<-"Still"
Still$Method<-"EM"
colnames(Still)[1]<-"bud_value"

# Still1<-as.data.frame(BudgetLittle_2019MixNotIn_EM[,2])
# Still1$beh<-"Still1"
# Still1$Method<-"EM"
# colnames(Still1)[1]<-"bud_value"

Swim_Porpoise<-as.data.frame(BudgetLittle_2019MixNotIn_EM[,c(2,13)])
Swim_Porpoise$beh<-"Swim/Porpoise"
Swim_Porpoise$Method<-"EM"
colnames(Swim_Porpoise)[1]<-"bud_value"

PreenHighFlap_W<-as.data.frame(BudgetLittle_2019MixNotIn_EM[,c(3,13)])
PreenHighFlap_W$beh<-"Preen/highFlap_W"
PreenHighFlap_W$Method<-"EM"
colnames(PreenHighFlap_W)[1]<-"bud_value"

Rest<-as.data.frame(BudgetLittle_2019MixNotIn_EM[,c(4,13)])
Rest$beh<-"Rest"
Rest$Method<-"EM"
colnames(Rest)[1]<-"bud_value"

Hunting<-as.data.frame(BudgetLittle_2019MixNotIn_EM[,c(5,13)])
Hunting$beh<-"Hunting"
Hunting$Method<-"EM"
colnames(Hunting)[1]<-"bud_value"

Ascending<-as.data.frame(BudgetLittle_2019MixNotIn_EM[,c(6,13)])
Ascending$beh<-"Ascending"
Ascending$Method<-"EM"
colnames(Ascending)[1]<-"bud_value"

Swimming<-as.data.frame(BudgetLittle_2019MixNotIn_EM[,c(7,13)])
Swimming$beh<-"Swimming"
Swimming$Method<-"EM"
colnames(Swimming)[1]<-"bud_value"

Descending<-as.data.frame(BudgetLittle_2019MixNotIn_EM[,c(8,13)])
Descending$beh<-"Descending"
Descending$Method<-"EM"
colnames(Descending)[1]<-"bud_value"


EMBud<-rbind(PreenHighFlap_W,Swim_Porpoise,Still,Rest,Hunting,Ascending,Swimming,Descending)

BudgetAllLittleMixNotIn_2019<-rbind(RFBud,EMBud)

BudgetAllLittleMixNotIn_2019$Method<-as.factor(BudgetAllLittleMixNotIn_2019$Method)
BudgetAllLittleMixNotIn_2019$beh<-as.factor(BudgetAllLittleMixNotIn_2019$beh)


BudgetAllLittleMixNotIn_2019$Beh<-factor(BudgetAllLittleMixNotIn_2019$beh, levels = c("Rest","Preen/highFlap_W","Swim/Porpoise","Still",
                                                                                        "Descending","Swimming","Hunting","Ascending"))

BudgetAllLittleMixNotIn_2019$Beh<-ifelse(BudgetAllLittleMixNotIn_2019$Beh=="Rest","Slow surface swim",
                                          ifelse(BudgetAllLittleMixNotIn_2019$Beh=="Preen/highFlap_W","Preen/Flap on water",
                                                 ifelse(BudgetAllLittleMixNotIn_2019$Beh=="Descending","Descend",
                                                        ifelse(BudgetAllLittleMixNotIn_2019$Beh=="Swimming","Swim/Cruise while diving",
                                                               ifelse(BudgetAllLittleMixNotIn_2019$Beh=="Hunting","Hunt",
                                                                      ifelse(BudgetAllLittleMixNotIn_2019$Beh=="Ascending","Ascend",as.character(BudgetAllLittleMixNotIn_2019$Beh)))))))


plot3<- ggplot() + theme_bw() +  
  geom_boxplot(aes(y = bud_value, x = Beh, fill = Method), data = BudgetAllLittleMixNotIn_2019)+
  ylab("proportion")+xlab("")+theme(text = element_text(size=15))+ylim(0,1)+
  scale_fill_manual(values=wes_palette(n=2, name="Darjeeling2"),name="")+
  ggtitle("Training from both seasons, prediction on season 2")


#ggarrange(plot1,plot2,plot3,common.legend = TRUE,nrow=3)

BudgetAllLittleMixNotIn_2019_EM<-BudgetAllLittleMixNotIn_2019[which(BudgetAllLittleMixNotIn_2019$Method=="EM"),]
colnames(BudgetAllLittleMixNotIn_2019_EM)[1]<-"bud_value_EM"
BudgetAllLittleMixNotIn_2019_RF<-BudgetAllLittleMixNotIn_2019[which(BudgetAllLittleMixNotIn_2019$Method=="RF"),]
colnames(BudgetAllLittleMixNotIn_2019_RF)[1]<-"bud_value_RF"

BudgetAllLittle_2019_MixNotIn_Comp<-cbind(BudgetAllLittleMixNotIn_2019_EM,BudgetAllLittleMixNotIn_2019_RF[,c("bud_value_RF")])
colnames(BudgetAllLittle_2019_MixNotIn_Comp)[6]<-"bud_value_RF"


ascend<-BudgetAllLittle_2019_MixNotIn_Comp[which(BudgetAllLittle_2019_MixNotIn_Comp$beh=="Ascending"),]

m<-lm(bud_value_EM~bud_value_RF,data=ascend)

plot3a<-ggplot(BudgetAllLittle_2019_MixNotIn_Comp) + 
  geom_point(aes(x=bud_value_RF, y=bud_value_EM,color=ID_Ind), size=pointS,alpha=0.6) +
  # geom_smooth(aes(x=bud_value_RF, y=bud_value_EM),method = "lm", se=TRUE, color="gray35", formula = y ~ x) +
  scale_color_viridis(discrete = TRUE, alpha=0.6,guide=FALSE) +
  geom_abline(slope=1, intercept=0,  color = "gray35",linetype="dashed",size=lineS)+
  # geom_line(aes(x=EE_VeDBA_RF,y = fit), size = 1,color = "gray45")+
  # geom_ribbon(aes(x=EE_VeDBA_RF,ymin = lwr, ymax = upr),  color = "gray45",alpha = .15,linetype="dashed")+
  theme_bw() +theme(text = element_text(size=textS))+
  ylim(0,0.7)+xlim(0,0.7)+
  #  xlab("")+ylab(expression(paste("Energy Expenditure (Kj ",g^-1," trip ", duration^-1,")")))+
  xlab("Budget predicted by RF")+
  ylab("Budget predicted by EM")+ facet_wrap(~Beh)+
  # geom_text(data=eq,aes(x = 0.2, y = 0.6,label=V1), parse = TRUE, inherit.aes=FALSE)+
  theme(legend.text=element_text(size=textS)) + theme(legend.position="top")+
  ggtitle("Training from both seasons, prediction on season 2")

plot3a

############## ############## ############## ############## ############## ############## 
############## ############## ############## ############## ############## ############## 
############## plot all seasons and compare
#### Adelie
############## ############## ############## ############## ############## ############## 
############## ############## ############## ############## ############## ############## 


Accuracy_2018_2019DF <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsAdelie2018_2019_All.csv",header=TRUE)
Accuracy_2018_2019DF$Season<-2
Accuracy_2018_2019DF$Train_Test<-"In Predict"
Accuracy_2018_2019DF$RFType<-"1Season"
summary(Accuracy_2018_2019DF)

Accuracy_2018_2019DF_MixNotIn <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsAdelie2018_2019_MixNotInTraining.csv",header=TRUE)
Accuracy_2018_2019DF_MixNotIn$Season<-2
Accuracy_2018_2019DF_MixNotIn$Train_Test<-"In Predict"
Accuracy_2018_2019DF_MixNotIn$RFType<-"MixSeason"
summary(Accuracy_2018_2019DF_MixNotIn)

Accuracy_2019_2020DF_MixNotIn <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsAdelie2019_2020_MixNotInTraining.csv",header=TRUE)
Accuracy_2019_2020DF_MixNotIn$Season<-1
Accuracy_2019_2020DF_MixNotIn$Train_Test<-"In Predict"
Accuracy_2019_2020DF_MixNotIn$RFType<-"MixSeason"
summary(Accuracy_2019_2020DF_MixNotIn)


Accuracy_2018_2019DF_Ag <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFAdelie2018_2019_All.csv",header=TRUE)
Accuracy_2018DF_MixNotIn_Ag <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFAdelie2018_2019_MixNotInTraining.csv",header=TRUE)
Accuracy_2019DF_MixNotIn_Ag <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFAdelie2019_2020_MixNotInTraining.csv",header=TRUE)

############## ############## ############## ############## ############## ############## 
############## ############## ############## ############## ############## ############## 

BudgetAdelie2018_2019_RF<-Accuracy_2018_2019DF[which(Accuracy_2018_2019DF$Method=="RF"),]
BudgetAdelie2018_2019_RF$ID_Ind<-Accuracy_2018_2019DF_Ag$ID_Ind

Preen_highFlap_L<-as.data.frame(BudgetAdelie2018_2019_RF[,c(1,19)])
Preen_highFlap_L$beh<-"Preen/highFlap_L"
Preen_highFlap_L$Method<-"RF"
colnames(Preen_highFlap_L)[1]<-"bud_value"

Walk<-as.data.frame(BudgetAdelie2018_2019_RF[,c(2,19)])
Walk$beh<-"Walk"
Walk$Method<-"RF"
colnames(Walk)[1]<-"bud_value"

Swim_Porpoise<-as.data.frame(BudgetAdelie2018_2019_RF[,c(3,19)])
Swim_Porpoise$beh<-"Swim/Porpoise"
Swim_Porpoise$Method<-"RF"
colnames(Swim_Porpoise)[1]<-"bud_value"

Preen_highFlap_W<-as.data.frame(BudgetAdelie2018_2019_RF[,c(4,19)])
Preen_highFlap_W$beh<-"Preen/highFlap_W"
Preen_highFlap_W$Method<-"RF"
colnames(Preen_highFlap_W)[1]<-"bud_value"

Rest<-as.data.frame(BudgetAdelie2018_2019_RF[,c(5,19)])
Rest$beh<-"Rest"
Rest$Method<-"RF"
colnames(Rest)[1]<-"bud_value"

Hunting<-as.data.frame(BudgetAdelie2018_2019_RF[,c(6,19)])
Hunting$beh<-"Hunting"
Hunting$Method<-"RF"
colnames(Hunting)[1]<-"bud_value"

Ascending<-as.data.frame(BudgetAdelie2018_2019_RF[,c(7,19)])
Ascending$beh<-"Ascending"
Ascending$Method<-"RF"
colnames(Ascending)[1]<-"bud_value"

Swimming<-as.data.frame(BudgetAdelie2018_2019_RF[,c(8,19)])
Swimming$beh<-"Swimming"
Swimming$Method<-"RF"
colnames(Swimming)[1]<-"bud_value"

Descending<-as.data.frame(BudgetAdelie2018_2019_RF[,c(9,19)])
Descending$beh<-"Descending"
Descending$Method<-"RF"
colnames(Descending)[1]<-"bud_value"

Stand<-as.data.frame(BudgetAdelie2018_2019_RF[,c(10,19)])
Stand$beh<-"Stand"
Stand$Method<-"RF"
colnames(Stand)[1]<-"bud_value"

LieDown<-as.data.frame(BudgetAdelie2018_2019_RF[,c(11,19)])
LieDown$beh<-"LieDown"
LieDown$Method<-"RF"
colnames(LieDown)[1]<-"bud_value"

RFBud<-rbind(Preen_highFlap_L,Walk,Swim_Porpoise,Preen_highFlap_W,Rest,Hunting,Ascending,Swimming,Descending,Stand,LieDown)


BudgetAdelie2018_2019_EM<-Accuracy_2018_2019DF[which(Accuracy_2018_2019DF$Method=="EM"),]
BudgetAdelie2018_2019_EM$ID_Ind<-Accuracy_2018_2019DF_Ag$ID_Ind


Preen_highFlap_L<-as.data.frame(BudgetAdelie2018_2019_EM[,c(1,19)])
Preen_highFlap_L$beh<-"Preen/highFlap_L"
Preen_highFlap_L$Method<-"EM"
colnames(Preen_highFlap_L)[1]<-"bud_value"

Walk<-as.data.frame(BudgetAdelie2018_2019_EM[,c(2,19)])
Walk$beh<-"Walk"
Walk$Method<-"EM"
colnames(Walk)[1]<-"bud_value"

Swim_Porpoise<-as.data.frame(BudgetAdelie2018_2019_EM[,c(3,19)])
Swim_Porpoise$beh<-"Swim/Porpoise"
Swim_Porpoise$Method<-"EM"
colnames(Swim_Porpoise)[1]<-"bud_value"

Preen_highFlap_W<-as.data.frame(BudgetAdelie2018_2019_EM[,c(4,19)])
Preen_highFlap_W$beh<-"Preen/highFlap_W"
Preen_highFlap_W$Method<-"EM"
colnames(Preen_highFlap_W)[1]<-"bud_value"

Rest<-as.data.frame(BudgetAdelie2018_2019_EM[,c(5,19)])
Rest$beh<-"Rest"
Rest$Method<-"EM"
colnames(Rest)[1]<-"bud_value"

Hunting<-as.data.frame(BudgetAdelie2018_2019_EM[,c(6,19)])
Hunting$beh<-"Hunting"
Hunting$Method<-"EM"
colnames(Hunting)[1]<-"bud_value"

Ascending<-as.data.frame(BudgetAdelie2018_2019_EM[,c(7,19)])
Ascending$beh<-"Ascending"
Ascending$Method<-"EM"
colnames(Ascending)[1]<-"bud_value"

Swimming<-as.data.frame(BudgetAdelie2018_2019_EM[,c(8,19)])
Swimming$beh<-"Swimming"
Swimming$Method<-"EM"
colnames(Swimming)[1]<-"bud_value"

Descending<-as.data.frame(BudgetAdelie2018_2019_EM[,c(9,19)])
Descending$beh<-"Descending"
Descending$Method<-"EM"
colnames(Descending)[1]<-"bud_value"

Stand<-as.data.frame(BudgetAdelie2018_2019_EM[,c(10,19)])
Stand$beh<-"Stand"
Stand$Method<-"EM"
colnames(Stand)[1]<-"bud_value"

LieDown<-as.data.frame(BudgetAdelie2018_2019_EM[,c(11,19)])
LieDown$beh<-"LieDown"
LieDown$Method<-"EM"
colnames(LieDown)[1]<-"bud_value"

EMBud<-rbind(Preen_highFlap_L,Walk,Swim_Porpoise,Preen_highFlap_W,Rest,Hunting,Ascending,Swimming,Descending,Stand,LieDown)

BudgetAllAdelie2018_2019<-rbind(RFBud,EMBud)

BudgetAllAdelie2018_2019$Method<-as.factor(BudgetAllAdelie2018_2019$Method)
BudgetAllAdelie2018_2019$beh<-as.factor(BudgetAllAdelie2018_2019$beh)


BudgetAllAdelie2018_2019$Beh<-factor(BudgetAllAdelie2018_2019$beh, levels = c("Walk", "Preen/highFlap_L", "Stand","LieDown","Rest","Preen/highFlap_W","Swim/Porpoise",
                                              "Descending","Swimming","Hunting","Ascending"))


BudgetAllAdelie2018_2019$Beh<-ifelse(BudgetAllAdelie2018_2019$Beh=="Rest","Slow surface swim",
                     ifelse(BudgetAllAdelie2018_2019$Beh=="Preen/highFlap_L","Preen/Flap on land",
                            ifelse(BudgetAllAdelie2018_2019$Beh=="Preen/highFlap_W","Preen/Flap on water",
                                   ifelse(BudgetAllAdelie2018_2019$Beh=="LieDown","Lie Down/Toboggan",
                                          ifelse(BudgetAllAdelie2018_2019$Beh=="Descending","Descend",
                                                 ifelse(BudgetAllAdelie2018_2019$Beh=="Swimming","Swim/Cruise while diving",
                                                        ifelse(BudgetAllAdelie2018_2019$Beh=="Hunting","Hunt",
                                                               ifelse(BudgetAllAdelie2018_2019$Beh=="Ascending","Ascend",as.character(BudgetAllAdelie2018_2019$Beh)))))))))

plot1<- ggplot() + theme_bw() + 
  geom_boxplot(aes(y = bud_value, x = Beh, fill = Method), data = BudgetAllAdelie2018_2019)+
  ylab("proportion")+xlab("")+theme(text = element_text(size=15))+ylim(0,1)+
  scale_fill_manual(values=wes_palette(n=2, name="Darjeeling2"),name="")+
  ggtitle("Training from season 1, prediction on season 2")

BudgetAllAdelie2018_2019_EM<-BudgetAllAdelie2018_2019[which(BudgetAllAdelie2018_2019$Method=="EM"),]
colnames(BudgetAllAdelie2018_2019_EM)[1]<-"bud_value_EM"
BudgetAllAdelie2018_2019_RF<-BudgetAllAdelie2018_2019[which(BudgetAllAdelie2018_2019$Method=="RF"),]
colnames(BudgetAllAdelie2018_2019_RF)[1]<-"bud_value_RF"

BudgetAllAdelie2018_2019_Comp<-cbind(BudgetAllAdelie2018_2019_EM,BudgetAllAdelie2018_2019_RF[,c("bud_value_RF")])
colnames(BudgetAllAdelie2018_2019_Comp)[6]<-"bud_value_RF"


plot1a<-ggplot(BudgetAllAdelie2018_2019_Comp) + 
  geom_point(aes(x=bud_value_RF, y=bud_value_EM,color=ID_Ind), size=pointS,alpha=0.6) +
  #  geom_smooth(aes(x=bud_value_RF, y=bud_value_EM),method = "lm", se=TRUE, color="gray35", formula = y ~ x) +
  scale_color_viridis(discrete = TRUE, alpha=0.6,guide=FALSE) +
  geom_abline(slope=1, intercept=0,  color = "gray35",linetype="dashed",size=lineS)+
  # geom_line(aes(x=EE_VeDBA_RF,y = fit), size = 1,color = "gray45")+
  # geom_ribbon(aes(x=EE_VeDBA_RF,ymin = lwr, ymax = upr),  color = "gray45",alpha = .15,linetype="dashed")+
  theme_bw() +theme(text = element_text(size=textS))+
  ylim(0,0.7)+xlim(0,0.7)+
  #  xlab("")+ylab(expression(paste("Energy Expenditure (Kj ",g^-1," trip ", duration^-1,")")))+
  xlab("Budget predicted by RF")+
  ylab("Budget predicted by EM")+ facet_wrap(~Beh)+
  # geom_text(data=eq,aes(x = 0.2, y = 0.6,label=V1), parse = TRUE, inherit.aes=FALSE)+
  theme(legend.text=element_text(size=textS)) + theme(legend.position="top")+
  ggtitle("Training from season 1, prediction on season 2")

plot1a


############## ############## ############## ############## ############## ############## 
############## ############## ############## ############## ############## ############## 

BudgetAdelie2019_2020DF_MixNotInRF<-Accuracy_2019_2020DF_MixNotIn[which(Accuracy_2019_2020DF_MixNotIn$Method=="RF"),]
BudgetAdelie2019_2020DF_MixNotInRF$ID_Ind<-Accuracy_2019DF_MixNotIn_Ag$ID_Ind


Preen_highFlap_L<-as.data.frame(BudgetAdelie2019_2020DF_MixNotInRF[,c(1,13)])
Preen_highFlap_L$beh<-"Preen/highFlap_L"
Preen_highFlap_L$Method<-"RF"
colnames(Preen_highFlap_L)[1]<-"bud_value"

Walk<-as.data.frame(BudgetAdelie2019_2020DF_MixNotInRF[,c(2,13)])
Walk$beh<-"Walk"
Walk$Method<-"RF"
colnames(Walk)[1]<-"bud_value"

Swim_Porpoise<-as.data.frame(BudgetAdelie2019_2020DF_MixNotInRF[,c(3,13)])
Swim_Porpoise$beh<-"Swim/Porpoise"
Swim_Porpoise$Method<-"RF"
colnames(Swim_Porpoise)[1]<-"bud_value"

Preen_highFlap_W<-as.data.frame(BudgetAdelie2019_2020DF_MixNotInRF[,c(4,13)])
Preen_highFlap_W$beh<-"Preen/highFlap_W"
Preen_highFlap_W$Method<-"RF"
colnames(Preen_highFlap_W)[1]<-"bud_value"

Rest<-as.data.frame(BudgetAdelie2019_2020DF_MixNotInRF[,c(5,13)])
Rest$beh<-"Rest"
Rest$Method<-"RF"
colnames(Rest)[1]<-"bud_value"

Hunting<-as.data.frame(BudgetAdelie2019_2020DF_MixNotInRF[,c(6,13)])
Hunting$beh<-"Hunting"
Hunting$Method<-"RF"
colnames(Hunting)[1]<-"bud_value"

Ascending<-as.data.frame(BudgetAdelie2019_2020DF_MixNotInRF[,c(7,13)])
Ascending$beh<-"Ascending"
Ascending$Method<-"RF"
colnames(Ascending)[1]<-"bud_value"

Swimming<-as.data.frame(BudgetAdelie2019_2020DF_MixNotInRF[,c(8,13)])
Swimming$beh<-"Swimming"
Swimming$Method<-"RF"
colnames(Swimming)[1]<-"bud_value"

Descending<-as.data.frame(BudgetAdelie2019_2020DF_MixNotInRF[,c(9,13)])
Descending$beh<-"Descending"
Descending$Method<-"RF"
colnames(Descending)[1]<-"bud_value"

Stand<-as.data.frame(BudgetAdelie2019_2020DF_MixNotInRF[,c(10,13)])
Stand$beh<-"Stand"
Stand$Method<-"RF"
colnames(Stand)[1]<-"bud_value"

LieDown<-as.data.frame(BudgetAdelie2019_2020DF_MixNotInRF[,c(11,13)])
LieDown$beh<-"LieDown"
LieDown$Method<-"RF"
colnames(LieDown)[1]<-"bud_value"

RFBud<-rbind(Preen_highFlap_L,Walk,Swim_Porpoise,Preen_highFlap_W,Rest,Hunting,Ascending,Swimming,Descending,Stand,LieDown)


BudgetAdelie2019_2020MixNotIn_EM<-Accuracy_2019_2020DF_MixNotIn[which(Accuracy_2019_2020DF_MixNotIn$Method=="EM"),]
BudgetAdelie2019_2020MixNotIn_EM$ID_Ind<-Accuracy_2019DF_MixNotIn_Ag$ID_Ind


Preen_highFlap_L<-as.data.frame(BudgetAdelie2019_2020MixNotIn_EM[,c(1,13)])
Preen_highFlap_L$beh<-"Preen/highFlap_L"
Preen_highFlap_L$Method<-"EM"
colnames(Preen_highFlap_L)[1]<-"bud_value"

Walk<-as.data.frame(BudgetAdelie2019_2020MixNotIn_EM[,c(2,13)])
Walk$beh<-"Walk"
Walk$Method<-"EM"
colnames(Walk)[1]<-"bud_value"

Swim_Porpoise<-as.data.frame(BudgetAdelie2019_2020MixNotIn_EM[,c(3,13)])
Swim_Porpoise$beh<-"Swim/Porpoise"
Swim_Porpoise$Method<-"EM"
colnames(Swim_Porpoise)[1]<-"bud_value"

Preen_highFlap_W<-as.data.frame(BudgetAdelie2019_2020MixNotIn_EM[,c(4,13)])
Preen_highFlap_W$beh<-"Preen/highFlap_W"
Preen_highFlap_W$Method<-"EM"
colnames(Preen_highFlap_W)[1]<-"bud_value"

Rest<-as.data.frame(BudgetAdelie2019_2020MixNotIn_EM[,c(5,13)])
Rest$beh<-"Rest"
Rest$Method<-"EM"
colnames(Rest)[1]<-"bud_value"

Hunting<-as.data.frame(BudgetAdelie2019_2020MixNotIn_EM[,c(6,13)])
Hunting$beh<-"Hunting"
Hunting$Method<-"EM"
colnames(Hunting)[1]<-"bud_value"

Ascending<-as.data.frame(BudgetAdelie2019_2020MixNotIn_EM[,c(7,13)])
Ascending$beh<-"Ascending"
Ascending$Method<-"EM"
colnames(Ascending)[1]<-"bud_value"

Swimming<-as.data.frame(BudgetAdelie2019_2020MixNotIn_EM[,c(8,13)])
Swimming$beh<-"Swimming"
Swimming$Method<-"EM"
colnames(Swimming)[1]<-"bud_value"

Descending<-as.data.frame(BudgetAdelie2019_2020MixNotIn_EM[,c(9,13)])
Descending$beh<-"Descending"
Descending$Method<-"EM"
colnames(Descending)[1]<-"bud_value"

Stand<-as.data.frame(BudgetAdelie2019_2020MixNotIn_EM[,c(10,13)])
Stand$beh<-"Stand"
Stand$Method<-"EM"
colnames(Stand)[1]<-"bud_value"

LieDown<-as.data.frame(BudgetAdelie2019_2020MixNotIn_EM[,c(11,13)])
LieDown$beh<-"LieDown"
LieDown$Method<-"EM"
colnames(LieDown)[1]<-"bud_value"

EMBud<-rbind(Preen_highFlap_L,Walk,Swim_Porpoise,Preen_highFlap_W,Rest,Hunting,Ascending,Swimming,Descending,Stand,LieDown)

BudgetAllAdelie2019_2020MixNotIn<-rbind(RFBud,EMBud)

BudgetAllAdelie2019_2020MixNotIn$Method<-as.factor(BudgetAllAdelie2019_2020MixNotIn$Method)
BudgetAllAdelie2019_2020MixNotIn$beh<-as.factor(BudgetAllAdelie2019_2020MixNotIn$beh)


BudgetAllAdelie2019_2020MixNotIn$Beh<-factor(BudgetAllAdelie2019_2020MixNotIn$beh, levels = c("Walk", "Preen/highFlap_L", "Stand","LieDown","Rest","Preen/highFlap_W","Swim/Porpoise",
                                                                              "Descending","Swimming","Hunting","Ascending"))


BudgetAllAdelie2019_2020MixNotIn$Beh<-ifelse(BudgetAllAdelie2019_2020MixNotIn$Beh=="Rest","Slow surface swim",
                                     ifelse(BudgetAllAdelie2019_2020MixNotIn$Beh=="Preen/highFlap_L","Preen/Flap on land",
                                            ifelse(BudgetAllAdelie2019_2020MixNotIn$Beh=="Preen/highFlap_W","Preen/Flap on water",
                                                   ifelse(BudgetAllAdelie2019_2020MixNotIn$Beh=="LieDown","Lie Down/Toboggan",
                                                          ifelse(BudgetAllAdelie2019_2020MixNotIn$Beh=="Descending","Descend",
                                                                 ifelse(BudgetAllAdelie2019_2020MixNotIn$Beh=="Swimming","Swim/Cruise while diving",
                                                                        ifelse(BudgetAllAdelie2019_2020MixNotIn$Beh=="Hunting","Hunt",
                                                                               ifelse(BudgetAllAdelie2019_2020MixNotIn$Beh=="Ascending","Ascend",as.character(BudgetAllAdelie2019_2020MixNotIn$Beh)))))))))

plot2<-ggplot() + theme_bw() +  
  geom_boxplot(aes(y = bud_value, x = Beh, fill = Method), data = BudgetAllAdelie2019_2020MixNotIn)+
  ylab("proportion")+xlab("")+theme(text = element_text(size=15))+ylim(0,1)+
  scale_fill_manual(values=wes_palette(n=2, name="Darjeeling2"),name="")+ 
  ggtitle("Training from both seasons, prediction on season 1")

BudgetAllAdelie2019_2020MixNotIn_EM<-BudgetAllAdelie2019_2020MixNotIn[which(BudgetAllAdelie2019_2020MixNotIn$Method=="EM"),]
colnames(BudgetAllAdelie2019_2020MixNotIn_EM)[1]<-"bud_value_EM"
BudgetAllAdelie2019_2020MixNotIn_RF<-BudgetAllAdelie2019_2020MixNotIn[which(BudgetAllAdelie2019_2020MixNotIn$Method=="RF"),]
colnames(BudgetAllAdelie2019_2020MixNotIn_RF)[1]<-"bud_value_RF"

BudgetAllAdelie2019_2020MixNotIn_Comp<-cbind(BudgetAllAdelie2019_2020MixNotIn_EM,BudgetAllAdelie2019_2020MixNotIn_RF[,c("bud_value_RF")])
colnames(BudgetAllAdelie2019_2020MixNotIn_Comp)[6]<-"bud_value_RF"


plot2a<-ggplot(BudgetAllAdelie2019_2020MixNotIn_Comp) + 
  geom_point(aes(x=bud_value_RF, y=bud_value_EM,color=ID_Ind), size=pointS,alpha=0.6) +
  #  geom_smooth(aes(x=bud_value_RF, y=bud_value_EM),method = "lm", se=TRUE, color="gray35", formula = y ~ x) +
  scale_color_viridis(discrete = TRUE, alpha=0.6,guide=FALSE) +
  geom_abline(slope=1, intercept=0,  color = "gray35",linetype="dashed",size=lineS)+
  # geom_line(aes(x=EE_VeDBA_RF,y = fit), size = 1,color = "gray45")+
  # geom_ribbon(aes(x=EE_VeDBA_RF,ymin = lwr, ymax = upr),  color = "gray45",alpha = .15,linetype="dashed")+
  theme_bw() +theme(text = element_text(size=textS))+
  ylim(0,0.7)+xlim(0,0.7)+
  #  xlab("")+ylab(expression(paste("Energy Expenditure (Kj ",g^-1," trip ", duration^-1,")")))+
  xlab("Budget predicted by RF")+
  ylab("Budget predicted by EM")+ facet_wrap(~Beh)+
  # geom_text(data=eq,aes(x = 0.2, y = 0.6,label=V1), parse = TRUE, inherit.aes=FALSE)+
  theme(legend.text=element_text(size=textS)) + theme(legend.position="top")+ 
  ggtitle("Training from both seasons, prediction on season 1")

plot2a


############## ############## ############## ############## ############## ############## 
############## ############## ############## ############## ############## ############## 


BudgetAdelie2018_2019_MixNotInRF<-Accuracy_2018_2019DF_MixNotIn[which(Accuracy_2018_2019DF_MixNotIn$Method=="RF"),]
BudgetAdelie2018_2019_MixNotInRF$ID_Ind<-Accuracy_2018DF_MixNotIn_Ag$ID_Ind


Preen_highFlap_L<-as.data.frame(BudgetAdelie2018_2019_MixNotInRF[,c(1,13)])
Preen_highFlap_L$beh<-"Preen/highFlap_L"
Preen_highFlap_L$Method<-"RF"
colnames(Preen_highFlap_L)[1]<-"bud_value"

Walk<-as.data.frame(BudgetAdelie2018_2019_MixNotInRF[,c(2,13)])
Walk$beh<-"Walk"
Walk$Method<-"RF"
colnames(Walk)[1]<-"bud_value"

Swim_Porpoise<-as.data.frame(BudgetAdelie2018_2019_MixNotInRF[,c(3,13)])
Swim_Porpoise$beh<-"Swim/Porpoise"
Swim_Porpoise$Method<-"RF"
colnames(Swim_Porpoise)[1]<-"bud_value"

Preen_highFlap_W<-as.data.frame(BudgetAdelie2018_2019_MixNotInRF[,c(4,13)])
Preen_highFlap_W$beh<-"Preen/highFlap_W"
Preen_highFlap_W$Method<-"RF"
colnames(Preen_highFlap_W)[1]<-"bud_value"

Rest<-as.data.frame(BudgetAdelie2018_2019_MixNotInRF[,c(5,13)])
Rest$beh<-"Rest"
Rest$Method<-"RF"
colnames(Rest)[1]<-"bud_value"

Hunting<-as.data.frame(BudgetAdelie2018_2019_MixNotInRF[,c(6,13)])
Hunting$beh<-"Hunting"
Hunting$Method<-"RF"
colnames(Hunting)[1]<-"bud_value"

Ascending<-as.data.frame(BudgetAdelie2018_2019_MixNotInRF[,c(7,13)])
Ascending$beh<-"Ascending"
Ascending$Method<-"RF"
colnames(Ascending)[1]<-"bud_value"

Swimming<-as.data.frame(BudgetAdelie2018_2019_MixNotInRF[,c(8,13)])
Swimming$beh<-"Swimming"
Swimming$Method<-"RF"
colnames(Swimming)[1]<-"bud_value"

Descending<-as.data.frame(BudgetAdelie2018_2019_MixNotInRF[,c(9,13)])
Descending$beh<-"Descending"
Descending$Method<-"RF"
colnames(Descending)[1]<-"bud_value"

Stand<-as.data.frame(BudgetAdelie2018_2019_MixNotInRF[,c(10,13)])
Stand$beh<-"Stand"
Stand$Method<-"RF"
colnames(Stand)[1]<-"bud_value"

LieDown<-as.data.frame(BudgetAdelie2018_2019_MixNotInRF[,c(11,13)])
LieDown$beh<-"LieDown"
LieDown$Method<-"RF"
colnames(LieDown)[1]<-"bud_value"

RFBud<-rbind(Preen_highFlap_L,Walk,Swim_Porpoise,Preen_highFlap_W,Rest,Hunting,Ascending,Swimming,Descending,Stand,LieDown)


BudgetAdelie2018_2019MixNotIn_EM<-Accuracy_2018_2019DF_MixNotIn[which(Accuracy_2018_2019DF_MixNotIn$Method=="EM"),]
BudgetAdelie2018_2019_MixNotInRF$ID_Ind<-Accuracy_2018DF_MixNotIn_Ag$ID_Ind


Preen_highFlap_L<-as.data.frame(BudgetAdelie2018_2019MixNotIn_EM[,c(1,13)])
Preen_highFlap_L$beh<-"Preen/highFlap_L"
Preen_highFlap_L$Method<-"EM"
colnames(Preen_highFlap_L)[1]<-"bud_value"

Walk<-as.data.frame(BudgetAdelie2018_2019MixNotIn_EM[,c(2,13)])
Walk$beh<-"Walk"
Walk$Method<-"EM"
colnames(Walk)[1]<-"bud_value"

Swim_Porpoise<-as.data.frame(BudgetAdelie2018_2019MixNotIn_EM[,c(3,13)])
Swim_Porpoise$beh<-"Swim/Porpoise"
Swim_Porpoise$Method<-"EM"
colnames(Swim_Porpoise)[1]<-"bud_value"

Preen_highFlap_W<-as.data.frame(BudgetAdelie2018_2019MixNotIn_EM[,c(4,13)])
Preen_highFlap_W$beh<-"Preen/highFlap_W"
Preen_highFlap_W$Method<-"EM"
colnames(Preen_highFlap_W)[1]<-"bud_value"

Rest<-as.data.frame(BudgetAdelie2018_2019MixNotIn_EM[,c(5,13)])
Rest$beh<-"Rest"
Rest$Method<-"EM"
colnames(Rest)[1]<-"bud_value"

Hunting<-as.data.frame(BudgetAdelie2018_2019MixNotIn_EM[,c(6,13)])
Hunting$beh<-"Hunting"
Hunting$Method<-"EM"
colnames(Hunting)[1]<-"bud_value"

Ascending<-as.data.frame(BudgetAdelie2018_2019MixNotIn_EM[,c(7,13)])
Ascending$beh<-"Ascending"
Ascending$Method<-"EM"
colnames(Ascending)[1]<-"bud_value"

Swimming<-as.data.frame(BudgetAdelie2018_2019MixNotIn_EM[,c(8,13)])
Swimming$beh<-"Swimming"
Swimming$Method<-"EM"
colnames(Swimming)[1]<-"bud_value"

Descending<-as.data.frame(BudgetAdelie2018_2019MixNotIn_EM[,c(9,13)])
Descending$beh<-"Descending"
Descending$Method<-"EM"
colnames(Descending)[1]<-"bud_value"

Stand<-as.data.frame(BudgetAdelie2018_2019MixNotIn_EM[,c(10,13)])
Stand$beh<-"Stand"
Stand$Method<-"EM"
colnames(Stand)[1]<-"bud_value"

LieDown<-as.data.frame(BudgetAdelie2018_2019MixNotIn_EM[,c(11,13)])
LieDown$beh<-"LieDown"
LieDown$Method<-"EM"
colnames(LieDown)[1]<-"bud_value"

EMBud<-rbind(Preen_highFlap_L,Walk,Swim_Porpoise,Preen_highFlap_W,Rest,Hunting,Ascending,Swimming,Descending,Stand,LieDown)

BudgetAllAdelie2018_2019MixNotIn<-rbind(RFBud,EMBud)

BudgetAllAdelie2018_2019MixNotIn$Method<-as.factor(BudgetAllAdelie2018_2019MixNotIn$Method)
BudgetAllAdelie2018_2019MixNotIn$beh<-as.factor(BudgetAllAdelie2018_2019MixNotIn$beh)


BudgetAllAdelie2018_2019MixNotIn$Beh<-factor(BudgetAllAdelie2018_2019MixNotIn$beh, levels = c("Walk", "Preen/highFlap_L", "Stand","LieDown","Rest","Preen/highFlap_W","Swim/Porpoise",
                                                                                              "Descending","Swimming","Hunting","Ascending"))


BudgetAllAdelie2018_2019MixNotIn$Beh<-ifelse(BudgetAllAdelie2018_2019MixNotIn$Beh=="Rest","Slow surface swim",
                                             ifelse(BudgetAllAdelie2018_2019MixNotIn$Beh=="Preen/highFlap_L","Preen/Flap on land",
                                                    ifelse(BudgetAllAdelie2018_2019MixNotIn$Beh=="Preen/highFlap_W","Preen/Flap on water",
                                                           ifelse(BudgetAllAdelie2018_2019MixNotIn$Beh=="LieDown","Lie Down/Toboggan",
                                                                  ifelse(BudgetAllAdelie2018_2019MixNotIn$Beh=="Descending","Descend",
                                                                         ifelse(BudgetAllAdelie2018_2019MixNotIn$Beh=="Swimming","Swim/Cruise while diving",
                                                                                ifelse(BudgetAllAdelie2018_2019MixNotIn$Beh=="Hunting","Hunt",
                                                                                       ifelse(BudgetAllAdelie2018_2019MixNotIn$Beh=="Ascending","Ascend",as.character(BudgetAllAdelie2018_2019MixNotIn$Beh)))))))))


plot3<- ggplot() + theme_bw() +  
  geom_boxplot(aes(y = bud_value, x = Beh, fill = Method), data = BudgetAllAdelie2018_2019MixNotIn)+
  ylab("proportion")+xlab("")+theme(text = element_text(size=15))+ylim(0,1)+
  scale_fill_manual(values=wes_palette(n=2, name="Darjeeling2"),name="")+
  ggtitle("Training from both seasons, prediction on season 2")


#ggarrange(plot1,plot2,plot3,common.legend = TRUE,nrow=3)

BudgetAllAdelie2018_2019MixNotIn_EM<-BudgetAllAdelie2018_2019MixNotIn[which(BudgetAllAdelie2018_2019MixNotIn$Method=="EM"),]
colnames(BudgetAllAdelie2018_2019MixNotIn_EM)[1]<-"bud_value_EM"
BudgetAllAdelie2018_2019MixNotIn_RF<-BudgetAllAdelie2018_2019MixNotIn[which(BudgetAllAdelie2018_2019MixNotIn$Method=="RF"),]
colnames(BudgetAllAdelie2018_2019MixNotIn_RF)[1]<-"bud_value_RF"

BudgetAllAdelie2018_2019MixNotIn_Comp<-cbind(BudgetAllAdelie2018_2019MixNotIn_EM,BudgetAllAdelie2018_2019MixNotIn_RF[,c("bud_value_RF")])
colnames(BudgetAllAdelie2018_2019MixNotIn_Comp)[6]<-"bud_value_RF"

plot3a<-ggplot(BudgetAllAdelie2018_2019MixNotIn_Comp) + 
  geom_point(aes(x=bud_value_RF, y=bud_value_EM,color=ID_Ind), size=pointS,alpha=0.6) +
  #  geom_smooth(aes(x=bud_value_RF, y=bud_value_EM),method = "lm", se=TRUE, color="gray35", formula = y ~ x) +
  scale_color_viridis(discrete = TRUE, alpha=0.6,guide=FALSE) +
  geom_abline(slope=1, intercept=0,  color = "gray35",linetype="dashed",size=lineS)+
  # geom_line(aes(x=EE_VeDBA_RF,y = fit), size = 1,color = "gray45")+
  # geom_ribbon(aes(x=EE_VeDBA_RF,ymin = lwr, ymax = upr),  color = "gray45",alpha = .15,linetype="dashed")+
  theme_bw() +theme(text = element_text(size=textS))+
  ylim(0,0.7)+xlim(0,0.7)+
  #  xlab("")+ylab(expression(paste("Energy Expenditure (Kj ",g^-1," trip ", duration^-1,")")))+
  xlab("Budget predicted by RF")+
  ylab("Budget predicted by EM")+ facet_wrap(~Beh)+
  # geom_text(data=eq,aes(x = 0.2, y = 0.6,label=V1), parse = TRUE, inherit.aes=FALSE)+
  theme(legend.text=element_text(size=textS)) + theme(legend.position="top")+
  ggtitle("Training from both seasons, prediction on season 2")

plot3a



