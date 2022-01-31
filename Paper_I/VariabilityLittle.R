#install.packages("rptR")
library(rptR)
library(data.table)
library(ggplot2)
library(ggpubr)
library(lme4)

########### repeatability within the training set used for RF
#load training for RF
trainingDF<-fread("/Users/mariannachimienti/MarieCurie/TrainingLittle2020.csv",header=TRUE)
head(trainingDF)
str(trainingDF)
dim(trainingDF)
trainingDF$TagID_2<-substr(trainingDF$ID_Ind,1,8)


# grouped boxplot
unique(trainingDF$StatesName)

ggplot(trainingDF[which(trainingDF$StatesNames=="Swimming")], aes(x=StatesNames, y=PitchDiff, fill=ID_Ind)) +
  geom_boxplot()+ theme(legend.position = "none")+ylim(-100,100)

ggplot(trainingDF[which(trainingDF$StatesNames=="Swimming")], aes(x=StatesNames, y=VeDBA, fill=ID_Ind)) +
  geom_boxplot()+ theme(legend.position = "none")+ylim(0,10)


# uniqueInd<- unique(trainingDF$TagID_2)
# countTrips<-data.frame(NA,NA)
# 
# for(i in 1:length(uniqueInd)){
# 
#   countTrips[i,1]<-length(unique(trainingDF$ID_Ind[which(trainingDF$TagID_2==uniqueInd[i])]))
#   countTrips[i,2]<-uniqueInd[i]
# }
# 
# colnames(countTrips)<-c("nTrips","TagID_2")
# head(countTrips)
# 
# singleTrip<-countTrips$TagID_2[which(countTrips$nTrips==1)]
# trainingDFSub<-trainingDF[-which(trainingDF$TagID_2 %in% singleTrip)] ### remove IDs with single trips
# length(unique(trainingDFSub$ID_Ind))
# length(unique(trainingDFSub$TagID_2))
# 
# 
StatesNames<-unique(trainingDF$StatesNames)
R_Scores_Pitch<-data.frame(NA,NA,NA)
R_Scores_VeDBA<-data.frame(NA,NA,NA)

count<-1
##################################################################
###look for each behaviour on Pitch and Vedba variable #########
for (names in 1:length(StatesNames)){
  
  idName<- StatesNames[names]
  trainingDF_Des<-trainingDF[which(trainingDF$StatesNames == idName),]
  print(head(trainingDF_Des))
  # two random effects, estimation of variance (instead repeatability)
  #a low repeatability (near 0) reflects  either a high within-individual variation or a low between-individual variation.
  #A high repeatability corresponds to a low within-individual variance.
  
  
  R_est <- rptGaussian(formula = Pitch  ~ (1|TagID_2),  
                       grname= c("TagID_2"),
                       data=trainingDF_Des, nboot=1000 ,npermut = 0)
  
  # R_est
  # summary(R_est)
  # #plot(R_est, cex.main = 1, grname = "ID_Ind", type = "boot", cex.main = 0.8)
  # plot(R_est, cex.main = 1, grname = "TagID_2",type = "boot", cex.main = 0.8)
  
  R_Scores_Pitch[count,1]<-as.numeric(R_est$R)
  R_Scores_Pitch[count,2]<-as.numeric(as.numeric(R_est$se))
  R_Scores_Pitch[count,3]<-StatesNames[names]
  
  # two random effects, estimation of variance (instead repeatability)
  R_estVeDBA <- rptGaussian(formula = VeDBA ~  (1|TagID_2), 
                            grname= c("TagID_2"),
                            data=trainingDF_Des, nboot=1000, npermut=0)
  
  # plot(R_estVeDBA, cex.main = 1, grname = "ID_Ind", type = "boot", cex.main = 0.8)
  # plot(R_estVeDBA, cex.main = 1, grname = "TagID_2",type = "boot", cex.main = 0.8)
  
  R_Scores_VeDBA[count,1]<-as.numeric(R_estVeDBA$R)
  R_Scores_VeDBA[count,2]<-as.numeric(as.numeric(R_estVeDBA$se))
  R_Scores_VeDBA[count,3]<-StatesNames[names]
  count<-count+1
}


colnames(R_Scores_Pitch)<-c("mean","se","Beh")
R_Scores_Pitch$var<-"Pitch"
R_Scores_Pitch$DF<-"TrainingLittle_2020"

colnames(R_Scores_VeDBA)<-c("mean","se","Beh")
R_Scores_VeDBA$var<-"VeDBA"
R_Scores_VeDBA$DF<-"TrainingLittle_2020"


RScores_all<-rbind(R_Scores_VeDBA,R_Scores_Pitch)
RScores_all


write.csv(RScores_all, "/Users/mariannachimienti/MarieCurie/RF_Results/RScores_Little_2020.csv", row.names = FALSE)

###################### general boxplot
lineS<-0.7
pointS<-6
textS<-20

trainingDF$Beh<-factor(trainingDF$StatesNames, levels = c("Rest","Preen/highFlap_W","Swim/Porpoise","Still",
                                                          "Descending","Swimming","Hunting","Ascending"))

trainingDF$Beh<-ifelse(trainingDF$Beh=="Rest","Slow surface swim",
                       ifelse(trainingDF$Beh=="Preen/highFlap_W","Preen/Flap on water",
                              ifelse(trainingDF$Beh=="Descending","Descend",
                                     ifelse(trainingDF$Beh=="Swimming","Swim/Cruise while diving",
                                            ifelse(trainingDF$Beh=="Hunting","Hunt",
                                                   ifelse(trainingDF$Beh=="Ascending","Ascend",as.character(trainingDF$Beh)))))))


plot1<- ggplot(trainingDF, aes(x=Beh, y=PitchDiff, fill=Beh)) + theme_bw() + 
  geom_boxplot(color="gray35",outlier.colour = "gray45",outlier.size = pointS)+ theme(legend.position = "none")+ylim(-110,110)+
  scale_fill_manual(values=wes_palette(n=11, name="Darjeeling2", type = "continuous"),name="")+
  ylab("Pitch (degrees)")+xlab("")+theme(text = element_text(size=textS))


#plot1


plot2<- ggplot(trainingDF, aes(x=Beh, y=VeDBA, fill=Beh)) + theme_bw() + 
  geom_boxplot(color="gray35",outlier.colour = "gray45",outlier.size = pointS)+ theme(legend.position = "none")+ylim(0,10)+
  scale_fill_manual(values=wes_palette(n=11, name="Darjeeling2", type = "continuous"),name="")+
  ylab("VeDBA (g)")+xlab("")+theme(text = element_text(size=textS))


ggarrange(plot1,plot2,nrow=2)


###############################################################################################################

########### repeatability within the training set used for RF
#load training for RF
trainingDF<-fread("/Users/mariannachimienti/MarieCurie/TrainingLittle_MixSeason.csv",header=TRUE)
head(trainingDF)
str(trainingDF)
dim(trainingDF)
trainingDF$TagID_2<-substr(trainingDF$ID_Ind,1,8)

# grouped boxplot
unique(trainingDF$StatesName)

ggplot(trainingDF[which(trainingDF$StatesNames=="Swim/Porpoise")], aes(x=StatesNames, y=PitchDiff, fill=ID_Ind)) +
  geom_boxplot()+ theme(legend.position = "none")+ylim(-100,100)

ggplot(trainingDF[which(trainingDF$StatesNames=="Swim/Porpoise")], aes(x=StatesNames, y=VeDBA, fill=ID_Ind)) +
  geom_boxplot()+ theme(legend.position = "none")+ylim(0,10)

# uniqueInd<- unique(trainingDF$TagID_2)
# countTrips<-data.frame(NA,NA)
# 
# for(i in 1:length(uniqueInd)){
# 
#   countTrips[i,1]<-length(unique(trainingDF$ID_Ind[which(trainingDF$TagID_2==uniqueInd[i])]))
#   countTrips[i,2]<-uniqueInd[i]
# }
# 
# colnames(countTrips)<-c("nTrips","TagID_2")
# head(countTrips)
# 
# singleTrip<-countTrips$TagID_2[which(countTrips$nTrips==1)]
# trainingDFSub<-trainingDF[-which(trainingDF$TagID_2 %in% singleTrip)] ### remove IDs with single trips
# length(unique(trainingDFSub$ID_Ind))
# length(unique(trainingDFSub$TagID_2))
# 
# 
StatesNames<-unique(trainingDF$StatesNames)
R_Scores_Pitch<-data.frame(NA,NA,NA)
R_Scores_VeDBA<-data.frame(NA,NA,NA)

count<-1
##################################################################
###look for each behaviour on Pitch and Vedba variable #########
for (names in 1:length(StatesNames)){
  
  idName<- StatesNames[names]
  trainingDF_Des<-trainingDF[which(trainingDF$StatesNames == idName),]
  print(head(trainingDF_Des))
  # two random effects, estimation of variance (instead repeatability)
  #a low repeatability (near 0) reflects  either a high within-individual variation or a low between-individual variation.
  #A high repeatability corresponds to a low within-individual variance.
  
  
  R_est <- rptGaussian(formula = Pitch  ~ (1|TagID_2),  
                       grname= c("TagID_2"),
                       data=trainingDF_Des, nboot=1000 ,npermut = 0)
  
  # R_est
  # summary(R_est)
  # #plot(R_est, cex.main = 1, grname = "ID_Ind", type = "boot", cex.main = 0.8)
  # plot(R_est, cex.main = 1, grname = "TagID_2",type = "boot", cex.main = 0.8)
  
  R_Scores_Pitch[count,1]<-as.numeric(R_est$R)
  R_Scores_Pitch[count,2]<-as.numeric(as.numeric(R_est$se))
  R_Scores_Pitch[count,3]<-StatesNames[names]
  
  # two random effects, estimation of variance (instead repeatability)
  R_estVeDBA <- rptGaussian(formula = VeDBA ~  (1|TagID_2), 
                            grname= c("TagID_2"),
                            data=trainingDF_Des, nboot=1000, npermut=0)
  
  # plot(R_estVeDBA, cex.main = 1, grname = "ID_Ind", type = "boot", cex.main = 0.8)
  # plot(R_estVeDBA, cex.main = 1, grname = "TagID_2",type = "boot", cex.main = 0.8)
  
  R_Scores_VeDBA[count,1]<-as.numeric(R_estVeDBA$R)
  R_Scores_VeDBA[count,2]<-as.numeric(as.numeric(R_estVeDBA$se))
  R_Scores_VeDBA[count,3]<-StatesNames[names]
  count<-count+1
}


colnames(R_Scores_Pitch)<-c("mean","se","Beh")
R_Scores_Pitch$var<-"Pitch"
R_Scores_Pitch$DF<-"Little_MixSeason"

colnames(R_Scores_VeDBA)<-c("mean","se","Beh")
R_Scores_VeDBA$var<-"VeDBA"
R_Scores_VeDBA$DF<-"Little_MixSeason"


RScores_all<-rbind(R_Scores_VeDBA,R_Scores_Pitch)
RScores_all


write.csv(RScores_all, "/Users/mariannachimienti/MarieCurie/RF_Results/RScores_Little_MixSeason.csv", row.names = FALSE)





############################################################################
############################################################################
############################################################################
# plot together with Adelie
library("wesanderson")
library(ggplot2)
library(ggpubr)

lineS<-0.7
pointS<-6
textS<-25


Little_MixSeason<-fread("/Users/mariannachimienti/MarieCurie/RF_Results/RScores_Little_MixSeason.csv",header=TRUE)
####beh Still1 had a problem in the Mix dataset for VeDBA~maybe remove 
#Little_MixSeason<-Little_MixSeason[-which(Little_MixSeason$Beh=="Still1"),]

Little_2020<-fread("/Users/mariannachimienti/MarieCurie/RF_Results/RScores_Little_2020.csv",header=TRUE)
####beh Still1 had a problem in Little 2020 as only 1 animal had that beh


Little_MixSeason$Species<-"Little"
Little_2020$Species<-"Little"
LittleDF<-rbind(Little_MixSeason,Little_2020)

LittleDF$Env<-NA
LittleDF$Env<-ifelse(LittleDF$Beh=="Rest"|LittleDF$Beh=="Preen/highFlap_W"|LittleDF$Beh=="Swim/Porpoise"|LittleDF$Beh=="Still",
                     "Sea surface", "Diving")

LittleDF$Beh<-as.factor(LittleDF$Beh)
LittleDF$var<-as.factor(LittleDF$var)
LittleDF$DF<-as.factor(LittleDF$DF)
LittleDF$Species<-as.factor(LittleDF$Species)
LittleDF$Env<-as.factor(LittleDF$Env)
####beh Still1 had a problem in the Mix dataset ~maybe remove 

LittleDF$Beh<-factor(LittleDF$Beh, levels = c("Rest","Preen/highFlap_W","Swim/Porpoise","Still",
                                              "Descending","Swimming","Hunting","Ascending"))

LittleDF$Beh<-ifelse(LittleDF$Beh=="Rest","Slow surface swim",
                     ifelse(LittleDF$Beh=="Preen/highFlap_W","Preen/Flap on water",
                            ifelse(LittleDF$Beh=="Descending","Descend",
                                   ifelse(LittleDF$Beh=="Swimming","Swim/Cruise while diving",
                                          ifelse(LittleDF$Beh=="Hunting","Hunt",
                                                 ifelse(LittleDF$Beh=="Ascending","Ascend",as.character(LittleDF$Beh)))))))


LittleDF$group<-paste(LittleDF$var,LittleDF$DF)
LittleDF$group<-as.factor(LittleDF$group)
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.5) # move them .05 to the left and right

plot1<-ggplot(LittleDF, aes(x=Beh, y=mean,fill=var,shape=DF, group=group)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black",  width=.5,size=lineS, position=pd) +
  geom_point(position=pd, size=pointS)+
  theme_bw() +theme(text = element_text(size=textS))+ylim(0,1)+
  scale_fill_manual(name="",values=wes_palette(name="Darjeeling2")[2:3])+
  scale_shape_manual(name="",values=c(24,22),labels = c("Training from both seasons", "Training from season one only"))+
  xlab("")+ylab("Repeatability")+ facet_wrap(~Env,scales="free_x")+ggtitle("Little penguin")+
  guides(fill = guide_legend(override.aes = list(shape = 21)),
         shape = guide_legend(override.aes = list(fill = "black")))+ theme(legend.text=element_text(size=textS))

plot1




Adelie_MixSeason<-fread("/Users/mariannachimienti/MarieCurie/RF_Results/RScores_Adelie_MixSeason.csv",header=TRUE)
Adelie2019_2020<-fread("/Users/mariannachimienti/MarieCurie/RF_Results/RScores_Adelie2019_2020.csv",header=TRUE)


Adelie_MixSeason$Species<-"Adelie"
Adelie2019_2020$Species<-"Adelie"
AdelieDF<-rbind(Adelie_MixSeason,Adelie2019_2020)
AdelieDF$Env<-NA
AdelieDF$Env<-ifelse(AdelieDF$Beh=="Walk"|AdelieDF$Beh=="Preen/highFlap_L"|AdelieDF$Beh=="Stand"|AdelieDF$Beh=="LieDown","Land/Ice",
                     ifelse(AdelieDF$Beh=="Rest"|AdelieDF$Beh=="Preen/highFlap_W"|AdelieDF$Beh=="Swim/Porpoise","Sea surface", "Diving"))

AdelieDF$Beh<-as.factor(AdelieDF$Beh)
AdelieDF$var<-as.factor(AdelieDF$var)
AdelieDF$DF<-as.factor(AdelieDF$DF)
AdelieDF$Species<-as.factor(AdelieDF$Species)
AdelieDF$Env<-as.factor(AdelieDF$Env)


AdelieDF$Beh<-factor(AdelieDF$Beh, levels = c("Walk", "Preen/highFlap_L", "Stand","LieDown","Rest","Preen/highFlap_W","Swim/Porpoise",
                                              "Descending","Swimming","Hunting","Ascending"))


AdelieDF$Beh<-ifelse(AdelieDF$Beh=="Rest","Slow surface swim",
                     ifelse(AdelieDF$Beh=="Preen/highFlap_L","Preen/Flap on land",
                     ifelse(AdelieDF$Beh=="Preen/highFlap_W","Preen/Flap on water",
                            ifelse(AdelieDF$Beh=="LieDown","Lie Down/Toboggan",
                            ifelse(AdelieDF$Beh=="Descending","Descend",
                                   ifelse(AdelieDF$Beh=="Swimming","Swim/Cruise while diving",
                                          ifelse(AdelieDF$Beh=="Hunting","Hunt",
                                                 ifelse(AdelieDF$Beh=="Ascending","Ascend",as.character(AdelieDF$Beh)))))))))

AdelieDF$group<-paste(AdelieDF$var,AdelieDF$DF)
AdelieDF$group<-as.factor(AdelieDF$group)
# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.5) # move them .05 to the left and right

plot2<-ggplot(AdelieDF, aes(x=Beh, y=mean,fill=var,shape=DF, group=group)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), colour="black", width=.5,size=lineS, position=pd) +
  geom_point(position=pd, size=pointS)+
  theme_bw() +theme(text = element_text(size=textS))+ylim(0,1)+
  scale_fill_manual(name="",values=wes_palette(name="Darjeeling2")[2:3])+
  scale_shape_manual(name="",values=c(24,22),labels = c("Training from both seasons", "Training from season one only"))+
  xlab("")+ylab("Repeatability")+ facet_wrap(~Env,scales="free_x")+ggtitle("Adélie penguin")+
  guides(fill = guide_legend(override.aes = list(shape = 21)),
         shape = guide_legend(override.aes = list(fill = "black")))+ theme(legend.text=element_text(size=textS))

plot2


ggarrange(plot2,plot1,common.legend = TRUE,nrow=2)




########### repeatability within the training set used for RF
#load training for RF
trainingDF<-fread("/Users/mariannachimienti/MarieCurie/TrainingLittle2019.csv",header=TRUE)
head(trainingDF)
str(trainingDF)
dim(trainingDF)
trainingDF$TagID_2<-substr(trainingDF$ID_Ind,1,8)


# grouped boxplot
unique(trainingDF$StatesName)

ggplot(trainingDF[which(trainingDF$StatesNames=="Still")], aes(x=StatesNames, y=PitchDiff, fill=ID_Ind)) +
  geom_boxplot(outlier.size = pointS)+ theme(legend.position = "none")+ylim(-100,100)+ theme(legend.text=element_text(size=textS))

ggplot(trainingDF[which(trainingDF$StatesNames=="Still")], aes(x=StatesNames, y=VeDBA, fill=ID_Ind)) +
  geom_boxplot(outlier.size = pointS)+ theme(legend.position = "none")+ylim(0,10)+ theme(legend.text=element_text(size=textS))
























