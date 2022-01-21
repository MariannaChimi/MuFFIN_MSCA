#install.packages("rptR")
library(rptR)
library(data.table)
library(ggplot2)
library(ggpubr)
library(lme4)

########### repeatability within the training set used for RF
#load training for RF
trainingDF<-fread("/Users/mariannachimienti/MarieCurie/TrainingAdelie2019_2020.csv",header=TRUE)
head(trainingDF)
str(trainingDF)
dim(trainingDF)
trainingDF$TagID_2<-substr(trainingDF$ID_Ind,1,8)


# grouped boxplot
# ggplot(trainingDF[which(trainingDF$StatesNames=="Preen/highFlap_W")], aes(x=StatesNames, y=PitchDiff, fill=ID_Ind)) +
#   geom_boxplot()+ theme(legend.position = "none")+ylim(-100,100)
# 
# ggplot(trainingDF[which(trainingDF$StatesNames=="Preen/highFlap_W")], aes(x=StatesNames, y=VeDBA, fill=ID_Ind)) +
#   geom_boxplot()+ theme(legend.position = "none")+ylim(0,10)


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
R_Scores_Pitch$DF<-"TrainingAdelie2019_2020"

colnames(R_Scores_VeDBA)<-c("mean","se","Beh")
R_Scores_VeDBA$var<-"VeDBA"
R_Scores_VeDBA$DF<-"TrainingAdelie2019_2020"


RScores_all<-rbind(R_Scores_VeDBA,R_Scores_Pitch)
RScores_all


write.csv(RScores_all, "/Users/mariannachimienti/MarieCurie/RF_Results/RScores_Adelie2019_2020.csv", row.names = FALSE)

###################### general boxplot

trainingDF$Beh<-factor(trainingDF$StatesNames, levels = c("Walk", "Preen/highFlap_L", "Stand","LieDown","Rest","Preen/highFlap_W","Swim/Porpoise",
                                                                              "Descending","Swimming","Hunting","Ascending"))


trainingDF$Beh<-ifelse(trainingDF$Beh=="Rest","Slow surface swim",
                                     ifelse(trainingDF$Beh=="Preen/highFlap_L","Preen/Flap on land",
                                            ifelse(trainingDF$Beh=="Preen/highFlap_W","Preen/Flap on water",
                                                   ifelse(trainingDF$Beh=="LieDown","Lie Down/Toboggan",
                                                          ifelse(trainingDF$Beh=="Descending","Descend",
                                                                 ifelse(trainingDF$Beh=="Swimming","Swim/Cruise while diving",
                                                                        ifelse(trainingDF$Beh=="Hunting","Hunt",
                                                                               ifelse(trainingDF$Beh=="Ascending","Ascend",as.character(trainingDF$Beh)))))))))

plot1<- ggplot(trainingDF, aes(x=Beh, y=PitchDiff, fill=Beh)) + theme_bw() + 
  geom_boxplot(color="gray35",outlier.colour = "gray45")+ theme(legend.position = "none")+ylim(-110,110)+
  scale_fill_manual(values=wes_palette(n=11, name="Darjeeling2", type = "continuous"),name="")+
  ylab("Pitch (degrees)")+xlab("")+theme(text = element_text(size=15))


#plot1


plot2<- ggplot(trainingDF, aes(x=Beh, y=VeDBA, fill=Beh)) + theme_bw() + 
  geom_boxplot(color="gray35",outlier.colour = "gray45")+ theme(legend.position = "none")+ylim(0,10)+
  scale_fill_manual(values=wes_palette(n=11, name="Darjeeling2", type = "continuous"),name="")+
  ylab("VeDBA (g)")+xlab("")+theme(text = element_text(size=15))


ggarrange(plot1,plot2,nrow=2)





###############################################################################################################

########### repeatability within the training set used for RF
#load training for RF
trainingDF<-fread("/Users/mariannachimienti/MarieCurie/TrainingAdelie_MixSeason.csv",header=TRUE)
head(trainingDF)
str(trainingDF)
dim(trainingDF)
trainingDF$TagID_2<-substr(trainingDF$ID_Ind,1,8)

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
R_Scores_Pitch$DF<-"Adelie_MixSeason"

colnames(R_Scores_VeDBA)<-c("mean","se","Beh")
R_Scores_VeDBA$var<-"VeDBA"
R_Scores_VeDBA$DF<-"Adelie_MixSeason"


RScores_all<-rbind(R_Scores_VeDBA,R_Scores_Pitch)
RScores_all


write.csv(RScores_all, "/Users/mariannachimienti/MarieCurie/RF_Results/RScores_Adelie_MixSeason.csv", row.names = FALSE)
quit(save="no")














