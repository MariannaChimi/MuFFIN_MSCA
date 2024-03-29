  rm(list = ls())
  
  ################### ################### ###################
  ################### ################### ###################
  ##################### load packages
  ################### ################### ###################
  ################### ################### ###################
  
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
  #### on mac to open several R windows, open terminal and paste:open -n /Applications/R.app
  
  ################### ################### ###################
  ################### ################### ###################
  ##################### parameters
  ################### ################### ###################
  ################### ################### ###################
  
  thr_pitch<- 60 #threshold for Pitch detecting land walking during foraging trips (default = 60)
  thr_pitch_low<- 20 #mean lower threshold for Pitch detecting land walking during foraging trips (default= 20)
  thr_temp<-0 #threshold for temperature detecting land walking during foraging trips (default = 0)
  thr_time<-750 #threshold for time required for detecting land walking during foraging trips (default = 750)
  thr_pitch_lie<- -12.5 #threshold for Pitch detecting when back in water (default = -12.5)
  thr_depth<-2 #threshold for surface not surface variable (default = 2)
  
  # Create the function for mode
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  
  ################### ################### ###################
  ################### ################### ###################
  ##################### read file
  ################### ################### ###################
  ################### ################### ###################
  
  filenames <-
    list.files(
      "/Users/mariannachimienti/MarieCurie/DataAdelie_Trips_2018-2019/",
      pattern = "*.csv",
      full.names = TRUE
    )
  
  #10:length(filenames)
  
  fix<-c(52)
  
  for (i in fix) { #change this line if you want more or less files running
    accData <- fread(filenames[i], header = TRUE)
    
    accData<-accData[,-c("V1")]
    names(accData) <- gsub(x = names(accData), pattern = "\\.", replacement = "_")
    names(accData) <- gsub(x = names(accData), pattern = "\\-", replacement = "_")
    names(accData) <- gsub(x = names(accData), pattern = "\\ ", replacement = "_")
    names(accData) <- gsub(x = names(accData), pattern = "\\(", replacement = "_")
    names(accData) <- gsub(x = names(accData), pattern = "\\)", replacement = "_")

    options(digits.secs=9)   
    accData$DatesPos<-as.POSIXct(accData$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="UTC")
    
    # #### calculate depth
    # result <- getmode(accData$Pressure[which(!is.na(accData$Pressure))])
    # accData$depth<-(accData$Pressure-result)/100
    # depth<-accData$depth[-which(is.na(accData$depth))]
    # depth25Hz<-rep(depth,each=25)
    # accData$depth25Hz<-depth25Hz[1:nrow(accData)]
    # accData$depth25Hz<-accData$depth25Hz-min(summary(accData$depth25Hz))
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
    # accData$SD_Roll <-
    #   runsd(accData$Roll, k = 751) #SD over running mean over <1min, sampling 25Hz
    # 
    # accData$SD_Pitch <-
    #   runsd(accData$Pitch, k = 1501) #SD over running mean over 1min, sampling 25Hz
    # 
    # #VeDBA
    # accData$VeDBA <- sqrt((accData$Dynamic_DorsoVentral^2)+(accData$Dynamic_Lateral^2)+(accData$Dynamic_BackForward^2))
    # 
    # #SD SD_VeDBA
    # accData$SD_VeDBA <- runsd(accData$VeDBA, k = 1501)
    # 
    # 
    # Sur - Not Sur variable
    #hist(accData$depth25Hz,breaks=50)
    Sur_NotSur <- ifelse(accData$depth25Hz <= thr_depth, 0, 1) ##### look at this
    accData$Sur_NotSur <- as.factor(Sur_NotSur)
    
    
    ################### ################### ###################
    ################### ################### ###################
    ################### separate UW and Dive parts
    ################### ################### ###################
    ################### ################### ###################
    
    #### #### #### #### #### #### #### #### #### #### 
    #prepare subsets and set to record all the combinations of states and BIC
    #### #### #### #### #### #### #### #### #### #### 
    
    DataPartition<-accData[,c("TagID","Timestamp")]
    BICDiveTable<-data.frame(NA,NA)
    BICUWTable<-data.frame(NA,NA)
    BICUWTable_Land<-data.frame(NA,NA)
    BICUWTable_NotLand<-data.frame(NA,NA)
    
    comboN<-1
  
    subDataDive <- accData[which(accData$Sur_NotSur == 1), ]
    ScaleDataDive <- as.data.frame(scale(subDataDive[, c("VeDBA","Pitch","SD_DV2","changeDepth25Hz")], center = TRUE))
    TimeDive<-subDataDive[,c("TagID","Timestamp")]
    
    subDataUW <- accData[which(accData$Sur_NotSur == 0), ]
    
    
    #### #### #### #### #### #### #### #### #### #### 
    #detect ice/land events
    #### #### #### #### #### #### #### #### #### #### 
    
    subDataUW$Land_NotLand<-0 #unknown 
    #calculate succesive differences between dates
    #and identify gaps larger than 0.0500 - watch out for datePosix that is rounding time
    subDataUW$gap <- c(0, diff(subDataUW$DatesPos,units = "secs") > 0.0500)
    # cumulative sum of 'gap' variable
    subDataUW$group <- cumsum(subDataUW$gap) + 1
    subDataUWList<-split(subDataUW, subDataUW$group)
  
    #   meanGroup<-aggregate(. ~ subDataUW$group, subDataUW[c("Pitch")], mean)
    #   hist(meanGroup$Pitch,breaks=50)
  
    for(nList in 1:length(subDataUWList)){
  
      meanT<-mean(subDataUWList[[nList]]$Temp,na.rm=T)
      timeChunk<-nrow(subDataUWList[[nList]])
      
       if((max(subDataUWList[[nList]]$Pitch)>=thr_pitch & meanT>thr_temp) | (mean(subDataUWList[[nList]]$Pitch)>=thr_pitch_low & timeChunk> thr_time)) {
        
        DataChunk<-subDataUWList[[nList]]
        DataChunk$Land_NotLand<-1 #is on Land
        DataChunk$Land_NotLand2<-NA
        DataChunk$Land_NotLand2[which(DataChunk$Pitch>=thr_pitch)]<-1 #1 is on Land
        DataChunk$gapNA <- NA
        DataChunk$groupNA <- NA
        
        DataChunk_NA<-DataChunk[which(is.na(DataChunk$Land_NotLand2)),]
        DataChunk_NA$gapNA <- c(0, diff(DataChunk_NA$DatesPos,units = "secs") > 0.0500)
        DataChunk_NA$groupNA <- cumsum(DataChunk_NA$gapNA) + 1
        
        if(nrow(DataChunk_NA)>0){
         DataChunk_NA_List<-split(DataChunk_NA, DataChunk_NA$groupNA)
       
         for(nListNA in 1:length(DataChunk_NA_List)){
          
          if( any(DataChunk_NA_List[[nListNA]]$Pitch<thr_pitch_lie)){
            DataChunk_NA_List[[nListNA]]$Land_NotLand<-0 #labelled as UW
          } else{
            DataChunk_NA_List[[nListNA]]$Land_NotLand<-1 #labelled as Land
          }
         }
        
         DataChunk_NA <- ldply(DataChunk_NA_List, data.frame,.id = NULL)
         }
        
        DataChunk_NotNA<-DataChunk[which(!is.na(DataChunk$Land_NotLand2)),]
        
        DataChunk<-rbind(DataChunk_NA,DataChunk_NotNA)
        DataChunk<-DataChunk[order(DataChunk$DatesPos), ]
        
        subDataUWList[[nList]]<-DataChunk
        
      }
    }
    
    
    subDataUW <- ldply(subDataUWList, data.frame,.id = NULL)
    subDataUW<-subDataUW[order(subDataUW$DatesPos), ]
    dim(subDataUW)
    subDataUW$Land_NotLand<-as.factor(subDataUW$Land_NotLand)
    LandPar<-length(unique(subDataUW$Land_NotLand))
    
    if(LandPar>1){ #if both water and land/ice events have been detected
      
      subDataUW_Land<-subDataUW[which(subDataUW$Land_NotLand==1),]
      subDataUW_NotLand<-subDataUW[which(subDataUW$Land_NotLand==0),]
      
      ScaleDataUW_Land <- as.data.frame(scale(subDataUW_Land[, c("VeDBA","Pitch","SD_Roll","SD_DV10")], center = TRUE))
      ScaleDataUW_NotLand <- as.data.frame(scale(subDataUW_NotLand[, c("VeDBA","Pitch","SD_Roll","SD_DV10")], center = TRUE))
      
      TimeUW_Land<-subDataUW_Land[,c("TagID","Timestamp")]
      TimeUW_NotLand<-subDataUW_NotLand[,c("TagID","Timestamp")]
      
      rm(subDataUW_Land,subDataUW_NotLand)
      gc()
    }
    
    
    if(LandPar==1){ #if only water or land/ice events have been detected
      
      ScaleDataUW <- as.data.frame(scale(subDataUW[, c("VeDBA","Pitch","SD_Roll","SD_DV10")], center = TRUE))
      TimeUW<-subDataUW[,c("TagID","Timestamp")]
    }
    
    rm(subDataDive,subDataUW,accData)
    gc() 
    #model structure
    stModel <- mixmodStrategy(initMethod = "random",
                              nbTryInInit = 1000,
                              nbTry = 10)
    
    #### #### #### #### #### #### #### #### #### #### 
    ### Diving EM
    #### #### #### #### #### #### #### #### #### #### 
    for(nStatesDive in 3:8){ # 3:8
      
      
      EM_All <-
      mixmodCluster(
        data = ScaleDataDive,
        nbCluster = nStatesDive,
        strategy = stModel,
        #                        weight=subData$Sur_NotSur,
        criterion = c("BIC", "ICL", "NEC")
      )
      
    EM_All@bestResult
    Partition<- paste(EM_All@bestResult@partition,"_Dive",sep="")
    TimeDive<-cbind(TimeDive,Partition)
    colnames(TimeDive)[ncol(TimeDive)]<-paste("StatesDive_",nStatesDive,sep="")
    
    BIC_Dive<-EM_All@bestResult@criterionValue[1]
    
    BICRow<-c(nStatesDive,BIC_Dive)
    BICDiveTable<-rbind(BICDiveTable,BICRow)
    
    gc() 
    } 
    
    #### #### #### #### #### #### #### #### #### #### 
    #### Subsurface EM
    #### #### #### #### #### #### #### #### #### #### 
   
     if(LandPar>1){ #if both water and land/ice events have been detected
      
      for(nStatesUW_Land in 2:5){
        
        
        EM_All <-
          mixmodCluster(
            data = ScaleDataUW_Land,
            nbCluster = nStatesUW_Land,
            strategy = stModel,
            criterion = c("BIC", "ICL", "NEC")
          )
        EM_All@bestResult
        Partition<- paste(EM_All@bestResult@partition,"_UWLand",sep="")
        TimeUW_Land<-cbind(TimeUW_Land,Partition)
        colnames(TimeUW_Land)[ncol(TimeUW_Land)]<-paste("StatesUWLand_",nStatesUW_Land,sep="")
        
        BIC_UW_Land<-EM_All@bestResult@criterionValue[1]
        BICRow<-c(nStatesUW_Land,BIC_UW_Land)
        BICUWTable_Land<-rbind(BICUWTable_Land,BICRow)
        
        gc() 
      }
      
      
      for(nStatesUW_NotLand in 2:5){
        
        
        EM_All <-
          mixmodCluster(
            data = ScaleDataUW_NotLand,
            nbCluster = nStatesUW_NotLand,
            strategy = stModel,
            criterion = c("BIC", "ICL", "NEC")
          )
        EM_All@bestResult
        Partition<- paste(EM_All@bestResult@partition,"_UWNotLand",sep="")
        TimeUW_NotLand<-cbind(TimeUW_NotLand,Partition)
        colnames(TimeUW_NotLand)[ncol(TimeUW_NotLand)]<-paste("StatesUWNotLand_",nStatesUW_NotLand,sep="")
        
        BIC_UW_NotLand<-EM_All@bestResult@criterionValue[1]
        BICRow<-c(nStatesUW_NotLand,BIC_UW_NotLand)
        BICUWTable_NotLand<-rbind(BICUWTable_NotLand,BICRow)
        
        gc() 
      }
      
      
    }
    
    if(LandPar==1){ #if only water or land/ice events have been detected
      
      for(nStatesUW in 2:8){
        
        EM_All <-
          mixmodCluster(
            data = ScaleDataUW,
            nbCluster = nStatesUW,
            strategy = stModel,
            criterion = c("BIC", "ICL", "NEC")
          )
        EM_All@bestResult
        Partition<- paste(EM_All@bestResult@partition,"_UW",sep="")
        TimeUW<-cbind(TimeUW,Partition)
        colnames(TimeUW)[ncol(TimeUW)]<-paste("StatesUW_",nStatesUW,sep="")
        
        BIC_UW<-EM_All@bestResult@criterionValue[1]
        BICRow<-c(nStatesUW,BIC_UW)
        BICUWTable<-rbind(BICUWTable,BICRow)
        
        gc() 
      }
    }
    
    
    #### #### #### #### #### #### #### #### #### #### 
    #### combine results
    #### #### #### #### #### #### #### #### #### #### 
  
      if(LandPar>1){ #if both water and land/ice events have been detected
      
      TimeDive<-as.data.frame(TimeDive)
      TimeUW_Land<-as.data.frame(TimeUW_Land)
      TimeUW_NotLand<-as.data.frame(TimeUW_NotLand)
      
      for (iDive in 3:ncol(TimeDive)){
        
        SubDive<-TimeDive[,c(1,2,iDive)]
        colnames(SubDive)[ncol(SubDive)]<-"Partition"
        
        for (iUW_Land in 3:ncol(TimeUW_Land)){
          
          SubUW_Land<-TimeUW_Land[,c(1,2,iUW_Land)]
          colnames(SubUW_Land)[ncol(SubUW_Land)]<-"Partition"
          
          for (iUW_NotLand in 3:ncol(TimeUW_NotLand)){
            
            SubUW_NotLand<-TimeUW_NotLand[,c(1,2,iUW_NotLand)]
            colnames(SubUW_NotLand)[ncol(SubUW_NotLand)]<-"Partition"
          
            allEM <- rbind(SubDive,SubUW_Land,SubUW_NotLand)
            options(digits.secs=9)   
            allEM$DatesPos<-as.POSIXct(allEM$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="UTC")
            EM_Partition <- allEM[order(allEM$DatesPos), ]
            rm(allEM)
          
            DataPartition<-cbind(DataPartition,EM_Partition$Partition)
            colnames(DataPartition)[ncol(DataPartition)]<-paste("StatesComb_",comboN,sep="")
            comboN<-comboN+1
            gc() 
          }
        gc() 
        }
       }
      }
    
      
      if(LandPar==1){ #if only water or land/ice events have been detected
        TimeDive<-as.data.frame(TimeDive)
        TimeUW<-as.data.frame(TimeUW)
        
        for (iDive in 3:ncol(TimeDive)){
          
          SubDive<-TimeDive[,c(1,2,iDive)]
          colnames(SubDive)[ncol(SubDive)]<-"Partition"
          
          for (iUW in 3:ncol(TimeUW)){
            
            SubUW<-TimeUW[,c(1,2,iUW)]
            colnames(SubUW)[ncol(SubUW)]<-"Partition"
            
            allEM <- rbind(SubDive,SubUW)
            options(digits.secs=9)   
            allEM$DatesPos<-as.POSIXct(allEM$Timestamp, format='%d/%m/%Y %H:%M:%OS',tz="UTC")
            EM_Partition <- allEM[order(allEM$DatesPos), ]
            rm(allEM)
            
            DataPartition<-cbind(DataPartition,EM_Partition$Partition)
            colnames(DataPartition)[ncol(DataPartition)]<-paste("StatesComb_",comboN,sep="")
            comboN<-comboN+1
            gc() 
          }
          gc() 
          
        }
      }
    #### #### #### #### #### #### #### #### #### #### 
    #### write results
    #### #### #### #### #### #### #### #### #### #### 
    
    #accData<-within(accData,rm("DatesPos")) # remove datesPos might create problems when loading
    
    ID_Ind <- basename(filenames[i])
    ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
    ID_Ind <- paste(ID_Ind, "_EM", ".csv", sep = "")
    pathRes <-
      paste("/Users/mariannachimienti/MarieCurie/EM_Results_Adelie_2018_2019/",
            ID_Ind,
            sep = "")
    
    write.csv(DataPartition, pathRes, row.names = TRUE)
    
    colnames(BICDiveTable)<-c("nStatesDive","BIC_Dive")
    BICDiveTable<-BICDiveTable[which(!is.na(BICDiveTable$nStatesDive)),]
    
    ID_Ind <- basename(filenames[i])
    ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
    ID_Ind <- paste(ID_Ind, "_BICDive_table", ".csv", sep = "")
    pathRes <-
      paste("/Users/mariannachimienti/MarieCurie/EM_Results_Adelie_2018_2019/",
            ID_Ind,
            sep = "")
    write.csv(BICDiveTable, pathRes, row.names = TRUE)
    
    rm(DataPartition,BICDiveTable)
    gc() 
   
    if(LandPar>1){
     
      colnames(BICUWTable_Land)<-c("nStatesUW","BIC_UW")
      BICUWTable_Land<-BICUWTable_Land[which(!is.na(BICUWTable_Land$nStatesUW)),]
      
      ID_Ind <- basename(filenames[i])
      ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
      ID_Ind <- paste(ID_Ind, "_BICUW_Land_table", ".csv", sep = "")
      pathRes <-
        paste("/Users/mariannachimienti/MarieCurie/EM_Results_Adelie_2018_2019/",
              ID_Ind,
              sep = "")
      write.csv(BICUWTable_Land, pathRes, row.names = TRUE)
      
      colnames(BICUWTable_NotLand)<-c("nStatesUW","BIC_UW")
      BICUWTable_NotLand<-BICUWTable_NotLand[which(!is.na(BICUWTable_NotLand$nStatesUW)),]
      
      ID_Ind <- basename(filenames[i])
      ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
      ID_Ind <- paste(ID_Ind, "_BICUW_NotLand_table", ".csv", sep = "")
      pathRes <-
        paste("/Users/mariannachimienti/MarieCurie/EM_Results_Adelie_2018_2019/",
              ID_Ind,
              sep = "")
      write.csv(BICUWTable_NotLand, pathRes, row.names = TRUE)
      
      rm(BICUWTable_NotLand,BICUWTable_Land)
      gc() 
    }
    
    
     if(LandPar==1){
    colnames(BICUWTable)<-c("nStatesUW","BIC_UW")
    BICUWTable<-BICUWTable[which(!is.na(BICUWTable$nStatesUW)),]
    
    ID_Ind <- basename(filenames[i])
    ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
    ID_Ind <- paste(ID_Ind, "_BICUW_table", ".csv", sep = "")
    pathRes <-
      paste("/Users/mariannachimienti/MarieCurie/EM_Results_Adelie_2018_2019/",
            ID_Ind,
            sep = "")
    write.csv(BICUWTable, pathRes, row.names = TRUE)
    
    rm(BICUWTable)
    gc() 
     }
    
  }
  
  
  quit(save="no")
  
  
  
  
  
