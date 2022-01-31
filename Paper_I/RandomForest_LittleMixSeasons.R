rm(list = ls())

################### ################### ###################
################### ################### ###################
##################### load packages
################## ################### ###################
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
library(dplyr)

library(tidymodels)
library(workflows)
library(tune)
library(rsample)
library(recipes)
library(yardstick)
library(usemodels)
library(randomForest)
library(caret)
library(ranger)
library(parsnip)
library(doParallel)
library(pryr)
#### on mac to open several R windows, open terminal and paste:open -n /Applications/R.app

################### ################### ###################
################### ################### ###################
##################### parameters and functions
################### ################### ################### 
################### ################### ###################
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

thr_depth<-1 #consider diving at 2 meters

################### ################### ###################
################### ################### ###################
##################### read file
################### ################### ###################
################### ################### ###################
filenames <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/Little_2020EM", #59
#    "/Users/mariannachimienti/MarieCurie/Little_2019EM", #39
    pattern = "*.csv",
    full.names = TRUE
  )

length(filenames)

nInd<-19 ### look at the total numbers of individuals, sample same n of individuals per season. 

Random_ind<-sample(filenames, size = nInd,replace=FALSE)

trainingDF<-data.frame()

for (TripID in 1:length(Random_ind)) { #change this line if you want more or less files running
  accData <- fread(Random_ind[TripID], header = TRUE)
  
  ################### ################### ################### ###################
  ################### ################### ################### ###################
  ##################### randomly sample behaviours and create training set
  ################### ################### ################### ###################
  ################### ################### ################### ###################
  
  rownames(accData)<-seq(1,nrow(accData),1)
  accData$V1<-seq(1,nrow(accData),1)
  #PitchAdj<-getmode(accData$Pitch[which(accData$StatesNames=="Rest"| accData$StatesNames=="Swim/Porpoise")]) #adjust Pitch taking Rest behaviour
  #PitchAdj<-median(accData$Pitch) #adjust Pitch taking animal median
  PitchAdj<-median(accData$Pitch[which(accData$depth25Hz<=thr_depth)])
  accData$PitchDiff<-accData$Pitch-PitchAdj ###diff from animal mean
  
  #tapply(accData$PitchDiff, accData$StatesNames, summary)
  
  #assign individual ID
  ID_Ind <- basename(Random_ind[TripID])
  ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
  accData$ID_Ind<-ID_Ind
  
  ############################################################################
  ############################################################################
  ####smoothing data over 1 sec - taking most common beh. 
  # smWin<-25
  # Sm_StatesNames<-zoo::rollapply(accData$StatesNames,smWin, function(x) names(which.max(table(x))))
  # #Sm_StatesNames is shorter  by smWin, i repeat the first 12 and last 12 beh
  # 
  # accData$Sm_StatesNames<-accData$StatesNames
  # accData$Sm_StatesNames[13:(nrow(accData)-12)]<-Sm_StatesNames
  # accData[1:51,c("StatesNames","Sm_StatesNames")]
  # 
  # # table(accData$StatesNames)/nrow(accData)
  # table(accData$Sm_StatesNames)/nrow(accData)
  ############################################################################
  ############################################################################
  
  as.character(unique(accData$StatesNames))
  accData$StatesNames[which(accData$StatesNames=="Swimming_2")]<-"Swimming"
  accData$StatesNames[which(accData$StatesNames=="Still1")]<-"Still"
  
  behNames<-as.character(unique(accData$StatesNames))
  Nbeh<-length(unique(accData$StatesNames))
  table(accData$StatesNames)
  behlength<-as.data.frame(table(accData$StatesNames))
  colnames(behlength)<-c("beh","freq")
  
  #Define n obs to sample: this way, you always leave at least half of the samples of each class for the test set and use at most half of the samples for training
  #the 1000 then is arbitrary and it's only use is to make sure that the amount of samples taken from each animal isn't diverging too much
  #i.e. each animal get's to contribute to the classifier similarly strongly
  samplesTot<-c(behlength$freq) 
  sampleSize<-min(min(samplesTot/2), 1000)#maximum n obs to randomly sample
  
  
  for(i in 1:Nbeh){
    
    VecBeh<-accData$V1[which(accData$StatesNames==behNames[i])]
    indexes<-sample(VecBeh, size = sampleSize,replace=FALSE)
    #print(indexes[duplicated(indexes)])
    
    trainingSub <- accData[indexes,]
    # print(trainingSub[duplicated(trainingSub)])
    trainingSub<-trainingSub[,c("V1","TagID","Timestamp","X","Y","Z","changeDepth25Hz", "depth25Hz",
                                "SD_BF2" ,"SD_LA2","SD_DV2" ,"SD_BF10","SD_LA10" ,"SD_DV10",             
                                "SD_BF30" ,"SD_LA30" ,"SD_DV30" , "SD_BF60","SD_LA60","SD_DV60",             
                                "Pitch","PitchDiff","Dynamic_DorsoVentral" ,"Dynamic_Lateral" , "Dynamic_BackForward" , "ODBA","Roll",                
                                "SD_Roll","SD_Pitch","VeDBA" ,"SD_VeDBA" ,"StatesNames","ID_Ind")]
    trainingDF<-rbind(trainingDF,trainingSub)
    #print(trainingDF[duplicated(trainingDF)])
  }  
  
}

trainingDF_Little2020<-trainingDF


################### ################### ###################
################### ################### ###################
##################### read file
################### ################### ###################
################### ################### ###################
filenames <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/Little_2019EM",
    pattern = "*.csv",
    full.names = TRUE
  )

#10:length(filenames)

nInd<-19#length(filenames)/2

Random_ind<-sample(filenames, size = nInd,replace=FALSE)
Random_ind
trainingDF<-data.frame()

for (TripID in 1:length(Random_ind)) { #change this line if you want more or less files running
  accData <- fread(Random_ind[TripID], header = TRUE)
  
  
  ################### ################### ################### ###################
  ################### ################### ################### ###################
  ##################### randomly sample behaviours and create training set
  ################### ################### ################### ###################
  ################### ################### ################### ###################
  
  rownames(accData)<-seq(1,nrow(accData),1)
  accData$V1<-seq(1,nrow(accData),1)
  #PitchAdj<-getmode(accData$Pitch[which(accData$StatesNames=="Rest"| accData$StatesNames=="Swim/Porpoise")]) #adjust Pitch taking Rest behaviour
  #PitchAdj<-median(accData$Pitch) #adjust Pitch taking animal median
  PitchAdj<-median(accData$Pitch[which(accData$depth25Hz<=thr_depth)])
  accData$PitchDiff<-accData$Pitch-PitchAdj ###diff from animal mean
  
  #tapply(accData$PitchDiff, accData$StatesNames, summary)
  
  #assign individual ID
  ID_Ind <- basename(Random_ind[TripID])
  ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
  accData$ID_Ind<-ID_Ind
  
  ############################################################################
  ############################################################################
  ####smoothing data over 1 sec - taking most common beh. 
  # smWin<-25
  # Sm_StatesNames<-zoo::rollapply(accData$StatesNames,smWin, function(x) names(which.max(table(x))))
  # #Sm_StatesNames is shorter  by smWin, i repeat the first 12 and last 12 beh
  # 
  # accData$Sm_StatesNames<-accData$StatesNames
  # accData$Sm_StatesNames[13:(nrow(accData)-12)]<-Sm_StatesNames
  # accData[1:51,c("StatesNames","Sm_StatesNames")]
  # 
  # # table(accData$StatesNames)/nrow(accData)
  # table(accData$Sm_StatesNames)/nrow(accData)
  ############################################################################
  ############################################################################
  
  as.character(unique(accData$StatesNames))
  accData$StatesNames[which(accData$StatesNames=="Swimming_2")]<-"Swimming"
  accData$StatesNames[which(accData$StatesNames=="Still1")]<-"Still"
  
  behNames<-as.character(unique(accData$StatesNames))
  Nbeh<-length(unique(accData$StatesNames))
  table(accData$StatesNames)
  behlength<-as.data.frame(table(accData$StatesNames))
  colnames(behlength)<-c("beh","freq")
  
  #Define n obs to sample: this way, you always leave at least half of the samples of each class for the test set and use at most half of the samples for training
  #the 1000 then is arbitrary and it's only use is to make sure that the amount of samples taken from each animal isn't diverging too much
  #i.e. each animal get's to contribute to the classifier similarly strongly
  samplesTot<-c(behlength$freq) 
  sampleSize<-min(min(samplesTot/2), 1000)#maximum n obs to randomly sample
  
  
  for(i in 1:Nbeh){
    
    VecBeh<-accData$V1[which(accData$StatesNames==behNames[i])]
    indexes<-sample(VecBeh, size = sampleSize,replace=FALSE)
    #print(indexes[duplicated(indexes)])
    
    trainingSub <- accData[indexes,]
    # print(trainingSub[duplicated(trainingSub)])
    trainingSub<-trainingSub[,c("V1","TagID","Timestamp","X","Y","Z","changeDepth25Hz", "depth25Hz",
                                "SD_BF2" ,"SD_LA2","SD_DV2" ,"SD_BF10","SD_LA10" ,"SD_DV10",             
                                "SD_BF30" ,"SD_LA30" ,"SD_DV30" , "SD_BF60","SD_LA60","SD_DV60",             
                                "Pitch","PitchDiff","Dynamic_DorsoVentral" ,"Dynamic_Lateral" , "Dynamic_BackForward" , "ODBA","Roll",                
                                "SD_Roll","SD_Pitch","VeDBA" ,"SD_VeDBA" ,"StatesNames","ID_Ind")]
    trainingDF<-rbind(trainingDF,trainingSub)
    #print(trainingDF[duplicated(trainingDF)])
  }  
  
}

trainingDF_Little2019<-trainingDF

trainingDF<-rbind(trainingDF_Little2020,trainingDF_Little2019)

write.csv(trainingDF, "/Users/mariannachimienti/MarieCurie/TrainingLittle_MixSeason.csv", row.names = FALSE)

################### ################### ###################
################### ################### ###################
##################### run RF
################### ################### ###################
################### ################### ###################
rm(list=ls())
gc() 
ls()
mem_used() 

################### ################### ###################
################### ################### ###################
##################### load packages
################## ################### ###################
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
library(dplyr)

library(tidymodels)
library(workflows)
library(tune)
library(rsample)
library(recipes)
library(yardstick)
library(usemodels)
library(randomForest)
library(caret)
library(ranger)
library(parsnip)
library(doParallel)
library(pryr)


trainingDF<-fread("/Users/mariannachimienti/MarieCurie/TrainingLittle_MixSeason.csv",header=TRUE)
head(trainingDF)
str(trainingDF)
dim(trainingDF)

training_clean<-trainingDF[ ,c("changeDepth25Hz","depth25Hz","SD_DV2" ,"SD_DV10","SD_DV30" ,"SD_DV60",             
                               "PitchDiff","Roll","SD_Roll","SD_Pitch","VeDBA" ,"SD_VeDBA" ,"StatesNames")]


training_clean$StatesNames<-as.factor(training_clean$StatesNames)


# split the data into training (75%) and testing (25%)
DF_training_split <- initial_split(training_clean, 
                                   prop = 3/4)
DF_training_split

# extract training and testing sets
DF_train <- training(DF_training_split)
DF_test <- testing(DF_training_split)
#### I should center and scale the variables

###### ###### ###### ###### ###### ###### ###### 
###### use tidymodels
###### ###### ###### ###### ###### ###### ###### 


# create Cross Validation object from training data
trainingAcc_cv <- vfold_cv(DF_train)

# define the recipe
AccData_recipe <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(StatesNames ~ ., 
         data = training_clean)

rf_model <- 
  # specify that the model is a random forest
  rand_forest(trees = 1000, mtry=4) %>%
  # specify that the `mtry` parameter needs to be tuned: Number of variables randomly sampled as candidates at each split, around the sqrt(n max variables)
  # set_args(mtry = tune()) %>%
  # select the engine/package that underlies the model
  # When we set the engine, we add a new argument: importance = "impurity". 
  # This will provide variable importance scores for our last model, which gives some insight into which predictors drive model performance.
  set_engine("ranger", importance = "impurity") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("classification")   

# set the workflow
rf_workflow <- workflow() %>%
  # add the recipe
  add_recipe(AccData_recipe) %>%
  # add the model
  add_model(rf_model)  

#You can tune multiple parameters at once by providing multiple parameters to the expand.grid() function, 
#e.g. expand.grid(mtry = c(3, 4, 5), trees = c(100, 500)).  
# specify which values want to try
rf_grid <- expand.grid(mtry = c(2,3,4,5))

doParallel::registerDoParallel(cores=7)

system.time(
  # extract results
  rf_tune_results <- rf_workflow %>%
    tune_grid(resamples = trainingAcc_cv, #CV object
              #           grid = rf_grid, # grid of values to try
              metrics = metric_set(accuracy, roc_auc),
              control = control_resamples(save_pred = TRUE)
    ) 
)  

# print results
rf_tune_results %>%
  collect_metrics(summarize = FALSE)

#How did this turn out? Let’s look at AUC.
rf_tune_results %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, mtry) %>%
  pivot_longer(mtry,
               values_to = "value",
               names_to = "parameter"
  )%>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")

#Let’s select the best model according to the ROC AUC metric. Our final tuning parameter values are:
rf_best <- 
  rf_tune_results %>% 
  select_best(metric = "accuracy")

rf_best

#To calculate the data needed to plot the ROC curve, we use collect_predictions()
rf_pred <- 
  rf_tune_results %>%
  collect_predictions()

#show confusion matrix
rf_pred %>%
  #  collect_predictions() %>%
  conf_mat(StatesNames, .pred_class)%>% 
  autoplot(type = "heatmap")

#show ROC curve
rf_pred %>% 
  #  filter(id == "Fold01") %>%
  group_by(id) %>% # id contains our folds
  roc_curve(StatesNames,.pred_Ascending:.pred_Swimming) %>% 
  autoplot()

# If you want to use your model to predict the response for new observations, you need to use the fit() function on your workflow 
# and the dataset that you want to fit the final model on (e.g. the complete training + testing dataset).

#We can add the best model parameters to the workflow using the finalize_workflow() function.

rf_workflow <- rf_workflow %>%
  finalize_workflow(rf_best)

rf_fit <- rf_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(DF_training_split)

rf_fit

test_performance <- rf_fit %>% collect_metrics()
test_performance

# generate predictions from the test set
test_predictions <- rf_fit %>% collect_predictions()
test_predictions

#show confusion matrix
test_predictions %>%
  #  collect_predictions() %>%
  conf_mat(StatesNames, .pred_class)%>% 
  autoplot(type = "heatmap")

#show ROC curve
test_predictions %>% 
  #  filter(id == "Fold01") %>%
  group_by(id) %>% # id contains our folds
  roc_curve(StatesNames,.pred_Ascending:.pred_Swimming) %>% 
  autoplot()

#Fitting and using your final model
#### not sure it is best to select variables that went into RF for predict ~ both ways work 
final_model <- fit(rf_workflow, training_clean)  


#variable importance
ranger_obj <- pull_workflow_fit(final_model)$fit
ranger_obj

VarImp<-as.data.frame(sort(ranger_obj$variable.importance)) #metric is "importance"

colnames(VarImp)<-"Importance"
VarImp$Variable<-rownames(VarImp)


save.image(file="/Users/mariannachimienti/MarieCurie/RF_Results/RF_Little_MixSeasons.RData")

quit(save="no")

################### ################### ###################
################### ################### ###################
########### load workspace and test on validation Season
################### ################### ###################
################### ################### ###################
################### ################### ###################
#retain only final model and check memory usage
rm(list=ls()[! ls() %in% c("final_model")])
gc() 
ls()

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

library(tidymodels)
library(workflows)
library(tune)
library(rsample)
library(recipes)
library(yardstick)
library(usemodels)
library(randomForest)
library(caret)
library(ranger)
library(parsnip)
library(doParallel)
library(pryr)

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

thr_depth<-1 #consider diving at 1 meters


mem_used() 
trainingDF<-fread("/Users/mariannachimienti/MarieCurie/TrainingLittle_MixSeason.csv",header=TRUE)

################### ################### ###################
################### ################### ###################
################### ################### ################### 
##################### read file
################### ################### ###################
################### ################### ###################
#retain only final model and check memory usage

filenames <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/Little_2020EM",
    pattern = "*.csv",
    full.names = TRUE
  )

#take individual ID
ID_Ind <- basename(filenames)
ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
Pred_DF<-as.data.frame(cbind(filenames,ID_Ind))

#take which ID were in the training
filenamesPred<-Pred_DF[which(Pred_DF$ID_Ind %in% trainingDF$ID_Ind),]

#TripID<-1 
AccuracyDF<-data.frame(NA,NA)
TimeBudget_EM<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA)
TimeBudget_RF<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA)

for (TripID in 1:nrow(filenamesPred)) {#
  accData <- fread(filenamesPred$filenames[TripID], header = TRUE)
  rownames(accData)<-seq(1,nrow(accData),1)
  accData<-accData[,-c("V1")]
  accData$V1<-seq(1,nrow(accData),1)
  
  as.character(unique(accData$StatesNames))
  accData$StatesNames[which(accData$StatesNames=="Swimming_2")]<-"Swimming"
  accData$StatesNames[which(accData$StatesNames=="Still1")]<-"Still"
  
  #adjust Pitch taking Rest behaviour
  rownames(accData)<-seq(1,nrow(accData),1)
  accData$V1<-seq(1,nrow(accData),1)
  #PitchAdj<-getmode(accData$Pitch[which(accData$StatesNames=="Rest"| accData$StatesNames=="Swim/Porpoise")]) #adjust Pitch taking Rest behaviour
  #PitchAdj<-median(accData$Pitch) #adjust Pitch taking animal median
  PitchAdj<-median(accData$Pitch[which(accData$depth25Hz<=thr_depth)])
  accData$PitchDiff<-accData$Pitch-PitchAdj ###diff from animal mean
  
  #assign individual ID
  ID_Ind <- basename(filenamesPred[TripID,1])
  ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
  accData$ID_Ind<-ID_Ind
  
  ############################################################################
  ############################################################################
  # ####smoothing data over 1 sec - taking most common beh. 
  # smWin<-25
  # Sm_StatesNames<-zoo::rollapply(accData$StatesNames,smWin, function(x) names(which.max(table(x))))
  # #Sm_StatesNames is shorter  by smWin, i repeat the first 12 and last 12 beh
  # 
  # accData$Sm_StatesNames<-accData$StatesNames
  # accData$Sm_StatesNames[13:(nrow(accData)-12)]<-Sm_StatesNames
  # accData[1:51,c("StatesNames","Sm_StatesNames")]
  # 
  # table(accData$StatesNames)/nrow(accData)
  # table(accData$Sm_StatesNames)/nrow(accData)
  ############################################################################
  ############################################################################
  
  trainingDF_Tag<-trainingDF[which(trainingDF$ID_Ind==unique(accData$ID_Ind)),]
  dim(trainingDF_Tag)
  validation1 <- accData[!(accData$V1 %in% trainingDF_Tag$V1),] #remove obs from same tag
  dim(validation1)
  
  predAcc<-predict(final_model, new_data = validation1)
  validation1$Pred_RF<-predAcc$.pred_class
  
  
  Accuracy_An<-length(which(validation1$StatesNames==validation1$Pred_RF))/nrow(validation1) 
  AccuracyDF[TripID,1]<-ID_Ind
  AccuracyDF[TripID,2]<-Accuracy_An
  
  
  # as.character(unique(accData$StatesNames))
  # [1] "Preen/highFlap_L" "Walk"             "Swim/Porpoise"    "Preen/highFlap_W" "Rest"             "Hunting"          "Ascending"       
  # [8] "Swimming"         "Descending"       "Stand"            "LieDown" 
  
  
  TimeBudget_EM[TripID,1]<-nrow(validation1[which(validation1$StatesNames=="Still"),])/nrow(validation1)
  #  TimeBudget_EM[TripID,2]<-nrow(validation1[which(validation1$StatesNames=="Still1"),])/nrow(validation1)
  TimeBudget_EM[TripID,2]<-nrow(validation1[which(validation1$StatesNames=="Swim/Porpoise"),])/nrow(validation1)
  TimeBudget_EM[TripID,3]<-nrow(validation1[which(validation1$StatesNames=="Preen/highFlap_W"),])/nrow(validation1)
  TimeBudget_EM[TripID,4]<-nrow(validation1[which(validation1$StatesNames=="Rest"),])/nrow(validation1)
  TimeBudget_EM[TripID,5]<-nrow(validation1[which(validation1$StatesNames=="Hunting"),])/nrow(validation1)
  TimeBudget_EM[TripID,6]<-nrow(validation1[which(validation1$StatesNames=="Ascending"),])/nrow(validation1)
  TimeBudget_EM[TripID,7]<-nrow(validation1[which(validation1$StatesNames=="Swimming"),])/nrow(validation1)
  TimeBudget_EM[TripID,8]<-nrow(validation1[which(validation1$StatesNames=="Descending"),])/nrow(validation1)
  TimeBudget_EM[TripID,9]<-as.character("EM")
  
  TimeBudget_RF[TripID,1]<-nrow(validation1[which(validation1$Pred_RF=="Still"),])/nrow(validation1)
  #  TimeBudget_RF[TripID,2]<-nrow(validation1[which(validation1$Pred_RF=="Still1"),])/nrow(validation1)
  TimeBudget_RF[TripID,2]<-nrow(validation1[which(validation1$Pred_RF=="Swim/Porpoise"),])/nrow(validation1)
  TimeBudget_RF[TripID,3]<-nrow(validation1[which(validation1$Pred_RF=="Preen/highFlap_W"),])/nrow(validation1)
  TimeBudget_RF[TripID,4]<-nrow(validation1[which(validation1$Pred_RF=="Rest"),])/nrow(validation1)
  TimeBudget_RF[TripID,5]<-nrow(validation1[which(validation1$Pred_RF=="Hunting"),])/nrow(validation1)
  TimeBudget_RF[TripID,6]<-nrow(validation1[which(validation1$Pred_RF=="Ascending"),])/nrow(validation1)
  TimeBudget_RF[TripID,7]<-nrow(validation1[which(validation1$Pred_RF=="Swimming"),])/nrow(validation1)
  TimeBudget_RF[TripID,8]<-nrow(validation1[which(validation1$Pred_RF=="Descending"),])/nrow(validation1)
  TimeBudget_RF[TripID,9]<-as.character("RF")
  
}

colnames(AccuracyDF) <-c("ID_Ind","Accuracy") 
summary(AccuracyDF)




colnames(TimeBudget_EM)<-c("Still", "Swim/Porpoise","Preen/highFlap_W", "Rest" , "Hunting", "Ascending",     
                           "Swimming","Descending","Method" )

# Preen_highFlap_L<-as.data.frame(TimeBudget_EM[,1])
# Preen_highFlap_L$beh<-"Preen/highFlap_L"
# Preen_highFlap_L$Method<-"EM"
# colnames(Preen_highFlap_L)[1]<-"bud_value"
# 
# Walk<-as.data.frame(TimeBudget_EM[,2])
# Walk$beh<-"Walk"
# Walk$Method<-"EM"
# colnames(Walk)[1]<-"bud_value"
# 
# Swim_Porpoise<-as.data.frame(TimeBudget_EM[,3])
# Swim_Porpoise$beh<-"Swim/Porpoise"
# Swim_Porpoise$Method<-"EM"
# colnames(Swim_Porpoise)[1]<-"bud_value"
# 
# Preen_highFlap_W<-as.data.frame(TimeBudget_EM[,4])
# Preen_highFlap_W$beh<-"Preen/highFlap_W"
# Preen_highFlap_W$Method<-"EM"
# colnames(Preen_highFlap_W)[1]<-"bud_value"
# 
# Rest<-as.data.frame(TimeBudget_EM[,5])
# Rest$beh<-"Rest"
# Rest$Method<-"EM"
# colnames(Rest)[1]<-"bud_value"
# 
# Hunting<-as.data.frame(TimeBudget_EM[,6])
# Hunting$beh<-"Hunting"
# Hunting$Method<-"EM"
# colnames(Hunting)[1]<-"bud_value"
# 
# Ascending<-as.data.frame(TimeBudget_EM[,7])
# Ascending$beh<-"Ascending"
# Ascending$Method<-"EM"
# colnames(Ascending)[1]<-"bud_value"
# 
# Swimming<-as.data.frame(TimeBudget_EM[,8])
# Swimming$beh<-"Swimming"
# Swimming$Method<-"EM"
# colnames(Swimming)[1]<-"bud_value"
# 
# Descending<-as.data.frame(TimeBudget_EM[,9])
# Descending$beh<-"Descending"
# Descending$Method<-"EM"
# colnames(Descending)[1]<-"bud_value"
# 
# Stand<-as.data.frame(TimeBudget_EM[,10])
# Stand$beh<-"Stand"
# Stand$Method<-"EM"
# colnames(Stand)[1]<-"bud_value"
# 
# LieDown<-as.data.frame(TimeBudget_EM[,11])
# LieDown$beh<-"LieDown"
# LieDown$Method<-"EM"
# colnames(LieDown)[1]<-"bud_value"
# 
# EMBud<-rbind(Preen_highFlap_L,Walk,Swim_Porpoise,Preen_highFlap_W,Rest,Hunting,Ascending,Swimming,Descending,Stand,LieDown)
# 

colnames(TimeBudget_RF)<-c("Still", "Swim/Porpoise","Preen/highFlap_W", "Rest" , "Hunting", "Ascending",     
                           "Swimming","Descending","Method")


# Preen_highFlap_L<-as.data.frame(TimeBudget_RF[,1])
# Preen_highFlap_L$beh<-"Preen/highFlap_L"
# Preen_highFlap_L$Method<-"RF"
# colnames(Preen_highFlap_L)[1]<-"bud_value"
# 
# Walk<-as.data.frame(TimeBudget_RF[,2])
# Walk$beh<-"Walk"
# Walk$Method<-"RF"
# colnames(Walk)[1]<-"bud_value"
# 
# Swim_Porpoise<-as.data.frame(TimeBudget_RF[,3])
# Swim_Porpoise$beh<-"Swim/Porpoise"
# Swim_Porpoise$Method<-"RF"
# colnames(Swim_Porpoise)[1]<-"bud_value"
# 
# Preen_highFlap_W<-as.data.frame(TimeBudget_RF[,4])
# Preen_highFlap_W$beh<-"Preen/highFlap_W"
# Preen_highFlap_W$Method<-"RF"
# colnames(Preen_highFlap_W)[1]<-"bud_value"
# 
# Rest<-as.data.frame(TimeBudget_RF[,5])
# Rest$beh<-"Rest"
# Rest$Method<-"RF"
# colnames(Rest)[1]<-"bud_value"
# 
# Hunting<-as.data.frame(TimeBudget_RF[,6])
# Hunting$beh<-"Hunting"
# Hunting$Method<-"RF"
# colnames(Hunting)[1]<-"bud_value"
# 
# Ascending<-as.data.frame(TimeBudget_RF[,7])
# Ascending$beh<-"Ascending"
# Ascending$Method<-"RF"
# colnames(Ascending)[1]<-"bud_value"
# 
# Swimming<-as.data.frame(TimeBudget_RF[,8])
# Swimming$beh<-"Swimming"
# Swimming$Method<-"RF"
# colnames(Swimming)[1]<-"bud_value"
# 
# Descending<-as.data.frame(TimeBudget_RF[,9])
# Descending$beh<-"Descending"
# Descending$Method<-"RF"
# colnames(Descending)[1]<-"bud_value"
# 
# Stand<-as.data.frame(TimeBudget_RF[,10])
# Stand$beh<-"Stand"
# Stand$Method<-"RF"
# colnames(Stand)[1]<-"bud_value"
# 
# LieDown<-as.data.frame(TimeBudget_RF[,11])
# LieDown$beh<-"LieDown"
# LieDown$Method<-"RF"
# colnames(LieDown)[1]<-"bud_value"
# 
# RFBud<-rbind(Preen_highFlap_L,Walk,Swim_Porpoise,Preen_highFlap_W,Rest,Hunting,Ascending,Swimming,Descending,Stand,LieDown)


AllBudgets<-rbind(TimeBudget_RF,TimeBudget_EM)


write.csv(AccuracyDF, "/Users/mariannachimienti/MarieCurie/AccuracyDFLittle_2020_MixInTraining.csv", row.names = FALSE)
write.csv(AllBudgets, "/Users/mariannachimienti/MarieCurie/AllBudgetsLittle_2020_MixInTraining.csv", row.names = FALSE)


# # Basic barplot
# p<-ggplot(data=VarImp, aes(x=reorder(Variable, Importance), y=Importance)) +
#   geom_bar(stat="identity",fill="gray25")+ coord_flip() + xlab("")
# 
# dev.new()
# p


################### ################### ###################
################### ################### ###################
########### load workspace and test on new Seasons
##################### read file
################### ################### ###################
################### ################### ###################

################### ################### ###################
################### ################### ###################
########### load workspace and test on validation Season
################### ################### ###################
################### ################### ###################
################### ################### ###################
#retain only final model and check memory usage
#retain only final model and check memory usage
rm(list=ls()[! ls() %in% c("final_model")])
gc() 
ls()

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

library(tidymodels)
library(workflows)
library(tune)
library(rsample)
library(recipes)
library(yardstick)
library(usemodels)
library(randomForest)
library(caret)
library(ranger)
library(parsnip)
library(doParallel)
library(pryr)

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

thr_depth<-1 #consider diving at 2 meters

trainingDF<-fread("/Users/mariannachimienti/MarieCurie/TrainingLittle_MixSeason.csv",header=TRUE)

################### ################### ###################
################### ################### ###################
################### ################### ################### 
##################### read file
################### ################### ###################
################### ################### ###################
#retain only final model and check memory usage

filenames <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/Little_2020EM",
    pattern = "*.csv",
    full.names = TRUE
  )

#take individual ID
ID_Ind <- basename(filenames)
ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
Pred_DF<-as.data.frame(cbind(filenames,ID_Ind))

#remove which ID were in the training
filenamesPred<-Pred_DF[-which(Pred_DF$ID_Ind %in% trainingDF$ID_Ind),]

#TripID<-1 
AccuracyDF<-data.frame(NA,NA)
TimeBudget_EM<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA)
TimeBudget_RF<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA)

for (TripID in 1:nrow(filenamesPred)) {#
  accData <- fread(filenamesPred[TripID,1], header = TRUE)
  rownames(accData)<-seq(1,nrow(accData),1)
  accData<-accData[,-c("V1")]
  accData$V1<-seq(1,nrow(accData),1)
  
  as.character(unique(accData$StatesNames))
  accData$StatesNames[which(accData$StatesNames=="Swimming_2")]<-"Swimming"
  accData$StatesNames[which(accData$StatesNames=="Still1")]<-"Still"
  
  #adjust Pitch taking Rest behaviour
  rownames(accData)<-seq(1,nrow(accData),1)
  accData$V1<-seq(1,nrow(accData),1)
  #PitchAdj<-getmode(accData$Pitch[which(accData$StatesNames=="Rest"| accData$StatesNames=="Swim/Porpoise")]) #adjust Pitch taking Rest behaviour
  #PitchAdj<-median(accData$Pitch) #adjust Pitch taking animal median
  PitchAdj<-median(accData$Pitch[which(accData$depth25Hz<=thr_depth)])
  accData$PitchDiff<-accData$Pitch-PitchAdj ###diff from animal mean
  
  #assign individual ID
  ID_Ind <- basename(filenamesPred[TripID,1])
  ID_Ind <- substr(ID_Ind, 1, nchar(ID_Ind) - 4)
  accData$ID_Ind<-ID_Ind
  
  ############################################################################
  ############################################################################
  ####smoothing data over 1 sec - taking most common beh. 
  # smWin<-25
  # Sm_StatesNames<-zoo::rollapply(accData$StatesNames,smWin, function(x) names(which.max(table(x))))
  # #Sm_StatesNames is shorter  by smWin, i repeat the first 12 and last 12 beh
  # 
  # accData$Sm_StatesNames<-accData$StatesNames
  # accData$Sm_StatesNames[13:(nrow(accData)-12)]<-Sm_StatesNames
  # accData[1:51,c("StatesNames","Sm_StatesNames")]
  # 
  # # table(accData$StatesNames)/nrow(accData)
  # table(accData$Sm_StatesNames)/nrow(accData)
  ############################################################################
  ############################################################################
  
  
  
  #Fitting and using your final model
  #### not sure it is best to select variables that went into RF for predict ~ both ways work 
  #final_model <- fit(rf_workflow, training_clean) 
  predAcc<-predict(final_model, new_data = accData)
  accData$Pred_RF<-predAcc$.pred_class
  
  Accuracy_An<-length(which(accData$StatesNames==accData$Pred_RF))/nrow(accData) 
  AccuracyDF[TripID,1]<-ID_Ind
  AccuracyDF[TripID,2]<-Accuracy_An
  
  
  # as.character(unique(accData$StatesNames))
  # [1] "Preen/highFlap_L" "Walk"             "Swim/Porpoise"    "Preen/highFlap_W" "Rest"             "Hunting"          "Ascending"       
  # [8] "Swimming"         "Descending"       "Stand"            "LieDown" 
  TimeBudget_EM[TripID,1]<-nrow(accData[which(accData$StatesNames=="Still"),])/nrow(accData)
  # TimeBudget_EM[TripID,2]<-nrow(accData[which(accData$StatesNames=="Still1"),])/nrow(accData)
  TimeBudget_EM[TripID,2]<-nrow(accData[which(accData$StatesNames=="Swim/Porpoise"),])/nrow(accData)
  TimeBudget_EM[TripID,3]<-nrow(accData[which(accData$StatesNames=="Preen/highFlap_W"),])/nrow(accData)
  TimeBudget_EM[TripID,4]<-nrow(accData[which(accData$StatesNames=="Rest"),])/nrow(accData)
  TimeBudget_EM[TripID,5]<-nrow(accData[which(accData$StatesNames=="Hunting"),])/nrow(accData)
  TimeBudget_EM[TripID,6]<-nrow(accData[which(accData$StatesNames=="Ascending"),])/nrow(accData)
  TimeBudget_EM[TripID,7]<-nrow(accData[which(accData$StatesNames=="Swimming"),])/nrow(accData)
  TimeBudget_EM[TripID,8]<-nrow(accData[which(accData$StatesNames=="Descending"),])/nrow(accData)
  TimeBudget_EM[TripID,9]<-as.character("EM")
  
  TimeBudget_RF[TripID,1]<-nrow(accData[which(accData$Pred_RF=="Still"),])/nrow(accData)
  #TimeBudget_RF[TripID,2]<-nrow(accData[which(accData$Pred_RF=="Still1"),])/nrow(accData)
  TimeBudget_RF[TripID,2]<-nrow(accData[which(accData$Pred_RF=="Swim/Porpoise"),])/nrow(accData)
  TimeBudget_RF[TripID,3]<-nrow(accData[which(accData$Pred_RF=="Preen/highFlap_W"),])/nrow(accData)
  TimeBudget_RF[TripID,4]<-nrow(accData[which(accData$Pred_RF=="Rest"),])/nrow(accData)
  TimeBudget_RF[TripID,5]<-nrow(accData[which(accData$Pred_RF=="Hunting"),])/nrow(accData)
  TimeBudget_RF[TripID,6]<-nrow(accData[which(accData$Pred_RF=="Ascending"),])/nrow(accData)
  TimeBudget_RF[TripID,7]<-nrow(accData[which(accData$Pred_RF=="Swimming"),])/nrow(accData)
  TimeBudget_RF[TripID,8]<-nrow(accData[which(accData$Pred_RF=="Descending"),])/nrow(accData)
  TimeBudget_RF[TripID,9]<-as.character("RF")
  
}

colnames(AccuracyDF) <-c("ID_Ind","Accuracy") 
summary(AccuracyDF)




colnames(TimeBudget_EM)<-c("Still","Swim/Porpoise","Preen/highFlap_W", "Rest" , "Hunting", "Ascending",     
                           "Swimming","Descending","Method" )

# Preen_highFlap_L<-as.data.frame(TimeBudget_EM[,1])
# Preen_highFlap_L$beh<-"Preen/highFlap_L"
# Preen_highFlap_L$Method<-"EM"
# colnames(Preen_highFlap_L)[1]<-"bud_value"
# 
# Walk<-as.data.frame(TimeBudget_EM[,2])
# Walk$beh<-"Walk"
# Walk$Method<-"EM"
# colnames(Walk)[1]<-"bud_value"
# 
# Swim_Porpoise<-as.data.frame(TimeBudget_EM[,3])
# Swim_Porpoise$beh<-"Swim/Porpoise"
# Swim_Porpoise$Method<-"EM"
# colnames(Swim_Porpoise)[1]<-"bud_value"
# 
# Preen_highFlap_W<-as.data.frame(TimeBudget_EM[,4])
# Preen_highFlap_W$beh<-"Preen/highFlap_W"
# Preen_highFlap_W$Method<-"EM"
# colnames(Preen_highFlap_W)[1]<-"bud_value"
# 
# Rest<-as.data.frame(TimeBudget_EM[,5])
# Rest$beh<-"Rest"
# Rest$Method<-"EM"
# colnames(Rest)[1]<-"bud_value"
# 
# Hunting<-as.data.frame(TimeBudget_EM[,6])
# Hunting$beh<-"Hunting"
# Hunting$Method<-"EM"
# colnames(Hunting)[1]<-"bud_value"
# 
# Ascending<-as.data.frame(TimeBudget_EM[,7])
# Ascending$beh<-"Ascending"
# Ascending$Method<-"EM"
# colnames(Ascending)[1]<-"bud_value"
# 
# Swimming<-as.data.frame(TimeBudget_EM[,8])
# Swimming$beh<-"Swimming"
# Swimming$Method<-"EM"
# colnames(Swimming)[1]<-"bud_value"
# 
# Descending<-as.data.frame(TimeBudget_EM[,9])
# Descending$beh<-"Descending"
# Descending$Method<-"EM"
# colnames(Descending)[1]<-"bud_value"
# 
# Stand<-as.data.frame(TimeBudget_EM[,10])
# Stand$beh<-"Stand"
# Stand$Method<-"EM"
# colnames(Stand)[1]<-"bud_value"
# 
# LieDown<-as.data.frame(TimeBudget_EM[,11])
# LieDown$beh<-"LieDown"
# LieDown$Method<-"EM"
# colnames(LieDown)[1]<-"bud_value"
# 
# EMBud<-rbind(Preen_highFlap_L,Walk,Swim_Porpoise,Preen_highFlap_W,Rest,Hunting,Ascending,Swimming,Descending,Stand,LieDown)
# 

colnames(TimeBudget_RF)<-c("Still","Swim/Porpoise","Preen/highFlap_W", "Rest" , "Hunting", "Ascending",     
                           "Swimming","Descending","Method")


# Preen_highFlap_L<-as.data.frame(TimeBudget_RF[,1])
# Preen_highFlap_L$beh<-"Preen/highFlap_L"
# Preen_highFlap_L$Method<-"RF"
# colnames(Preen_highFlap_L)[1]<-"bud_value"
# 
# Walk<-as.data.frame(TimeBudget_RF[,2])
# Walk$beh<-"Walk"
# Walk$Method<-"RF"
# colnames(Walk)[1]<-"bud_value"
# 
# Swim_Porpoise<-as.data.frame(TimeBudget_RF[,3])
# Swim_Porpoise$beh<-"Swim/Porpoise"
# Swim_Porpoise$Method<-"RF"
# colnames(Swim_Porpoise)[1]<-"bud_value"
# 
# Preen_highFlap_W<-as.data.frame(TimeBudget_RF[,4])
# Preen_highFlap_W$beh<-"Preen/highFlap_W"
# Preen_highFlap_W$Method<-"RF"
# colnames(Preen_highFlap_W)[1]<-"bud_value"
# 
# Rest<-as.data.frame(TimeBudget_RF[,5])
# Rest$beh<-"Rest"
# Rest$Method<-"RF"
# colnames(Rest)[1]<-"bud_value"
# 
# Hunting<-as.data.frame(TimeBudget_RF[,6])
# Hunting$beh<-"Hunting"
# Hunting$Method<-"RF"
# colnames(Hunting)[1]<-"bud_value"
# 
# Ascending<-as.data.frame(TimeBudget_RF[,7])
# Ascending$beh<-"Ascending"
# Ascending$Method<-"RF"
# colnames(Ascending)[1]<-"bud_value"
# 
# Swimming<-as.data.frame(TimeBudget_RF[,8])
# Swimming$beh<-"Swimming"
# Swimming$Method<-"RF"
# colnames(Swimming)[1]<-"bud_value"
# 
# Descending<-as.data.frame(TimeBudget_RF[,9])
# Descending$beh<-"Descending"
# Descending$Method<-"RF"
# colnames(Descending)[1]<-"bud_value"
# 
# Stand<-as.data.frame(TimeBudget_RF[,10])
# Stand$beh<-"Stand"
# Stand$Method<-"RF"
# colnames(Stand)[1]<-"bud_value"
# 
# LieDown<-as.data.frame(TimeBudget_RF[,11])
# LieDown$beh<-"LieDown"
# LieDown$Method<-"RF"
# colnames(LieDown)[1]<-"bud_value"
# 
# RFBud<-rbind(Preen_highFlap_L,Walk,Swim_Porpoise,Preen_highFlap_W,Rest,Hunting,Ascending,Swimming,Descending,Stand,LieDown)


AllBudgets<-rbind(TimeBudget_RF,TimeBudget_EM)


write.csv(AccuracyDF, "/Users/mariannachimienti/MarieCurie/AccuracyDFLittle_2020_MixNotInTraining.csv", row.names = FALSE)
write.csv(AllBudgets, "/Users/mariannachimienti/MarieCurie/AllBudgetsLittle_2020_MixNotInTraining.csv", row.names = FALSE)

quit(save="no")


AllBudgets<-rbind(RFBud,EMBud)
AllBudgets$Method<-as.factor(AllBudgets$Method)
AllBudgets$beh<-as.factor(AllBudgets$beh)

ggplot() + 
  geom_boxplot(aes(y = bud_value, x = beh, fill = Method), data = AllBudgets)+
  ylab("proportion")+xlab("")+theme(text = element_text(size=15))




############## plot all seasons and compare
AccuracyDFLittle_2020 <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFLittle_2020_All.csv",header=TRUE)
AccuracyDFLittle_2020$Season<-1
AccuracyDFLittle_2020$Train_Test<-"In Train"
AccuracyDFLittle_2020$RFType<-"1Season"
summary(AccuracyDFLittle_2020)
sd(AccuracyDFLittle_2020$Accuracy)

AccuracyDFLittle_2019 <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFLittle_2019_All.csv",header=TRUE)
AccuracyDFLittle_2019$Season<-2
AccuracyDFLittle_2019$Train_Test<-"In Predict"
AccuracyDFLittle_2019$RFType<-"1Season"
summary(AccuracyDFLittle_2019)
sd(AccuracyDFLittle_2019$Accuracy)

AccuracyDFLittle_2020_MixIn <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFLittle_2020_MixInTraining.csv",header=TRUE)
AccuracyDFLittle_2020_MixIn$Season<-1
AccuracyDFLittle_2020_MixIn$Train_Test<-"In Train"
AccuracyDFLittle_2020_MixIn$RFType<-"MixSeason"
summary(AccuracyDFLittle_2020_MixIn)
sd(AccuracyDFLittle_2020_MixIn$Accuracy)


AccuracyDFLittle_2020_MixNotIn <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFLittle_2020_MixNotInTraining.csv",header=TRUE)
AccuracyDFLittle_2020_MixNotIn$Season<-1
AccuracyDFLittle_2020_MixNotIn$Train_Test<-"In Predict"
AccuracyDFLittle_2020_MixNotIn$RFType<-"MixSeason"
summary(AccuracyDFLittle_2020_MixNotIn)
sd(AccuracyDFLittle_2020_MixNotIn$Accuracy)


AccuracyDFLittle_2019_MixIn <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFLittle_2019_MixInTraining.csv",header=TRUE)
AccuracyDFLittle_2019_MixIn$Season<-2
AccuracyDFLittle_2019_MixIn$Train_Test<-"In Train"
AccuracyDFLittle_2019_MixIn$RFType<-"MixSeason"
summary(AccuracyDFLittle_2019_MixIn)
sd(AccuracyDFLittle_2019_MixIn$Accuracy)


AccuracyDFLittle_2019_MixNotIn <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFLittle_2019_MixNotInTraining.csv",header=TRUE)
AccuracyDFLittle_2019_MixNotIn$Season<-2
AccuracyDFLittle_2019_MixNotIn$Train_Test<-"In Predict"
AccuracyDFLittle_2019_MixNotIn$RFType<-"MixSeason"
summary(AccuracyDFLittle_2019_MixNotIn)
sd(AccuracyDFLittle_2019_MixNotIn$Accuracy)



AccuracyAll<-rbind(AccuracyDFLittle_2020,AccuracyDFLittle_2019,
                   AccuracyDFLittle_2020_MixIn,AccuracyDFLittle_2020_MixNotIn,
                   AccuracyDFLittle_2019_MixIn,AccuracyDFLittle_2019_MixNotIn)
AccuracyAll$Season<-as.factor(AccuracyAll$Season)
AccuracyAll$RFType<-as.factor(AccuracyAll$RFType)
AccuracyAll$Train_Test<-as.factor(AccuracyAll$Train_Test)
AccuracyAll$Lev1 <- factor(AccuracyAll$Train_Test, levels = c("In Train", "In Predict"))

supp.labs <- c("Training from season 1 only", "Training from both seasons")
names(supp.labs) <- c("1Season", "MixSeason")


library("wesanderson")
plotLittle<- ggplot() + theme_bw() +
  geom_boxplot(aes(y = Accuracy, x = Season, fill = Lev1),outlier.size = pointS, data = AccuracyAll)+
  ylab("")+xlab("Season number")+theme(text = element_text(size=textS))+ylim(0,1)+ggtitle("Little penguin")+
  theme(plot.title = element_text(hjust = 1)) +  
  scale_fill_manual(values=wes_palette(n=2, name="Chevalier1"),name="",labels = c("Train","Predict"))+ 
  facet_grid(. ~ RFType,labeller = labeller(RFType = supp.labs))+ theme(legend.position="top")+
  theme(legend.text=element_text(size=textS))







