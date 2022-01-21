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

thr_depth<-2 #consider diving at 2 meters
thr_pitch<- 60 #threshold for Pitch detecting land walking during foraging trips (default = 60)


################### ################### ###################
################### ################### ###################
##################### read file
################### ################### ###################
################### ################### ###################
filenames <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/Adelie_2019_2020EM",
#    "/Users/mariannachimienti/MarieCurie/Adelie_2018_2019EM",
    pattern = "*.csv",
    full.names = TRUE
  )

#10:length(filenames)

trainingDF<-data.frame()

for (TripID in 1:length(filenames)) { #change this line if you want more or less files running
  accData <- fread(filenames[TripID], header = TRUE)
  

  ################### ################### ################### ###################
  ################### ################### ################### ###################
  ##################### randomly sample behaviours and create training set
  ################### ################### ################### ###################
  ################### ################### ################### ###################
  
  rownames(accData)<-seq(1,nrow(accData),1)
  accData$V1<-seq(1,nrow(accData),1)
  #PitchAdj<-getmode(accData$Pitch[which(accData$StatesNames=="Rest"| accData$StatesNames=="Swim/Porpoise")]) #adjust Pitch taking Rest behaviour
  #PitchAdj<-median(accData$Pitch) #adjust Pitch taking animal median
  PitchAdj<-median(accData$Pitch[which(accData$depth25Hz<=thr_depth & accData$Pitch<thr_pitch)])
  accData$PitchDiff<-accData$Pitch-PitchAdj ###diff from animal mean
  
  #tapply(accData$PitchDiff, accData$StatesNames, summary)
  
  #assign individual ID
  ID_Ind <- basename(filenames[TripID])
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

#summary(trainingDF)

# unique(trainingDF$StatesNames)
# [1] "Walk"             "Preen/highFlap_L" "Preen/highFlap_W" "Rest"             "Swim/Porpoise"    "Descending"       "Swimming"         "Ascending"        "Hunting"         
# [10] "LieDown"          "Stand"    

# # grouped boxplot
# dev.new()
# ggplot(trainingDF[which(trainingDF$StatesNames=="Stand")], aes(x=StatesNames, y=PitchDiff, fill=ID_Ind)) +
#   geom_boxplot()+ theme(legend.position = "none")

write.csv(trainingDF, "/Users/mariannachimienti/MarieCurie/TrainingAdelie2019_2020.csv", row.names = FALSE)

################### ################### ###################
################### ################### ###################
##################### run RF
################### ################### ###################
################### ################### ###################

trainingDF<-fread("/Users/mariannachimienti/MarieCurie/TrainingAdelie2019_2020.csv",header=TRUE)
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
  rand_forest(trees = 1500, mtry=4) %>% #try 100, 500, 1000 and 1500
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

# #How did this turn out? Let’s look at AUC.
# rf_tune_results %>%
#   collect_metrics() %>%
#   filter(.metric == "roc_auc") %>%
#   select(mean, mtry) %>%
#   pivot_longer(mtry,
#                values_to = "value",
#                names_to = "parameter"
#   )%>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(show.legend = FALSE) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "AUC")

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
  autoplot(type = "heatmap")+theme(text = element_text(size=15))

#show ROC curve
rf_pred %>% 
  #  filter(id == "Fold01") %>%
  group_by(id) %>% # id contains our folds
  roc_curve(StatesNames,.pred_Ascending:.pred_Walk) %>% 
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
  roc_curve(StatesNames,.pred_Ascending:.pred_Walk) %>% 
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


save.image(file="/Users/mariannachimienti/MarieCurie/RF_Results/RF_Adelie2019_1500T.RData")

quit(save="no")


################### ################### ###################
################### ################### ###################
########### load workspace and test on validation Season
##################### read file
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

mem_used() 
#save.image(file="/Users/mariannachimienti/MarieCurie/RF_Results/RF_Adelie2019_NewPitch2.RData")
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

thr_depth<-2 #consider diving at 2 meters
thr_pitch<- 60 #threshold for Pitch detecting land walking during foraging trips (default = 60)




trainingDF<-fread("/Users/mariannachimienti/MarieCurie/TrainingAdelie2019_2020.csv",header=TRUE)


filenames <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/Adelie_2019_2020EM",
    #    "/Users/mariannachimienti/MarieCurie/Adelie_2018_2019EM",
    pattern = "*.csv",
    full.names = TRUE
  )


#TripID<-1 
AccuracyDF<-data.frame(NA,NA)
TimeBudget_EM<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
TimeBudget_RF<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
length(filenames)

for (TripID in 1:40) {#length(filenames)
accData <- fread(filenames[TripID], header = TRUE)
rownames(accData)<-seq(1,nrow(accData),1)
accData<-accData[,-c("V1")]
accData$V1<-seq(1,nrow(accData),1)

as.character(unique(accData$StatesNames))
accData$StatesNames[which(accData$StatesNames=="Swimming_2")]<-"Swimming"

#adjust Pitch taking Rest behaviour
rownames(accData)<-seq(1,nrow(accData),1)
accData$V1<-seq(1,nrow(accData),1)
#PitchAdj<-getmode(accData$Pitch[which(accData$StatesNames=="Rest"| accData$StatesNames=="Swim/Porpoise")]) #adjust Pitch taking Rest behaviour
#PitchAdj<-median(accData$Pitch) #adjust Pitch taking animal median
PitchAdj<-median(accData$Pitch[which(accData$depth25Hz<=thr_depth & accData$Pitch<thr_pitch)])
accData$PitchDiff<-accData$Pitch-PitchAdj ###diff from animal mean

#assign individual ID
ID_Ind <- basename(filenames[TripID])
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

TimeBudget_EM[TripID,1]<-nrow(validation1[which(validation1$StatesNames=="Preen/highFlap_L"),])/nrow(validation1)
TimeBudget_EM[TripID,2]<-nrow(validation1[which(validation1$StatesNames=="Walk"),])/nrow(validation1)
TimeBudget_EM[TripID,3]<-nrow(validation1[which(validation1$StatesNames=="Swim/Porpoise"),])/nrow(validation1)
TimeBudget_EM[TripID,4]<-nrow(validation1[which(validation1$StatesNames=="Preen/highFlap_W"),])/nrow(validation1)
TimeBudget_EM[TripID,5]<-nrow(validation1[which(validation1$StatesNames=="Rest"),])/nrow(validation1)
TimeBudget_EM[TripID,6]<-nrow(validation1[which(validation1$StatesNames=="Hunting"),])/nrow(validation1)
TimeBudget_EM[TripID,7]<-nrow(validation1[which(validation1$StatesNames=="Ascending"),])/nrow(validation1)
TimeBudget_EM[TripID,8]<-nrow(validation1[which(validation1$StatesNames=="Swimming"),])/nrow(validation1)
TimeBudget_EM[TripID,9]<-nrow(validation1[which(validation1$StatesNames=="Descending"),])/nrow(validation1)
TimeBudget_EM[TripID,10]<-nrow(validation1[which(validation1$StatesNames=="Stand"),])/nrow(validation1)
TimeBudget_EM[TripID,11]<-nrow(validation1[which(validation1$StatesNames=="LieDown"),])/nrow(validation1)


############################
VeDBA_Preen<- mean(validation1$VeDBA[which(validation1$StatesNames=="Preen/highFlap_W")])
TimeBudget_EM[TripID,12]<-VeDBA_Preen

Time_Preen<- length(validation1$VeDBA[which(validation1$StatesNames=="Preen/highFlap_W")])
TimeBudget_EM[TripID,13]<-Time_Preen/25  #time spent in sec

############################
VeDBA_Water<- mean(validation1$VeDBA[which(validation1$StatesNames=="Descending" |
                                             validation1$StatesNames=="Hunting" |
                                             validation1$StatesNames=="Ascending" |
                                             validation1$StatesNames=="Swimming" |
                                             validation1$StatesNames=="Rest" |
                                             validation1$StatesNames=="Swim/Porpoise")])

TimeBudget_EM[TripID,14]<-VeDBA_Water

Time_Water<- length(validation1$VeDBA[which(validation1$StatesNames=="Descending" |
                                              validation1$StatesNames=="Hunting" |
                                              validation1$StatesNames=="Ascending" |
                                              validation1$StatesNames=="Swimming" |
                                              validation1$StatesNames=="Rest" |
                                              validation1$StatesNames=="Swim/Porpoise")])
TimeBudget_EM[TripID,15]<-Time_Water/25 


############################
VeDBA_Land<-mean(validation1$VeDBA[which(validation1$StatesNames=="Preen/highFlap_L"|
                                           validation1$StatesNames=="Walk" |
                                           validation1$StatesNames=="LieDown" |
                                           validation1$StatesNames=="Stand" )])

TimeBudget_EM[TripID,16]<-VeDBA_Land

Time_Land<-length(validation1$VeDBA[which(validation1$StatesNames=="Preen/highFlap_L"|
                                            validation1$StatesNames=="Walk" |
                                            validation1$StatesNames=="LieDown" |
                                            validation1$StatesNames=="Stand" )])

TimeBudget_EM[TripID,17]<-Time_Land/25

TimeBudget_EM[TripID,18]<-as.character("EM")



############################
TimeBudget_RF[TripID,1]<-nrow(validation1[which(validation1$Pred_RF=="Preen/highFlap_L"),])/nrow(validation1)
TimeBudget_RF[TripID,2]<-nrow(validation1[which(validation1$Pred_RF=="Walk"),])/nrow(validation1)
TimeBudget_RF[TripID,3]<-nrow(validation1[which(validation1$Pred_RF=="Swim/Porpoise"),])/nrow(validation1)
TimeBudget_RF[TripID,4]<-nrow(validation1[which(validation1$Pred_RF=="Preen/highFlap_W"),])/nrow(validation1)
TimeBudget_RF[TripID,5]<-nrow(validation1[which(validation1$Pred_RF=="Rest"),])/nrow(validation1)
TimeBudget_RF[TripID,6]<-nrow(validation1[which(validation1$Pred_RF=="Hunting"),])/nrow(validation1)
TimeBudget_RF[TripID,7]<-nrow(validation1[which(validation1$Pred_RF=="Ascending"),])/nrow(validation1)
TimeBudget_RF[TripID,8]<-nrow(validation1[which(validation1$Pred_RF=="Swimming"),])/nrow(validation1)
TimeBudget_RF[TripID,9]<-nrow(validation1[which(validation1$Pred_RF=="Descending"),])/nrow(validation1)
TimeBudget_RF[TripID,10]<-nrow(validation1[which(validation1$Pred_RF=="Stand"),])/nrow(validation1)
TimeBudget_RF[TripID,11]<-nrow(validation1[which(validation1$Pred_RF=="LieDown"),])/nrow(validation1)

############################
VeDBA_Preen<- mean(validation1$VeDBA[which(validation1$Pred_RF=="Preen/highFlap_W")])
TimeBudget_RF[TripID,12]<-VeDBA_Preen

Time_Preen<- length(validation1$VeDBA[which(validation1$Pred_RF=="Preen/highFlap_W")])
TimeBudget_RF[TripID,13]<-Time_Preen/25  #time spent in sec

############################
VeDBA_Water<- mean(validation1$VeDBA[which(validation1$Pred_RF=="Descending" |
                                             validation1$Pred_RF=="Hunting" |
                                             validation1$Pred_RF=="Ascending" |
                                             validation1$Pred_RF=="Swimming" |
                                             validation1$Pred_RF=="Rest" |
                                             validation1$Pred_RF=="Swim/Porpoise")])

TimeBudget_RF[TripID,14]<-VeDBA_Water

Time_Water<- length(validation1$VeDBA[which(validation1$Pred_RF=="Descending" |
                                              validation1$Pred_RF=="Hunting" |
                                              validation1$Pred_RF=="Ascending" |
                                              validation1$Pred_RF=="Swimming" |
                                              validation1$Pred_RF=="Rest" |
                                              validation1$Pred_RF=="Swim/Porpoise")])
TimeBudget_RF[TripID,15]<-Time_Water/25 


############################
VeDBA_Land<-mean(validation1$VeDBA[which(validation1$Pred_RF=="Preen/highFlap_L"|
                                           validation1$Pred_RF=="Walk" |
                                           validation1$Pred_RF=="LieDown" |
                                           validation1$Pred_RF=="Stand" )])

TimeBudget_RF[TripID,16]<-VeDBA_Land

Time_Land<-length(validation1$VeDBA[which(validation1$Pred_RF=="Preen/highFlap_L"|
                                            validation1$Pred_RF=="Walk" |
                                            validation1$Pred_RF=="LieDown" |
                                            validation1$Pred_RF=="Stand" )])

TimeBudget_RF[TripID,17]<-Time_Land/25


TimeBudget_RF[TripID,18]<-as.character("RF")

}

colnames(AccuracyDF) <-c("ID_Ind","Accuracy") 
summary(AccuracyDF)




colnames(TimeBudget_EM)<-c("Preen/highFlap_L","Walk" , "Swim/Porpoise","Preen/highFlap_W", "Rest" , "Hunting", "Ascending",     
                           "Swimming","Descending","Stand","LieDown","VeDBA_Preen","Time_Preen_s","VeDBA_Water","Time_Water_s",
                           "VeDBA_Land","Time_Land_s","Method" )

TimeBudget_EM$ID_Ind<-AccuracyDF$ID_Ind

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

colnames(TimeBudget_RF)<-c("Preen/highFlap_L","Walk" , "Swim/Porpoise","Preen/highFlap_W", "Rest" , "Hunting", "Ascending",     
                           "Swimming","Descending","Stand","LieDown","VeDBA_Preen","Time_Preen_s","VeDBA_Water","Time_Water_s",
                           "VeDBA_Land","Time_Land_s","Method")

TimeBudget_RF$ID_Ind<-AccuracyDF$ID_Ind

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


write.csv(AccuracyDF, "/Users/mariannachimienti/MarieCurie/AccuracyDFAdelie2019_2020_1_40.csv", row.names = FALSE)
write.csv(AllBudgets, "/Users/mariannachimienti/MarieCurie/AllBudgetsAdelie2019_2020_1_40.csv", row.names = FALSE)

quit(save="no")


AccuracyDF1<-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFAdelie2019_2020_1_40.csv",header=TRUE)
AccuracyDF2<-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFAdelie2019_2020_41_80.csv",header=TRUE)
AccuracyDF3<-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFAdelie2019_2020_81_end.csv",header=TRUE)

AccuracyDF<-rbind(AccuracyDF1,AccuracyDF2,AccuracyDF3)
AccuracyDF<-AccuracyDF[which(!is.na(AccuracyDF$Accuracy))]
length(unique(AccuracyDF$ID_Ind))


summary(AccuracyDF)
boxplot(AccuracyDF$Accuracy)



AllBudgetsDF1<-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsAdelie2019_2020_1_40.csv",header=TRUE)
AllBudgetsDF2<-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsAdelie2019_2020_41_80.csv",header=TRUE)
AllBudgetsDF3<-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsAdelie2019_2020_81_end.csv",header=TRUE)

AllBudgetsDF<-rbind(AllBudgetsDF1,AllBudgetsDF2,AllBudgetsDF3)
AllBudgetsDF<-AllBudgetsDF[which(!is.na(AllBudgetsDF$Method))]

head(AllBudgetsDF)

AllBudgetsDF$Method<-as.factor(AllBudgetsDF$Method)
AllBudgetsDF$beh<-as.factor(AllBudgetsDF$beh)

ggplot() + 
  geom_boxplot(aes(y = bud_value, x = beh, fill = Method), data = AllBudgetsDF)+
  ylab("proportion")+xlab("")+theme(text = element_text(size=15))


write.csv(AccuracyDF, "/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFAdelie2019_2020_All.csv", row.names = FALSE)
write.csv(AllBudgetsDF, "/Users/mariannachimienti/MarieCurie/RF_Results/AllBudgetsAdelie2019_2020_All.csv", row.names = FALSE)



################### ################### ###################
################### ################### ###################
########### load workspace and test on new Seasons
##################### read file
################### ################### ###################
################### ################### ###################


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

thr_depth<-2 #consider diving at 2 meters
thr_pitch<- 60 #threshold for Pitch detecting land walking during foraging trips (default = 60)


filenames <-
  list.files(
    "/Users/mariannachimienti/MarieCurie/Adelie_2018_2019EM",
    pattern = "*.csv",
    full.names = TRUE
  )

#TripID<-1 
AccuracyDF<-data.frame(NA,NA)
TimeBudget_EM<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
TimeBudget_RF<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
length(filenames)

for (TripID in 1:length(filenames)) { #change this line if you want more or less files running
  accData <- fread(filenames[TripID], header = TRUE)
  rownames(accData)<-seq(1,nrow(accData),1)
  accData<-accData[,-c("V1")]
  accData$V1<-seq(1,nrow(accData),1)
  
  as.character(unique(accData$StatesNames))
  accData$StatesNames[which(accData$StatesNames=="Swimming_2")]<-"Swimming"
  
  #adjust Pitch taking Rest behaviour
  rownames(accData)<-seq(1,nrow(accData),1)
  accData$V1<-seq(1,nrow(accData),1)
  #PitchAdj<-getmode(accData$Pitch[which(accData$StatesNames=="Rest"| accData$StatesNames=="Swim/Porpoise")]) #adjust Pitch taking Rest behaviour
  #PitchAdj<-median(accData$Pitch) #adjust Pitch taking animal median
  PitchAdj<-median(accData$Pitch[which(accData$depth25Hz<=thr_depth & accData$Pitch<thr_pitch)])
  accData$PitchDiff<-accData$Pitch-PitchAdj ###diff from animal mean
  
  #assign individual ID
  ID_Ind <- basename(filenames[TripID])
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
  predAcc<-predict(final_model, new_data = accData)
  accData$Pred_RF<-predAcc$.pred_class
  
  Accuracy_An<-length(which(accData$StatesNames==accData$Pred_RF))/nrow(accData) 
  AccuracyDF[TripID,1]<-ID_Ind
  AccuracyDF[TripID,2]<-Accuracy_An
  
  
  # as.character(unique(accData$StatesNames))
  # [1] "Preen/highFlap_L" "Walk"             "Swim/Porpoise"    "Preen/highFlap_W" "Rest"             "Hunting"          "Ascending"       
  # [8] "Swimming"         "Descending"       "Stand"            "LieDown" 
  
  TimeBudget_EM[TripID,1]<-nrow(accData[which(accData$StatesNames=="Preen/highFlap_L"),])/nrow(accData)
  TimeBudget_EM[TripID,2]<-nrow(accData[which(accData$StatesNames=="Walk"),])/nrow(accData)
  TimeBudget_EM[TripID,3]<-nrow(accData[which(accData$StatesNames=="Swim/Porpoise"),])/nrow(accData)
  TimeBudget_EM[TripID,4]<-nrow(accData[which(accData$StatesNames=="Preen/highFlap_W"),])/nrow(accData)
  TimeBudget_EM[TripID,5]<-nrow(accData[which(accData$StatesNames=="Rest"),])/nrow(accData)
  TimeBudget_EM[TripID,6]<-nrow(accData[which(accData$StatesNames=="Hunting"),])/nrow(accData)
  TimeBudget_EM[TripID,7]<-nrow(accData[which(accData$StatesNames=="Ascending"),])/nrow(accData)
  TimeBudget_EM[TripID,8]<-nrow(accData[which(accData$StatesNames=="Swimming"),])/nrow(accData)
  TimeBudget_EM[TripID,9]<-nrow(accData[which(accData$StatesNames=="Descending"),])/nrow(accData)
  TimeBudget_EM[TripID,10]<-nrow(accData[which(accData$StatesNames=="Stand"),])/nrow(accData)
  TimeBudget_EM[TripID,11]<-nrow(accData[which(accData$StatesNames=="LieDown"),])/nrow(accData)
  
  
  ############################
  VeDBA_Preen<- mean(accData$VeDBA[which(accData$StatesNames=="Preen/highFlap_W")])
  TimeBudget_EM[TripID,12]<-VeDBA_Preen
  
  Time_Preen<- length(accData$VeDBA[which(accData$StatesNames=="Preen/highFlap_W")])
  TimeBudget_EM[TripID,13]<-Time_Preen/25  #time spent in sec
  
  ############################
  VeDBA_Water<- mean(accData$VeDBA[which(accData$StatesNames=="Descending" |
                                               accData$StatesNames=="Hunting" |
                                               accData$StatesNames=="Ascending" |
                                               accData$StatesNames=="Swimming" |
                                               accData$StatesNames=="Rest" |
                                               accData$StatesNames=="Swim/Porpoise")])
  
  TimeBudget_EM[TripID,14]<-VeDBA_Water
  
  Time_Water<- length(accData$VeDBA[which(accData$StatesNames=="Descending" |
                                                accData$StatesNames=="Hunting" |
                                                accData$StatesNames=="Ascending" |
                                                accData$StatesNames=="Swimming" |
                                                accData$StatesNames=="Rest" |
                                                accData$StatesNames=="Swim/Porpoise")])
  TimeBudget_EM[TripID,15]<-Time_Water/25 
  
  
  ############################
  VeDBA_Land<-mean(accData$VeDBA[which(accData$StatesNames=="Preen/highFlap_L"|
                                             accData$StatesNames=="Walk" |
                                             accData$StatesNames=="LieDown" |
                                             accData$StatesNames=="Stand" )])
  
  TimeBudget_EM[TripID,16]<-VeDBA_Land
  
  Time_Land<-length(accData$VeDBA[which(accData$StatesNames=="Preen/highFlap_L"|
                                              accData$StatesNames=="Walk" |
                                              accData$StatesNames=="LieDown" |
                                              accData$StatesNames=="Stand" )])
  
  TimeBudget_EM[TripID,17]<-Time_Land/25
  
  TimeBudget_EM[TripID,18]<-as.character("EM")
  
  
  
  ############################
  TimeBudget_RF[TripID,1]<-nrow(accData[which(accData$Pred_RF=="Preen/highFlap_L"),])/nrow(accData)
  TimeBudget_RF[TripID,2]<-nrow(accData[which(accData$Pred_RF=="Walk"),])/nrow(accData)
  TimeBudget_RF[TripID,3]<-nrow(accData[which(accData$Pred_RF=="Swim/Porpoise"),])/nrow(accData)
  TimeBudget_RF[TripID,4]<-nrow(accData[which(accData$Pred_RF=="Preen/highFlap_W"),])/nrow(accData)
  TimeBudget_RF[TripID,5]<-nrow(accData[which(accData$Pred_RF=="Rest"),])/nrow(accData)
  TimeBudget_RF[TripID,6]<-nrow(accData[which(accData$Pred_RF=="Hunting"),])/nrow(accData)
  TimeBudget_RF[TripID,7]<-nrow(accData[which(accData$Pred_RF=="Ascending"),])/nrow(accData)
  TimeBudget_RF[TripID,8]<-nrow(accData[which(accData$Pred_RF=="Swimming"),])/nrow(accData)
  TimeBudget_RF[TripID,9]<-nrow(accData[which(accData$Pred_RF=="Descending"),])/nrow(accData)
  TimeBudget_RF[TripID,10]<-nrow(accData[which(accData$Pred_RF=="Stand"),])/nrow(accData)
  TimeBudget_RF[TripID,11]<-nrow(accData[which(accData$Pred_RF=="LieDown"),])/nrow(accData)
  
  ############################
  VeDBA_Preen<- mean(accData$VeDBA[which(accData$Pred_RF=="Preen/highFlap_W")])
  TimeBudget_RF[TripID,12]<-VeDBA_Preen
  
  Time_Preen<- length(accData$VeDBA[which(accData$Pred_RF=="Preen/highFlap_W")])
  TimeBudget_RF[TripID,13]<-Time_Preen/25  #time spent in sec
  
  ############################
  VeDBA_Water<- mean(accData$VeDBA[which(accData$Pred_RF=="Descending" |
                                               accData$Pred_RF=="Hunting" |
                                               accData$Pred_RF=="Ascending" |
                                               accData$Pred_RF=="Swimming" |
                                               accData$Pred_RF=="Rest" |
                                               accData$Pred_RF=="Swim/Porpoise")])
  
  TimeBudget_RF[TripID,14]<-VeDBA_Water
  
  Time_Water<- length(accData$VeDBA[which(accData$Pred_RF=="Descending" |
                                                accData$Pred_RF=="Hunting" |
                                                accData$Pred_RF=="Ascending" |
                                                accData$Pred_RF=="Swimming" |
                                                accData$Pred_RF=="Rest" |
                                                accData$Pred_RF=="Swim/Porpoise")])
  TimeBudget_RF[TripID,15]<-Time_Water/25 
  
  
  ############################
  VeDBA_Land<-mean(accData$VeDBA[which(accData$Pred_RF=="Preen/highFlap_L"|
                                             accData$Pred_RF=="Walk" |
                                             accData$Pred_RF=="LieDown" |
                                             accData$Pred_RF=="Stand" )])
  
  TimeBudget_RF[TripID,16]<-VeDBA_Land
  
  Time_Land<-length(accData$VeDBA[which(accData$Pred_RF=="Preen/highFlap_L"|
                                              accData$Pred_RF=="Walk" |
                                              accData$Pred_RF=="LieDown" |
                                              accData$Pred_RF=="Stand" )])
  
  TimeBudget_RF[TripID,17]<-Time_Land/25
  
  
  TimeBudget_RF[TripID,18]<-as.character("RF")
  
}

colnames(AccuracyDF) <-c("ID_Ind","Accuracy") 
summary(AccuracyDF)




colnames(TimeBudget_EM)<-c("Preen/highFlap_L","Walk" , "Swim/Porpoise","Preen/highFlap_W", "Rest" , "Hunting", "Ascending",     
                           "Swimming","Descending","Stand","LieDown","VeDBA_Preen","Time_Preen_s","VeDBA_Water","Time_Water_s",
                           "VeDBA_Land","Time_Land_s","Method" )

TimeBudget_EM$ID_Ind<-AccuracyDF$ID_Ind

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

colnames(TimeBudget_RF)<-c("Preen/highFlap_L","Walk" , "Swim/Porpoise","Preen/highFlap_W", "Rest" , "Hunting", "Ascending",     
                           "Swimming","Descending","Stand","LieDown","VeDBA_Preen","Time_Preen_s","VeDBA_Water","Time_Water_s",
                           "VeDBA_Land","Time_Land_s","Method")

TimeBudget_RF$ID_Ind<-AccuracyDF$ID_Ind

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

 
 write.csv(AccuracyDF, "/Users/mariannachimienti/MarieCurie/AccuracyDFAdelie2018_2019_All.csv", row.names = FALSE)
 write.csv(AllBudgets, "/Users/mariannachimienti/MarieCurie/AllBudgetsAdelie2018_2019_All.csv", row.names = FALSE)

 quit(save="no")


AllBudgets$Method<-as.factor(AllBudgets$Method)
AllBudgets$beh<-as.factor(AllBudgets$beh)

  ggplot() + 
  geom_boxplot(aes(y = bud_value, x = beh, fill = Method), data = AllBudgets)+
    ylab("proportion")+xlab("")+theme(text = element_text(size=15))




  ############## plot all seasons and compare
  Accuracy_2019DF <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFAdelie2019_2020_All.csv",header=TRUE)
  Accuracy_2019DF$Season<-1
  
  Accuracy_2018DF <-fread("/Users/mariannachimienti/MarieCurie/RF_Results/AccuracyDFAdelie2018_2019_All.csv",header=TRUE)
  Accuracy_2018DF$Season<-2
  
  AccuracyAll<-rbind(Accuracy_2019DF,Accuracy_2018DF)
  AccuracyAll$Season<-as.factor(AccuracyAll$Season)
  
  library("wesanderson")
  ggplot() + 
    geom_boxplot(aes(y = Accuracy, x = Season, fill = Season), data = AccuracyAll)+
    ylab("RF accuracy")+xlab("")+theme(text = element_text(size=15))+
    scale_fill_manual(values=wes_palette(n=2, name="Royal1"))
  

  #########################################################
  #plots
  #show confusion matrix
  rf_pred %>%
    #  collect_predictions() %>%
    conf_mat(StatesNames, .pred_class)#%>% 
   # autoplot(type = "heatmap")
  
  #show ROC curve
  
  rf_pred %>% 
    #  filter(id == "Fold01") %>%
    group_by(id) %>% # id contains our folds
    roc_curve(StatesNames,.pred_Ascending:.pred_Walk) %>% 
    autoplot()
  
  
  ########### rename labels
  g<-rf_pred %>% 
    #  filter(id == "Fold01") %>%
    group_by(id) %>% # id contains our folds
    roc_curve(StatesNames,.pred_Ascending:.pred_Walk) #%>% 
  
  
 level1<-ifelse(g$.level=="Rest","Slow surface swim",
   ifelse(g$.level=="Preen/highFlap_L","Preen/Flap on land",
      ifelse(g$.level=="Preen/highFlap_W","Preen/Flap on water",
         ifelse(g$.level=="LieDown","Lie Down/Toboggan",
            ifelse(g$.level=="Descending","Descend",
                ifelse(g$.level=="Swimming","Swim/Cruise while diving",
                   ifelse(g$.level=="Hunting","Hunt",
                        ifelse(g$.level=="Ascending","Ascend",as.character(g$.level)))))))))
  
  
  g$.level<-level1
  dev.new()
  autoplot(g)
  
  
  
  # Basic barplot
  VarImp<-as.data.frame(sort(ranger_obj$variable.importance)) #metric is "importance"
  
  colnames(VarImp)<-"Importance"
  VarImp$Variable<-rownames(VarImp)
  
  head(VarImp)
  
  # 
  # [1] "changeDepth25Hz" "depth25Hz"       "PitchDiff"       "Roll"            "SD_DV10"         "SD_DV2"          "SD_DV30"        
  #"SD_DV60"         "SD_Pitch"        "SD_Roll"        
  # [11] "SD_VeDBA"        "VeDBA"     
  
  
  level1<-ifelse(VarImp$Variable=="changeDepth25Hz","Change in depth",
                 ifelse(VarImp$Variable=="depth25Hz","Depth",
                        ifelse(VarImp$Variable=="PitchDiff","Pitch",
                               ifelse(VarImp$Variable=="SD_DV10","SD heave 10 sec ",
                                      ifelse(VarImp$Variable=="SD_DV2","SD heave 2 sec ",
                                             ifelse(VarImp$Variable=="SD_DV30","SD heave 30 sec ",
                                                    ifelse(VarImp$Variable=="SD_DV60","SD heave 60 sec ",
                                                           ifelse(VarImp$Variable=="SD_Pitch","SD Pitch 60 sec ",
                                                                  ifelse(VarImp$Variable=="SD_Roll","SD Roll 30 sec ",
                                                                         ifelse(VarImp$Variable=="SD_VeDBA","SD VeDBA",as.character(VarImp$Variable)))))))))))
  
  VarImp$Variable<-level1
  VarImp
  
  VarImp<-VarImp[order(VarImp$Importance),]
  rownames(VarImp)<-seq(1,nrow(VarImp),1)
  
  VarImp$Variable1<-factor(VarImp$Variable, levels =VarImp$Variable)
  
  p<-ggplot(data=VarImp, aes(x=Variable1, y=Importance)) +
    geom_bar(stat="identity")+ coord_flip()+
    theme_bw() +theme(text = element_text(size=15))+xlab("")
  p
  
  
  


  
