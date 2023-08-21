library(RcppHungarian)
library(dplyr)
library(tidyverse)

### Functions ###
euclidean_dist <- function(x, y) sqrt(sum((x - y)^2))

### Pre-defined variables ###
set.seed(593903)
difficulty <- 5
vectorDim <- 3
data.accuracy.raw  <- data.frame()
NoParticipant <- 0

### Loop for all participant files
for(par in 1:16){
  dirPath <- paste(c("~/R Projects/ARSpatialMemory-R/Data/participant", par), collapse = " ")
  if(dir.exists(dirPath)){
    setwd(dirPath)
    NoParticipant <- NoParticipant + 1
    
    ### Experiment Data ###
    UserFilePattern <- list.files(pattern="*_trialCards.csv")
    TaskFilePattern <- list.files(pattern="*_answerCards.csv")
    InteractionFilepattern <- list.files(pattern="*_Interaction.csv")
    
    readUserFile <- read.csv(UserFilePattern, header=FALSE)
    readTaskFile <- read.csv(TaskFilePattern, header=FALSE)
    readInteractionFile <- read.csv(InteractionFilepattern, header=TRUE)
    
    AccuracyResult <- c()
    EuclErrorResult <- c()

    ### Calculate interaction time
    InteractionFileSubset <- readInteractionFile[(readInteractionFile$Info == "Learning Phase" | readInteractionFile$Info == "Distractor") & readInteractionFile$TrialID != "Training",]
    timeCal <- c()
    rowN <- 1
    while (rowN < nrow(InteractionFileSubset)) {
      timeCal <- c(timeCal, InteractionFileSubset[rowN + 1, 1] - InteractionFileSubset[rowN, 1])
      rowN <- rowN + 2
    }
    
    for(row in 1:nrow(readUserFile)){
      UserCards <- c()
      UserCard1 <- c()
      UserCard2 <- c()
      UserCard3 <- c()
      UserCard4 <- c()
      UserCard5 <- c()
      
      TaskCards <- c()
      TaskCard1 <- c()
      TaskCard2 <- c()
      TaskCard3 <- c()
      TaskCard4 <- c()
      TaskCard5 <- c()
      
      for(col in 1:20){
        if((col-1) %% 4 == 0){
          UserCards <- c(UserCards, readUserFile[row,col])
        }else{
          if((col-1) %/% 4 == 0){
            UserCard1 <- c(UserCard1, readUserFile[row,col])
          }else if((col-1) %/% 4 == 1){
            UserCard2 <- c(UserCard2, readUserFile[row,col])
          }else if((col-1) %/% 4 == 2){
            UserCard3 <- c(UserCard3, readUserFile[row,col])
          }else if((col-1) %/% 4 == 3){
            UserCard4 <- c(UserCard4, readUserFile[row,col])
          }else if((col-1) %/% 4 == 4){
            UserCard5 <- c(UserCard5, readUserFile[row,col])
          }
        }
      }
      
      for(col in 1:20){
        if((col-1) %% 4 == 0){
          TaskCards <- c(TaskCards, readTaskFile[row,col])
        }else{
          if((col-1) %/% 4 == 0){
            TaskCard1 <- c(TaskCard1, readTaskFile[row,col])
          }else if((col-1) %/% 4 == 1){
            TaskCard2 <- c(TaskCard2, readTaskFile[row,col])
          }else if((col-1) %/% 4 == 2){
            TaskCard3 <- c(TaskCard3, readTaskFile[row,col])
          }else if((col-1) %/% 4 == 3){
            TaskCard4 <- c(TaskCard4, readTaskFile[row,col])
          }else if((col-1) %/% 4 == 4){
            TaskCard5 <- c(TaskCard5, readTaskFile[row,col])
          }
        }
      }
      
      
      ### Data into Array ###
      UserArray <- array(c(UserCard1,UserCard2,UserCard3,UserCard4,UserCard5), dim = c(vectorDim, difficulty))
      UserDiffArray <- UserArray[,match(setdiff(UserCards,TaskCards),UserCards)]

      TaskArray <- array(c(TaskCard1,TaskCard2,TaskCard3,TaskCard4,TaskCard5), dim = c(vectorDim, difficulty))
      TaskDiffArray <- TaskArray[,match(setdiff(TaskCards,UserCards),TaskCards)]

      ### Analyse data ###
      if(length(match(setdiff(TaskCards,UserCards),TaskCards)) == 0){
        ### Result ###
        AccuracyResult <- c(AccuracyResult, 1)
        EuclErrorResult <- c(EuclErrorResult, 0)
      }else if(length(match(setdiff(TaskCards,UserCards),TaskCards)) == 1){
        ### Result ###
        AccuracyResult <- c(AccuracyResult, (difficulty - length(match(setdiff(TaskCards,UserCards),TaskCards))) / difficulty)
        EuclErrorResult <- c(EuclErrorResult, euclidean_dist(UserDiffArray, TaskDiffArray))
      }else if(length(match(setdiff(TaskCards,UserCards),TaskCards)) > 1){
        tmpVec <- c()
        for (a in 1:length(UserDiffArray[1,])) {
          for (b in 1:length(TaskDiffArray[1,])) {
            dist <- euclidean_dist(UserDiffArray[,a], TaskDiffArray[,b])
            tmpVec <- c(tmpVec,dist)
          }
        }
        DistArray <- array(tmpVec, dim = c(length(UserDiffArray[1,]),length(TaskDiffArray[1,])))
        
        ### Result ###
        AccuracyResult <- c(AccuracyResult, (difficulty - length(UserDiffArray[1,])) / difficulty)
        EuclErrorResult <- c(EuclErrorResult, HungarianSolver(DistArray)$cost)
      }
    }
    
    tmp <- data.frame(cbind(AccuracyResult, EuclErrorResult, timeCal))
    tmp$PID <- NoParticipant
    Condition1DF.tmp <- tmp[1:5, ]
    Condition2DF.tmp  <- tmp[6:10, ]
    Condition3DF.tmp  <- tmp[11:15, ]
    Condition4DF.tmp  <- tmp[16:20, ]
    
    if(par %% 4 == 1){
      Condition1DF.tmp$Furniture <- "Furniture"
      Condition2DF.tmp$Furniture <- "NoFurniture"
      Condition3DF.tmp$Furniture <- "Furniture"
      Condition4DF.tmp$Furniture <- "NoFurniture"
      
      Condition1DF.tmp$Layout <- "Regular"
      Condition2DF.tmp$Layout <- "Regular"
      Condition3DF.tmp$Layout <- "Irregular"
      Condition4DF.tmp$Layout <- "Irregular"
      
      Condition1DF.tmp$Condition <- "Furniture-Regular"
      Condition2DF.tmp$Condition <- "NoFurniture-Regular"
      Condition3DF.tmp$Condition <- "Furniture-Irregular"
      Condition4DF.tmp$Condition <- "NoFurniture-Irregular"
    }else if(par %% 4 == 2){
      Condition1DF.tmp$Furniture <- "NoFurniture"
      Condition2DF.tmp$Furniture <- "NoFurniture"
      Condition3DF.tmp$Furniture <- "Furniture"
      Condition4DF.tmp$Furniture <- "Furniture"
      
      Condition1DF.tmp$Layout <- "Regular"
      Condition2DF.tmp$Layout <- "Irregular"
      Condition3DF.tmp$Layout <- "Regular"
      Condition4DF.tmp$Layout <- "Irregular"
      
      Condition1DF.tmp$Condition <- "NoFurniture-Regular"
      Condition2DF.tmp$Condition <- "NoFurniture-Irregular"
      Condition3DF.tmp$Condition <- "Furniture-Regular"
      Condition4DF.tmp$Condition <- "Furniture-Irregular"
    }
    else if(par %% 4 == 3){
      Condition1DF.tmp$Furniture <- "Furniture"
      Condition2DF.tmp$Furniture <- "Furniture"
      Condition3DF.tmp$Furniture <- "NoFurniture"
      Condition4DF.tmp$Furniture <- "NoFurniture"
      
      Condition1DF.tmp$Layout <- "Irregular"
      Condition2DF.tmp$Layout <- "Regular"
      Condition3DF.tmp$Layout <- "Irregular"
      Condition4DF.tmp$Layout <- "Regular"
      
      Condition1DF.tmp$Condition <- "Furniture-Irregular"
      Condition2DF.tmp$Condition <- "Furniture-Regular"
      Condition3DF.tmp$Condition <- "NoFurniture-Irregular"
      Condition4DF.tmp$Condition <- "NoFurniture-Regular"
    }
    else if(par %% 4 == 0){
      Condition1DF.tmp$Furniture <- "NoFurniture"
      Condition2DF.tmp$Furniture <- "Furniture"
      Condition3DF.tmp$Furniture <- "NoFurniture"
      Condition4DF.tmp$Furniture <- "Furniture"
      
      Condition1DF.tmp$Layout <- "Irregular"
      Condition2DF.tmp$Layout <- "Irregular"
      Condition3DF.tmp$Layout <- "Regular"
      Condition4DF.tmp$Layout <- "Regular"
      
      Condition1DF.tmp$Condition <- "NoFurniture-Irregular"
      Condition2DF.tmp$Condition <- "Furniture-Irregular"
      Condition3DF.tmp$Condition <- "NoFurniture-Regular"
      Condition4DF.tmp$Condition <- "Furniture-Regular"
    }
    
    tmp <- rbind(Condition1DF.tmp, Condition2DF.tmp, Condition3DF.tmp, Condition4DF.tmp)
    
    tmp <- tmp %>% select(PID, Condition, Furniture, Layout, AccuracyResult, EuclErrorResult, timeCal)
    colnames(tmp)[5] <- "Accuracy"
    colnames(tmp)[6] <- "EuclideanError"
    colnames(tmp)[7] <- "LearningTime"
    data.accuracy.raw <- rbind(tmp, data.accuracy.raw)
  }
}

data.accuracy.raw <- data.accuracy.raw %>% mutate(PID = as.factor(PID),
                                                  Condition = as.factor(Condition), 
                                                  Furniture = as.factor(Furniture), 
                                                  Layout = as.factor(Layout))

setwd("~/R Projects/ARSpatialMemory-R")
save(data.accuracy.raw, file = "data.accuracy.raw.Rdata")