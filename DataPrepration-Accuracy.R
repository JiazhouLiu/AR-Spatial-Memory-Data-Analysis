library(RcppHungarian)
library(dplyr)
library(tidyverse)

### Functions ###
euclidean_dist <- function(x, y) sqrt(sum((x - y)^2))


### Pre-defined variables ###
set.seed(593903)
difficulty <- 5
vectorDim <- 3
removeDiscardedTrials <- TRUE
removeDiscardedTrialsTimeout <- 30

### initiate data frames for analysis
AccuracyDF <- data.frame(
  Participant = rep(c(1:16), each = 4),
  Condition = rep(c("Has+Regular", "No+Irregular", "Has+Irregular", "No+Regular"), times = 16),
  Furniture = rep(c("Has Furniture", "No Furniture", "Has Furniture", "No Furniture"), times = 16),
  Layout = rep(c("Regular", "Irregular", "Irregular", "Regular"), times = 16),
  Accuracy = rep(c(0), times = 64),
  Accuracy_Modif = rep(c(0), times = 64),
  EuclideanError = rep(c(0), times = 64)
)

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
    AccuracyResult_Modif <- c()

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
      
      ### calculate modified accuracy
      Usercards_Modif <- UserCards %% 12
      TaskCards_Modif <- TaskCards %% 12
      
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
      
      ### calculate modif accuracy
      AccuracyResult_Modif <- c(AccuracyResult_Modif, (difficulty - length(match(setdiff(TaskCards_Modif,Usercards_Modif),TaskCards_Modif))) / difficulty)
    }
    
    FinalResult <- data.frame(cbind(AccuracyResult, AccuracyResult_Modif, EuclErrorResult, timeCal))
    
################################## write to CSV for presentation ########################
    #FinalResultTable <- rbind(c("NA", "NA", "NA"), FinalResult[1:5,], c("NA", "NA", "NA"), FinalResult[6:10,], c("NA", "NA", "NA"), FinalResult[11:15,], c("NA", "NA", "NA"), FinalResult[16:20,])
    
    #write.table(FinalResultTable, paste(c("../FinalResult_", par, ".csv"), collapse = ""), sep = ",", col.names = FALSE, row.names = FALSE)
    
################################## write to DataFrame for analysis #####################
    Condition1DF <- FinalResult[1:5, ]
    Condition2DF <- FinalResult[6:10, ]
    Condition3DF <- FinalResult[11:15, ]
    Condition4DF <- FinalResult[16:20, ]
    
    if(removeDiscardedTrials){
      Condition1DF<- Condition1DF[Condition1DF$timeCal < removeDiscardedTrialsTimeout, ]
      Condition2DF<- Condition2DF[Condition2DF$timeCal < removeDiscardedTrialsTimeout, ]
      Condition3DF<- Condition3DF[Condition3DF$timeCal < removeDiscardedTrialsTimeout, ]
      Condition4DF<- Condition4DF[Condition4DF$timeCal < removeDiscardedTrialsTimeout, ]
    }

    condition1Result_Accuracy <- mean(Condition1DF$AccuracyResult)
    condition2Result_Accuracy <- mean(Condition2DF$AccuracyResult)
    condition3Result_Accuracy <- mean(Condition3DF$AccuracyResult)
    condition4Result_Accuracy <- mean(Condition4DF$AccuracyResult)
    
    condition1Result_Accuracy_Modif <- mean(Condition1DF$AccuracyResult_Modif)
    condition2Result_Accuracy_Modif <- mean(Condition2DF$AccuracyResult_Modif)
    condition3Result_Accuracy_Modif <- mean(Condition3DF$AccuracyResult_Modif)
    condition4Result_Accuracy_Modif <- mean(Condition4DF$AccuracyResult_Modif)
    
    condition1Result_EuclError <- mean(Condition1DF$EuclErrorResult)
    condition2Result_EuclError <- mean(Condition2DF$EuclErrorResult)
    condition3Result_EuclError <- mean(Condition3DF$EuclErrorResult)
    condition4Result_EuclError <- mean(Condition4DF$EuclErrorResult)
    
    if(par %% 4 == 1){
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "Has Furniture" & Layout == "Regular", condition1Result_Accuracy))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "No Furniture" & Layout == "Regular", condition2Result_Accuracy))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "Has Furniture" & Layout == "Irregular", condition3Result_Accuracy))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "No Furniture" & Layout == "Irregular", condition4Result_Accuracy))
      
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "Has Furniture" & Layout == "Regular", condition1Result_Accuracy_Modif))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "No Furniture" & Layout == "Regular", condition2Result_Accuracy_Modif))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "Has Furniture" & Layout == "Irregular", condition3Result_Accuracy_Modif))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "No Furniture" & Layout == "Irregular", condition4Result_Accuracy_Modif))
      
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "Has Furniture" & Layout == "Regular", condition1Result_EuclError))
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "No Furniture" & Layout == "Regular", condition2Result_EuclError))
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "Has Furniture" & Layout == "Irregular", condition3Result_EuclError))
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "No Furniture" & Layout == "Irregular", condition4Result_EuclError))
    }else if(par %% 4 == 2){
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "No Furniture" & Layout == "Regular", condition1Result_Accuracy))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "No Furniture" & Layout == "Irregular", condition2Result_Accuracy))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "Has Furniture" & Layout == "Regular", condition3Result_Accuracy))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "Has Furniture" & Layout == "Irregular", condition4Result_Accuracy))

      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "No Furniture" & Layout == "Regular", condition1Result_Accuracy_Modif))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "No Furniture" & Layout == "Irregular", condition2Result_Accuracy_Modif))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "Has Furniture" & Layout == "Regular", condition3Result_Accuracy_Modif))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "Has Furniture" & Layout == "Irregular", condition4Result_Accuracy_Modif))
      
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "No Furniture" & Layout == "Regular", condition1Result_EuclError))
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "No Furniture" & Layout == "Irregular", condition2Result_EuclError))
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "Has Furniture" & Layout == "Regular", condition3Result_EuclError))
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "Has Furniture" & Layout == "Irregular", condition4Result_EuclError))
    }
    else if(par %% 4 == 3){
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "Has Furniture" & Layout == "Irregular", condition1Result_Accuracy))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "Has Furniture" & Layout == "Regular", condition2Result_Accuracy))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "No Furniture" & Layout == "Irregular", condition3Result_Accuracy))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "No Furniture" & Layout == "Regular", condition4Result_Accuracy))
      
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "Has Furniture" & Layout == "Irregular", condition1Result_Accuracy_Modif))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "Has Furniture" & Layout == "Regular", condition2Result_Accuracy_Modif))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "No Furniture" & Layout == "Irregular", condition3Result_Accuracy_Modif))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "No Furniture" & Layout == "Regular", condition4Result_Accuracy_Modif))
      
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "Has Furniture" & Layout == "Irregular", condition1Result_EuclError))
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "Has Furniture" & Layout == "Regular", condition2Result_EuclError))
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "No Furniture" & Layout == "Irregular", condition3Result_EuclError))
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "No Furniture" & Layout == "Regular", condition4Result_EuclError))
    }
    else if(par %% 4 == 0){
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "No Furniture" & Layout == "Irregular", condition1Result_Accuracy))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "Has Furniture" & Layout == "Irregular", condition2Result_Accuracy))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "No Furniture" & Layout == "Regular", condition3Result_Accuracy))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy = replace(Accuracy, Participant == par & Furniture == "Has Furniture" & Layout == "Regular", condition4Result_Accuracy))
      
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "No Furniture" & Layout == "Irregular", condition1Result_Accuracy_Modif))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "Has Furniture" & Layout == "Irregular", condition2Result_Accuracy_Modif))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "No Furniture" & Layout == "Regular", condition3Result_Accuracy_Modif))
      AccuracyDF <- AccuracyDF %>%
        mutate(Accuracy_Modif = replace(Accuracy_Modif, Participant == par & Furniture == "Has Furniture" & Layout == "Regular", condition4Result_Accuracy_Modif))
      
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "No Furniture" & Layout == "Irregular", condition1Result_EuclError))
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "Has Furniture" & Layout == "Irregular", condition2Result_EuclError))
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "No Furniture" & Layout == "Regular", condition3Result_EuclError))
      AccuracyDF <- AccuracyDF %>%
        mutate(EuclideanError = replace(EuclideanError, Participant == par & Furniture == "Has Furniture" & Layout == "Regular", condition4Result_EuclError))
    }
  }
}

## remove non-participated rows
data.accuracy <- filter(AccuracyDF, AccuracyDF$Accuracy != 0)
data.accuracy <- data.accuracy %>% mutate(Condition = as.factor(Condition), 
                                              Furniture = as.factor(Furniture), 
                                              Layout = as.factor(Layout))

data.accuracy.female <- filter(data.accuracy, data.accuracy$Participant <= 8)
data.accuracy.male  <- filter(data.accuracy, data.accuracy$Participant > 8)

data.accuracy <- data.accuracy %>% mutate(Participant = as.factor(Participant))
data.accuracy.female <- data.accuracy.female %>% mutate(Participant = as.factor(Participant))
data.accuracy.male <- data.accuracy.male %>% mutate(Participant = as.factor(Participant))

setwd("~/R Projects/ARSpatialMemory-R")
save(data.accuracy, file = "data.accuracy.Rdata")
save(data.accuracy.female, file = "data.accuracy.female.Rdata")
save(data.accuracy.male, file = "data.accuracy.male.Rdata")