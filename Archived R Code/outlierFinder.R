#<- data.frame(
#  Participant = rep(c(1:16), each = 20),
#  Condition = rep(c("Has+Regular", "No+Irregular", "Has+Irregular", "No+Regular"), times = 80),
#  Furniture = rep(c("Has Furniture", "No Furniture", "Has Furniture", "No Furniture"), times = 80),
#  Layout = rep(c("Regular", "Irregular", "Irregular", "Regular"), times = 80),
#  Accuracy = rep(c(0), times = 320),
#  EuclideanError = rep(c(0), times = 320),
#  LearningTime = rep(c(0), times = 320)
#)

NoParticipant <- 0
timeCal <- c()
### Loop for all participant files
for(par in 1:16){
  dirPath <- paste(c("~/R Projects/ARSpatialMemory-R/Data/participant", par), collapse = " ")
  if(dir.exists(dirPath)){
    setwd(dirPath)
    NoParticipant <- NoParticipant + 1
    
    ### Experiment Data ###
    InteractionFilepattern <- list.files(pattern="*_Interaction.csv")
    readInteractionFile <- read.csv(InteractionFilepattern, header=TRUE)
    InteractionFileSubset <- readInteractionFile[(readInteractionFile$Info == "Learning Phase" | readInteractionFile$Info == "Distractor") & readInteractionFile$TrialID != "Training",]
    
    rowN <- 1
    while (rowN < nrow(InteractionFileSubset)) {
      timeCal <- c(timeCal, InteractionFileSubset[rowN + 1, 1] - InteractionFileSubset[rowN, 1])
      rowN <- rowN + 2
    }
  }
}


library(outliers)
grubbs.test(timeCal)

summary(timeCal)
boxplot(timeCal)

df.timeCal <- data.frame(timeCal)

df.timeCal <- data.frame(df.timeCal, abs((df.timeCal[,1] - mean(df.timeCal[,1])))/sd(df.timeCal[,1]))
colnames(df.timeCal) <- c("x", "tau")
df.timeCal

df.timeCal <- data.frame(df.timeCal, abs((df.timeCal[,1] - mean(df.timeCal[,1])))/sd(df.timeCal[,1]) > 2.0)
colnames(df.timeCal) <- c("x", "tau", "outlier")
df.timeCal

df.timeCal[df.timeCal$outlier==TRUE,]





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

removeDiscardedTrials <- FALSE
removeDiscardedTrialsTimeout <- 30