library("dplyr")

setwd("~/R Projects/ARSpatialMemory-R/Data/interactionData")
WDFiles = list.files(pattern="*Interaction.csv")

data.recallTime.raw <- data.frame()

for (i in 1:length(WDFiles)){
    mydata <- read.csv(WDFiles[i])
    mydata <- select(mydata, TimeSinceStart, TrialID, FurnitureCondition, AlignmentCondition, Info)
    mydata <- subset(mydata, TrialID!="Training")
    mydata <- subset(mydata, Info=="Recall Phase" | Info=="Result")
    
    differenceDF <- data.frame(matrix(ncol = 3, nrow = 0))
    
    for(j in seq(1, 40, 2)){
      difference <- mydata[j+1,"TimeSinceStart"] - mydata[j,"TimeSinceStart"]
      differenceDF <- rbind(differenceDF, c(mydata[j, "TrialID"], mydata[j, "FurnitureCondition"], mydata[j, "AlignmentCondition"], difference))
    }
    colnames(differenceDF) <- c("TrialID", "Furniture", "Layout", "AnswerTime")
    differenceDF$PID <- i
    differenceDF$Condition <- paste(differenceDF$Furniture, differenceDF$Layout, sep="-")
    
    differenceDF <- differenceDF %>% 
      select(PID, TrialID, Furniture, Layout, Condition, AnswerTime) %>% 
      mutate(PID= as.factor(PID), 
             TrialID = as.factor(TrialID),
             Condition = as.factor(Condition),
             Furniture = as.factor(Furniture),
             Layout = as.factor(Layout),
             AnswerTime = as.numeric(AnswerTime))
    
    data.recallTime.raw <- rbind(data.recallTime.raw, differenceDF)
}



setwd("~/R Projects/ARSpatialMemory-R")
save(data.recallTime.raw, file = "data.recallTime.raw.Rdata")

  