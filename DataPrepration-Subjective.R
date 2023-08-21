library(tidyverse)

setwd("~/R Projects/ARSpatialMemory-R")

dataF.IREG <- read.csv("F_IREG.csv")
dataF.REG <- read.csv("F_REG.csv")
dataNF.IRE <- read.csv("NF_IRE.csv")
dataNF.REG <- read.csv("NF_REG.csv")


formatData <- function(d, col, fur, reg){
  colnames(d) <- c("Timestamp", "PID", "Mental", "Physical", "Temporal", "Performance", "Effort", "Frustration")
  d <- d %>% select(Timestamp:Frustration) %>% 
    filter(PID != "102") 
  d$Condition = col
  d$Furniture = fur
  d$Layout = reg
  return(d)
}

dataF.IREG <- formatData(dataF.IREG, "Furniture-Irregular", "Furniture", "Irregular")
dataF.REG <- formatData(dataF.REG, "Furniture-Regular", "Furniture", "Regular")
dataNF.IRE <- formatData(dataNF.IRE, "NoFurniture-Irregular", "NoFurniture", "Irregular")
dataNF.REG <- formatData(dataNF.REG, "NoFurniture-Regular", "NoFurniture", "Regular")

data.subjective <- rbind(dataF.IREG, dataF.REG, dataNF.IRE, dataNF.REG)

data.subjective$Mean <- rowMeans(data.subjective[,3:8])

data.subjective <- data.subjective %>% 
  select(Timestamp, PID, Mental, Physical, Temporal, Performance, Effort, Frustration, Mean, Condition, Furniture, Layout) %>% 
  mutate(PID= as.factor(PID), 
         Condition = as.factor(Condition),
         Furniture = as.factor(Furniture),
         Layout = as.factor(Layout))

save(data.subjective, file = "data.subjective.Rdata")


############################
data.rating <- read.csv("ConditionRating_Pivoted.csv")
colnames(data.rating) <- c("Condition", "PID", "Rating", "Furniture", "Layout")
data.rating$PID <- as.factor(data.rating$PID)
data.rating$Furniture <- as.factor(data.rating$Furniture)
data.rating$Layout <- as.factor(data.rating$Layout)

save(data.rating, file = "data.rating.Rdata")
