library(dplyr)

setwd("~/R Projects/ARSpatialMemory-R/Data/positionData")
WDFiles = list.files(pattern="*Raw.csv")

##################################################
######### Locomotion by Furniture #######################
for(pNum in 1:length(WDFiles)){
  mydata <- read.csv(WDFiles[pNum])
  mydata <- subset(mydata, TrialID != "Training")
  mydataLearning <- subset(mydata, TrialState == "learning")
  mydataLearning <- dplyr::select(mydataLearning, TrialID, CameraEulerAngles.y, CameraPosition.x, CameraPosition.y, CameraPosition.z)
  
  for(row in 1:nrow(mydataLearning)){
    if(mydataLearning[row,2]>180){
      mydataLearning[row,2] <- mydataLearning[row,2]-360
    }
  }
  
  FurnitureRotation = vector()
  NoFurnitureRotation = vector()
  
  FurnitureDistance = vector()
  NoFurnitureDistance = vector()
  
  for(i in 1:20){
    if((pNum == 2 && i == 12) || 
       (pNum == 2 && i == 16) || 
       (pNum == 5 && i == 19) ||
       (pNum == 6 && i == 14) ||
       (pNum == 6 && i == 15) ||
       (pNum == 7 && i == 12) ||
       (pNum == 9 && i == 6) ||
       (pNum == 9 && i == 9) ||
       (pNum == 9 && i == 10) ||
       (pNum == 11 && i == 1) ||
       (pNum == 12 && i == 12) ||
       (pNum == 12 && i == 17) ||
       (pNum == 13 && i == 1) ||
       (pNum == 14 && i == 1) ||
       (pNum == 14 && i == 17) ||
       (pNum == 15 && i == 5) ||
       (pNum == 15 && i == 6) ||
       (pNum == 15 && i == 7) ||
       (pNum == 15 && i == 9)){
      
    }else{
      df <- subset(mydataLearning, TrialID==i)
      ar = vector()
      ad = vector()
      
      for(row in 1:nrow(df)-1){
        r <- abs(df[row+1,2] - df[row,2])
        ar <- append(ar,r)
        
        d <- sqrt((df[row+1,3] - df[row,3])*(df[row+1,3] - df[row,3]) + (df[row+1,5] - df[row,5])*(df[row+1,5] - df[row,5]))
        ad <- append(ad,d)
      }
      
      if(pNum %% 4 == 1){
        if((i >= 1 && i <= 5) || (i >= 11 && i <= 15)){
          FurnitureRotation <- append(FurnitureRotation,sum(ar))
          FurnitureDistance <- append(FurnitureDistance,sum(ad))
        }else{
          NoFurnitureRotation <- append(NoFurnitureRotation,sum(ar))
          NoFurnitureDistance <- append(NoFurnitureDistance,sum(ad))
        }
      }else if(pNum %% 4 == 2){
        if(i >= 11 && i <= 20){
          FurnitureRotation <- append(FurnitureRotation,sum(ar))
          FurnitureDistance <- append(FurnitureDistance,sum(ad))
        }else{
          NoFurnitureRotation <- append(NoFurnitureRotation,sum(ar))
          NoFurnitureDistance <- append(NoFurnitureDistance,sum(ad))
        }
      }
      else if(pNum %% 4 == 3){
        if(i >= 1 && i <= 10){
          FurnitureRotation <- append(FurnitureRotation,sum(ar))
          FurnitureDistance <- append(FurnitureDistance,sum(ad))
        }else{
          NoFurnitureRotation <- append(NoFurnitureRotation,sum(ar))
          NoFurnitureDistance <- append(NoFurnitureDistance,sum(ad))
        }
      }
      else if(pNum %% 4 == 0){
        if((i >= 6 && i <= 10) || (i >= 16 && i <= 20)){
          FurnitureRotation <- append(FurnitureRotation,sum(ar))
          FurnitureDistance <- append(FurnitureDistance,sum(ad))
        }else{
          NoFurnitureRotation <- append(NoFurnitureRotation,sum(ar))
          NoFurnitureDistance <- append(NoFurnitureDistance,sum(ad))
        }
      }
    }
    assign(paste("MeanFurnitureRotation",pNum), mean(FurnitureRotation))
    assign(paste("MeanNoFurnitureRotation",pNum), mean(NoFurnitureRotation))
    assign(paste("MeanFurnitureDistance",pNum), mean(FurnitureDistance))
    assign(paste("MeanNoFurnitureDistance",pNum), mean(NoFurnitureDistance))
  }
    
}

data.locomotion.furniture <- data.frame( 
  PID = 1:16,
  Furniture = rep(c("Furniture", "NoFurniture"), each = 16),
  Rotation = c(get('MeanFurnitureRotation 1'),  get('MeanFurnitureRotation 2'), get('MeanFurnitureRotation 3'), get('MeanFurnitureRotation 4'), 
                 get('MeanFurnitureRotation 5'), get('MeanFurnitureRotation 6'), get('MeanFurnitureRotation 7'), get('MeanFurnitureRotation 8'), 
                 get('MeanFurnitureRotation 9'), get('MeanFurnitureRotation 10'), get('MeanFurnitureRotation 11'), get('MeanFurnitureRotation 12'),
                 get('MeanFurnitureRotation 13'), get('MeanFurnitureRotation 14'), get('MeanFurnitureRotation 15'), get('MeanFurnitureRotation 16'),
                 get('MeanNoFurnitureRotation 1'),  get('MeanNoFurnitureRotation 2'), get('MeanNoFurnitureRotation 3'), get('MeanNoFurnitureRotation 4'), 
                 get('MeanNoFurnitureRotation 5'), get('MeanNoFurnitureRotation 6'), get('MeanNoFurnitureRotation 7'), get('MeanNoFurnitureRotation 8'), 
                 get('MeanNoFurnitureRotation 9'), get('MeanNoFurnitureRotation 10'), get('MeanNoFurnitureRotation 11'), get('MeanNoFurnitureRotation 12'),
                 get('MeanNoFurnitureRotation 13'), get('MeanNoFurnitureRotation 14'), get('MeanNoFurnitureRotation 15'), get('MeanNoFurnitureRotation 16')),
  Distance = c(get('MeanFurnitureDistance 1'),  get('MeanFurnitureDistance 2'), get('MeanFurnitureDistance 3'), get('MeanFurnitureDistance 4'), 
               get('MeanFurnitureDistance 5'), get('MeanFurnitureDistance 6'), get('MeanFurnitureDistance 7'), get('MeanFurnitureDistance 8'), 
               get('MeanFurnitureDistance 9'), get('MeanFurnitureDistance 10'), get('MeanFurnitureDistance 11'), get('MeanFurnitureDistance 12'),
               get('MeanFurnitureDistance 13'), get('MeanFurnitureDistance 14'), get('MeanFurnitureDistance 15'), get('MeanFurnitureDistance 16'),
               get('MeanNoFurnitureDistance 1'),  get('MeanNoFurnitureDistance 2'), get('MeanNoFurnitureDistance 3'), get('MeanNoFurnitureDistance 4'), 
               get('MeanNoFurnitureDistance 5'), get('MeanNoFurnitureDistance 6'), get('MeanNoFurnitureDistance 7'), get('MeanNoFurnitureDistance 8'), 
               get('MeanNoFurnitureDistance 9'), get('MeanNoFurnitureDistance 10'), get('MeanNoFurnitureDistance 11'), get('MeanNoFurnitureDistance 12'),
               get('MeanNoFurnitureDistance 13'), get('MeanNoFurnitureDistance 14'), get('MeanNoFurnitureDistance 15'), get('MeanNoFurnitureDistance 16'))
)

data.locomotion.furniture$PID=as.factor(data.locomotion.furniture$PID)
data.locomotion.furniture$Furniture=as.factor(data.locomotion.furniture$Furniture)


setwd("~/R Projects/ARSpatialMemory-R")
save(data.locomotion.furniture, file = "data.locomotion.furniture.Rdata")


##################################################
######### Locomotion by Layout #######################
setwd("~/R Projects/ARSpatialMemory-R/Data/positionData")
WDFiles = list.files(pattern="*Raw.csv")
for(pNum in 1:length(WDFiles)){
  mydata <- read.csv(WDFiles[pNum])
  mydata <- subset(mydata, TrialID!="Training")
  mydataLearning <- subset(mydata, TrialState=="learning")
  mydataLearning <- dplyr::select(mydataLearning, TrialID, CameraEulerAngles.y, CameraPosition.x, CameraPosition.y, CameraPosition.z)
  
  for(row in 1:nrow(mydataLearning)){
    if(mydataLearning[row,2]>180){
      mydataLearning[row,2] <- mydataLearning[row,2]-360
    }
  }
  
  RegularRotation = vector()
  IrregularRotation = vector()
  
  RegularDistance = vector()
  IrregularDistance = vector()
  
  for(i in 1:20){
    if((pNum == 2 && i == 12) || 
       (pNum == 2 && i == 16) || 
       (pNum == 5 && i == 19) ||
       (pNum == 6 && i == 14) ||
       (pNum == 6 && i == 15) ||
       (pNum == 7 && i == 12) ||
       (pNum == 9 && i == 6) ||
       (pNum == 9 && i == 9) ||
       (pNum == 9 && i == 10) ||
       (pNum == 11 && i == 1) ||
       (pNum == 12 && i == 12) ||
       (pNum == 12 && i == 17) ||
       (pNum == 13 && i == 1) ||
       (pNum == 14 && i == 1) ||
       (pNum == 14 && i == 17) ||
       (pNum == 15 && i == 5) ||
       (pNum == 15 && i == 6) ||
       (pNum == 15 && i == 7) ||
       (pNum == 15 && i == 9)){
      
    }else{
      df <- subset(mydataLearning, TrialID==i)
      ar = vector()
      ad = vector()
      
      for(row in 1:nrow(df)-1){
        r <- abs(df[row+1,2] - df[row,2])
        ar <- append(ar,r)
        
        d <- sqrt((df[row+1,3] - df[row,3])*(df[row+1,3] - df[row,3]) + (df[row+1,5] - df[row,5])*(df[row+1,5] - df[row,5]))
        ad <- append(ad,d)
      }
      
      if(pNum %% 4 == 1){
        if(i >= 1 && i <= 10){
          RegularRotation <- append(RegularRotation,sum(ar))
          RegularDistance <- append(RegularDistance,sum(ad))
        }else{
          IrregularRotation <- append(IrregularRotation,sum(ar))
          IrregularDistance <- append(IrregularDistance,sum(ad))
        }
      }else if(pNum %% 4 == 2){
        if((i >= 1 && i <= 5) || (i >= 11 && i <= 15)){
          RegularRotation <- append(RegularRotation,sum(ar))
          RegularDistance <- append(RegularDistance,sum(ad))
        }else{
          IrregularRotation <- append(IrregularRotation,sum(ar))
          IrregularDistance <- append(IrregularDistance,sum(ad))
        }
      }
      else if(pNum %% 4 == 3){
        if((i >= 6 && i <= 10) || (i >= 16 && i <= 20)){
          RegularRotation <- append(RegularRotation,sum(ar))
          RegularDistance <- append(RegularDistance,sum(ad))
        }else{
          IrregularRotation <- append(IrregularRotation,sum(ar))
          IrregularDistance <- append(IrregularDistance,sum(ad))
        }
      }
      else if(pNum %% 4 == 0){
        if(i >= 11 && i <= 20){
          RegularRotation <- append(RegularRotation,sum(ar))
          RegularDistance <- append(RegularDistance,sum(ad))
        }else{
          IrregularRotation <- append(IrregularRotation,sum(ar))
          IrregularDistance <- append(IrregularDistance,sum(ad))
        }
      }
    }
    
    
    assign(paste("MeanRegularRotation",pNum), mean(RegularRotation))
    assign(paste("MeanIrregularRotation",pNum), mean(IrregularRotation))
    assign(paste("MeanRegularDistance",pNum), mean(RegularDistance))
    assign(paste("MeanIrregularDistance",pNum), mean(IrregularDistance))
  }
  
}

data.locomotion.layout <- data.frame( 
  PID = 1:16,
  Layout = rep(c("Regular", "Irregular"), each = 16),
  Rotation = c(get('MeanRegularRotation 1'),  get('MeanRegularRotation 2'), get('MeanRegularRotation 3'), get('MeanRegularRotation 4'), 
               get('MeanRegularRotation 5'), get('MeanRegularRotation 6'), get('MeanRegularRotation 7'), get('MeanRegularRotation 8'), 
               get('MeanRegularRotation 9'), get('MeanRegularRotation 10'), get('MeanRegularRotation 11'), get('MeanRegularRotation 12'),
               get('MeanRegularRotation 13'), get('MeanRegularRotation 14'), get('MeanRegularRotation 15'), get('MeanRegularRotation 16'),
               get('MeanIrregularRotation 1'),  get('MeanIrregularRotation 2'), get('MeanIrregularRotation 3'), get('MeanIrregularRotation 4'), 
               get('MeanIrregularRotation 5'), get('MeanIrregularRotation 6'), get('MeanIrregularRotation 7'), get('MeanIrregularRotation 8'), 
               get('MeanIrregularRotation 9'), get('MeanIrregularRotation 10'), get('MeanIrregularRotation 11'), get('MeanIrregularRotation 12'),
               get('MeanIrregularRotation 13'), get('MeanIrregularRotation 14'), get('MeanIrregularRotation 15'), get('MeanIrregularRotation 16')),
  Distance = c(get('MeanRegularDistance 1'),  get('MeanRegularDistance 2'), get('MeanRegularDistance 3'), get('MeanRegularDistance 4'), 
               get('MeanRegularDistance 5'), get('MeanRegularDistance 6'), get('MeanRegularDistance 7'), get('MeanRegularDistance 8'), 
               get('MeanRegularDistance 9'), get('MeanRegularDistance 10'), get('MeanRegularDistance 11'), get('MeanRegularDistance 12'),
               get('MeanRegularDistance 13'), get('MeanRegularDistance 14'), get('MeanRegularDistance 15'), get('MeanRegularDistance 16'),
               get('MeanIrregularDistance 1'),  get('MeanIrregularDistance 2'), get('MeanIrregularDistance 3'), get('MeanIrregularDistance 4'), 
               get('MeanIrregularDistance 5'), get('MeanIrregularDistance 6'), get('MeanIrregularDistance 7'), get('MeanIrregularDistance 8'), 
               get('MeanIrregularDistance 9'), get('MeanIrregularDistance 10'), get('MeanIrregularDistance 11'), get('MeanIrregularDistance 12'),
               get('MeanIrregularDistance 13'), get('MeanIrregularDistance 14'), get('MeanIrregularDistance 15'), get('MeanIrregularDistance 16'))
)

data.locomotion.layout$PID=as.factor(data.locomotion.layout$PID)
data.locomotion.layout$Layout=as.factor(data.locomotion.layout$Layout)


setwd("~/R Projects/ARSpatialMemory-R")
save(data.locomotion.layout, file = "data.locomotion.layout.Rdata")

##################################################
######### Locomotion by Condition #######################
setwd("~/R Projects/ARSpatialMemory-R/Data/positionData")
WDFiles = list.files(pattern="*Raw.csv")
for(pNum in 1:length(WDFiles)){
  mydata <- read.csv(WDFiles[pNum])
  mydata <- subset(mydata, TrialID != "Training")
  mydataLearning <- subset(mydata, TrialState == "learning")
  mydataLearning <- dplyr::select(mydataLearning, TrialID, CameraEulerAngles.y, CameraPosition.x, CameraPosition.y, CameraPosition.z)
  
  for(row in 1:nrow(mydataLearning)){
    if(mydataLearning[row,2]>180){
      mydataLearning[row,2] <- mydataLearning[row,2]-360
    }
  }
  
  Furniture_RegularRotation = vector()
  NoFurniture_RegularRotation = vector()
  Furniture_IrregularRotation = vector()
  NoFurniture_IrregularRotation = vector()
  
  Furniture_RegularDistance = vector()
  NoFurniture_RegularDistance  = vector()
  Furniture_IrregularDistance  = vector()
  NoFurniture_IrregularDistance = vector()
  
  for(i in 1:20){
    if((pNum == 2 && i == 12) || 
       (pNum == 2 && i == 16) || 
       (pNum == 5 && i == 19) ||
       (pNum == 6 && i == 14) ||
       (pNum == 6 && i == 15) ||
       (pNum == 7 && i == 12) ||
       (pNum == 9 && i == 6) ||
       (pNum == 9 && i == 9) ||
       (pNum == 9 && i == 10) ||
       (pNum == 11 && i == 1) ||
       (pNum == 12 && i == 12) ||
       (pNum == 12 && i == 17) ||
       (pNum == 13 && i == 1) ||
       (pNum == 14 && i == 1) ||
       (pNum == 14 && i == 17) ||
       (pNum == 15 && i == 5) ||
       (pNum == 15 && i == 6) ||
       (pNum == 15 && i == 7) ||
       (pNum == 15 && i == 9)){
      
    }else{
      df <- subset(mydataLearning, TrialID==i)
      ar = vector()
      ad = vector()
      
      for(row in 1:nrow(df)-1){
        r <- abs(df[row+1,2] - df[row,2])
        ar <- append(ar,r)
        
        d <- sqrt((df[row+1,3] - df[row,3])*(df[row+1,3] - df[row,3]) + (df[row+1,5] - df[row,5])*(df[row+1,5] - df[row,5]))
        ad <- append(ad,d)
      }
      
      if(pNum %% 4 == 1){
        if(i >= 1 && i <= 5){
          Furniture_RegularRotation <- append(Furniture_RegularRotation,sum(ar))
          Furniture_RegularDistance <- append(Furniture_RegularDistance,sum(ad))
        }else if(i >= 6 && i <= 10){
          NoFurniture_RegularRotation <- append(NoFurniture_RegularRotation,sum(ar))
          NoFurniture_RegularDistance <- append(NoFurniture_RegularDistance,sum(ad))
        }else if(i >= 11 && i <= 15){
          Furniture_IrregularRotation <- append(Furniture_IrregularRotation,sum(ar))
          Furniture_IrregularDistance <- append(Furniture_IrregularDistance,sum(ad))
        }else{
          NoFurniture_IrregularRotation <- append(NoFurniture_IrregularRotation,sum(ar))
          NoFurniture_IrregularDistance <- append(NoFurniture_IrregularDistance,sum(ad))
        }
      }else if(pNum %% 4 == 2){
        if(i >= 11 && i <= 15){
          Furniture_RegularRotation <- append(Furniture_RegularRotation,sum(ar))
          Furniture_RegularDistance <- append(Furniture_RegularDistance,sum(ad))
        }else if(i >= 1 && i <= 5){
          NoFurniture_RegularRotation <- append(NoFurniture_RegularRotation,sum(ar))
          NoFurniture_RegularDistance <- append(NoFurniture_RegularDistance,sum(ad))
        }else if(i >= 16 && i <= 20){
          Furniture_IrregularRotation <- append(Furniture_IrregularRotation,sum(ar))
          Furniture_IrregularDistance <- append(Furniture_IrregularDistance,sum(ad))
        }else{
          NoFurniture_IrregularRotation <- append(NoFurniture_IrregularRotation,sum(ar))
          NoFurniture_IrregularDistance <- append(NoFurniture_IrregularDistance,sum(ad))
        }
      }
      else if(pNum %% 4 == 3){
        if(i >= 6 && i <= 10){
          Furniture_RegularRotation <- append(Furniture_RegularRotation,sum(ar))
          Furniture_RegularDistance <- append(Furniture_RegularDistance,sum(ad))
        }else if(i >= 16 && i <= 20){
          NoFurniture_RegularRotation <- append(NoFurniture_RegularRotation,sum(ar))
          NoFurniture_RegularDistance <- append(NoFurniture_RegularDistance,sum(ad))
        }else if(i >= 1 && i <= 5){
          Furniture_IrregularRotation <- append(Furniture_IrregularRotation,sum(ar))
          Furniture_IrregularDistance <- append(Furniture_IrregularDistance,sum(ad))
        }else{
          NoFurniture_IrregularRotation <- append(NoFurniture_IrregularRotation,sum(ar))
          NoFurniture_IrregularDistance <- append(NoFurniture_IrregularDistance,sum(ad))
        }
      }
      else if(pNum %% 4 == 0){
        if(i >= 16 && i <= 20){
          Furniture_RegularRotation <- append(Furniture_RegularRotation,sum(ar))
          Furniture_RegularDistance <- append(Furniture_RegularDistance,sum(ad))
        }else if(i >= 11 && i <= 15){
          NoFurniture_RegularRotation <- append(NoFurniture_RegularRotation,sum(ar))
          NoFurniture_RegularDistance <- append(NoFurniture_RegularDistance,sum(ad))
        }else if(i >= 6 && i <= 10){
          Furniture_IrregularRotation <- append(Furniture_IrregularRotation,sum(ar))
          Furniture_IrregularDistance <- append(Furniture_IrregularDistance,sum(ad))
        }else{
          NoFurniture_IrregularRotation <- append(NoFurniture_IrregularRotation,sum(ar))
          NoFurniture_IrregularDistance <- append(NoFurniture_IrregularDistance,sum(ad))
        }
      }
    }
    
    assign(paste("MeanFurniture_RegularRotation",pNum), mean(Furniture_RegularRotation))
    assign(paste("MeanNoFurniture_RegularRotation",pNum), mean(NoFurniture_RegularRotation))
    assign(paste("MeanFurniture_IrregularRotation",pNum), mean(Furniture_IrregularRotation))
    assign(paste("MeanNoFurniture_IrregularRotation",pNum), mean(NoFurniture_IrregularRotation))
    
    assign(paste("MeanFurniture_RegularDistance",pNum), mean(Furniture_RegularDistance))
    assign(paste("MeanNoFurniture_RegularDistance",pNum), mean(NoFurniture_RegularDistance))
    assign(paste("MeanFurniture_IrregularDistance",pNum), mean(Furniture_IrregularDistance))
    assign(paste("MeanNoFurniture_IrregularDistance",pNum), mean(NoFurniture_IrregularDistance))
  }
  
}

data.locomotion.condition <- data.frame( 
  PID = 1:16,
  Condition = rep(c("Furniture-Regular", "NoFurniture-Regular", "Furniture-Irregular", "NoFurniture-Irregular"), each = 16),
  Rotation = c(get('MeanFurniture_RegularRotation 1'),  get('MeanFurniture_RegularRotation 2'), get('MeanFurniture_RegularRotation 3'), get('MeanFurniture_RegularRotation 4'), 
               get('MeanFurniture_RegularRotation 5'), get('MeanFurniture_RegularRotation 6'), get('MeanFurniture_RegularRotation 7'), get('MeanFurniture_RegularRotation 8'), 
               get('MeanFurniture_RegularRotation 9'), get('MeanFurniture_RegularRotation 10'), get('MeanFurniture_RegularRotation 11'), get('MeanFurniture_RegularRotation 12'),
               get('MeanFurniture_RegularRotation 13'), get('MeanFurniture_RegularRotation 14'), get('MeanFurniture_RegularRotation 15'), get('MeanFurniture_RegularRotation 16'),
               get('MeanNoFurniture_RegularRotation 1'),  get('MeanNoFurniture_RegularRotation 2'), get('MeanNoFurniture_RegularRotation 3'), get('MeanNoFurniture_RegularRotation 4'), 
               get('MeanNoFurniture_RegularRotation 5'), get('MeanNoFurniture_RegularRotation 6'), get('MeanNoFurniture_RegularRotation 7'), get('MeanNoFurniture_RegularRotation 8'), 
               get('MeanNoFurniture_RegularRotation 9'), get('MeanNoFurniture_RegularRotation 10'), get('MeanNoFurniture_RegularRotation 11'), get('MeanNoFurniture_RegularRotation 12'),
               get('MeanNoFurniture_RegularRotation 13'), get('MeanNoFurniture_RegularRotation 14'), get('MeanNoFurniture_RegularRotation 15'), get('MeanNoFurniture_RegularRotation 16'),
               get('MeanFurniture_IrregularRotation 1'),  get('MeanFurniture_IrregularRotation 2'), get('MeanFurniture_IrregularRotation 3'), get('MeanFurniture_IrregularRotation 4'), 
               get('MeanFurniture_IrregularRotation 5'), get('MeanFurniture_IrregularRotation 6'), get('MeanFurniture_IrregularRotation 7'), get('MeanFurniture_IrregularRotation 8'), 
               get('MeanFurniture_IrregularRotation 9'), get('MeanFurniture_IrregularRotation 10'), get('MeanFurniture_IrregularRotation 11'), get('MeanFurniture_IrregularRotation 12'),
               get('MeanFurniture_IrregularRotation 13'), get('MeanFurniture_IrregularRotation 14'), get('MeanFurniture_IrregularRotation 15'), get('MeanFurniture_IrregularRotation 16'),
               get('MeanNoFurniture_IrregularRotation 1'),  get('MeanNoFurniture_IrregularRotation 2'), get('MeanNoFurniture_IrregularRotation 3'), get('MeanNoFurniture_IrregularRotation 4'), 
               get('MeanNoFurniture_IrregularRotation 5'), get('MeanNoFurniture_IrregularRotation 6'), get('MeanNoFurniture_IrregularRotation 7'), get('MeanNoFurniture_IrregularRotation 8'), 
               get('MeanNoFurniture_IrregularRotation 9'), get('MeanNoFurniture_IrregularRotation 10'), get('MeanNoFurniture_IrregularRotation 11'), get('MeanNoFurniture_IrregularRotation 12'),
               get('MeanNoFurniture_IrregularRotation 13'), get('MeanNoFurniture_IrregularRotation 14'), get('MeanNoFurniture_IrregularRotation 15'), get('MeanNoFurniture_IrregularRotation 16')),
  Distance = c(get('MeanFurniture_RegularDistance 1'),  get('MeanFurniture_RegularDistance 2'), get('MeanFurniture_RegularDistance 3'), get('MeanFurniture_RegularDistance 4'), 
               get('MeanFurniture_RegularDistance 5'), get('MeanFurniture_RegularDistance 6'), get('MeanFurniture_RegularDistance 7'), get('MeanFurniture_RegularDistance 8'), 
               get('MeanFurniture_RegularDistance 9'), get('MeanFurniture_RegularDistance 10'), get('MeanFurniture_RegularDistance 11'), get('MeanFurniture_RegularDistance 12'),
               get('MeanFurniture_RegularDistance 13'), get('MeanFurniture_RegularDistance 14'), get('MeanFurniture_RegularDistance 15'), get('MeanFurniture_RegularDistance 16'),
               get('MeanNoFurniture_RegularDistance 1'),  get('MeanNoFurniture_RegularDistance 2'), get('MeanNoFurniture_RegularDistance 3'), get('MeanNoFurniture_RegularDistance 4'), 
               get('MeanNoFurniture_RegularDistance 5'), get('MeanNoFurniture_RegularDistance 6'), get('MeanNoFurniture_RegularDistance 7'), get('MeanNoFurniture_RegularDistance 8'), 
               get('MeanNoFurniture_RegularDistance 9'), get('MeanNoFurniture_RegularDistance 10'), get('MeanNoFurniture_RegularDistance 11'), get('MeanNoFurniture_RegularDistance 12'),
               get('MeanNoFurniture_RegularDistance 13'), get('MeanNoFurniture_RegularDistance 14'), get('MeanNoFurniture_RegularDistance 15'), get('MeanNoFurniture_RegularDistance 16'),
               get('MeanFurniture_IrregularDistance 1'),  get('MeanFurniture_IrregularDistance 2'), get('MeanFurniture_IrregularDistance 3'), get('MeanFurniture_IrregularDistance 4'), 
               get('MeanFurniture_IrregularDistance 5'), get('MeanFurniture_IrregularDistance 6'), get('MeanFurniture_IrregularDistance 7'), get('MeanFurniture_IrregularDistance 8'), 
               get('MeanFurniture_IrregularDistance 9'), get('MeanFurniture_IrregularDistance 10'), get('MeanFurniture_IrregularDistance 11'), get('MeanFurniture_IrregularDistance 12'),
               get('MeanFurniture_IrregularDistance 13'), get('MeanFurniture_IrregularDistance 14'), get('MeanFurniture_IrregularDistance 15'), get('MeanFurniture_IrregularDistance 16'),
               get('MeanNoFurniture_IrregularDistance 1'),  get('MeanNoFurniture_IrregularDistance 2'), get('MeanNoFurniture_IrregularDistance 3'), get('MeanNoFurniture_IrregularDistance 4'), 
               get('MeanNoFurniture_IrregularDistance 5'), get('MeanNoFurniture_IrregularDistance 6'), get('MeanNoFurniture_IrregularDistance 7'), get('MeanNoFurniture_IrregularDistance 8'), 
               get('MeanNoFurniture_IrregularDistance 9'), get('MeanNoFurniture_IrregularDistance 10'), get('MeanNoFurniture_IrregularDistance 11'), get('MeanNoFurniture_IrregularDistance 12'),
               get('MeanNoFurniture_IrregularDistance 13'), get('MeanNoFurniture_IrregularDistance 14'), get('MeanNoFurniture_IrregularDistance 15'), get('MeanNoFurniture_IrregularDistance 16'))
)

data.locomotion.condition$PID=as.factor(data.locomotion.condition$PID)
data.locomotion.condition$Condition=as.factor(data.locomotion.condition$Condition)


setwd("~/R Projects/ARSpatialMemory-R")
save(data.locomotion.condition, file = "data.locomotion.condition.Rdata")
