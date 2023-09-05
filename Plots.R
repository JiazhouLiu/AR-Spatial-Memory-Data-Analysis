error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

data.accuracy$CardsIncorrect <- 1 - data.accuracy$Accuracy

d <- Rmisc::summarySE(data.accuracy, measurevar = "CardsIncorrect", groupvars = c("Furniture"))
ggplot(d, aes(x = Furniture, y = CardsIncorrect, fill = Furniture)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_errorbar(aes(ymin=CardsIncorrect-se, ymax=CardsIncorrect+se), width = 0.4, alpha=0.9) + 
  scale_fill_manual(values=c("#66c2a5", "#fc8d62")) + 
  labs(y = "Cards Incorrect", x = "") + 
  theme_minimal()


######################################################
d <- Rmisc::summarySE(data.accuracy, measurevar = "CardsIncorrect", groupvars = c("Condition"))
ggplot(d, aes(x = Condition, y = CardsIncorrect, fill = Condition)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_errorbar(aes(ymin=CardsIncorrect-se, ymax=CardsIncorrect+se), width = 0.4, alpha=0.9) + 
  scale_fill_manual(values=c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00")) + 
  labs(y = "Cards Incorrect", x = "") + 
  theme_minimal()

######################################################
d <- Rmisc::summarySE(data.accuracy, measurevar = "EuclideanError", groupvars = c("Furniture"))
ggplot(d, aes(x = Furniture, y = EuclideanError, fill = Furniture)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_errorbar(aes(ymin=EuclideanError-se, ymax=EuclideanError+se), width = 0.4, alpha=0.9) + 
  scale_fill_manual(values=c("#66c2a5", "#fc8d62"))+ 
  labs(y = "Euclidean Distance Error", x = "") + 
  theme_minimal()

######################################################
d <- Rmisc::summarySE(data.accuracy, measurevar = "EuclideanError", groupvars = c("Condition"))
ggplot(d, aes(x = Condition, y = EuclideanError, fill = Condition)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_errorbar(aes(ymin=EuclideanError-se, ymax=EuclideanError+se), width = 0.4, alpha=0.9) + 
  scale_fill_manual(values=c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+ 
  labs(y = "Euclidean Distance Error", x = "") + 
  theme_minimal()


######################################################
mean_Chart <- aggregate(cbind(Mental, Physical, Temporal, Performance, Effort, Frustration, Mean)~Furniture , data=data.subjective , mean)
rownames(mean_Chart) <- mean_Chart[,1]
mean_Chart <- as.matrix(mean_Chart[,-1])

lim <- 1.2*max(mean_Chart)

error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

stdev_Chart <- aggregate(cbind(Mental, Physical, Temporal, Performance, Effort, Frustration, Mean)~Furniture , data=data.subjective , sd)
rownames(stdev_Chart) <- stdev_Chart[,1]
se_Baseline <- as.matrix(stdev_Chart[,-1]) / sqrt(12)

ze_barplot <- barplot(mean_Chart , beside=T , legend.text=T,col=c("#66c2a5", "#fc8d62") , ylim=c(0,lim) , ylab="NASA-TLX Score", args.legend = list(x = "topright", inset = c(0, -0.15)), cex.names=0.8)
error.bar(ze_barplot, mean_Chart, se_Baseline)

######################################################
mean_Chart <- aggregate(cbind(Mental, Physical, Temporal, Performance, Effort, Frustration, Mean)~Condition , data=data.subjective , mean)
rownames(mean_Chart) <- mean_Chart[,1]
mean_Chart <- as.matrix(mean_Chart[,-1])

lim <- 1.2*max(mean_Chart)

error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

stdev_Chart <- aggregate(cbind(Mental, Physical, Temporal, Performance, Effort, Frustration, Mean)~Condition , data=data.subjective , sd)
rownames(stdev_Chart) <- stdev_Chart[,1]
se_Baseline <- as.matrix(stdev_Chart[,-1]) / sqrt(12)

ze_barplot <- barplot(mean_Chart , beside=T , legend.text=T,col=c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00") , ylim=c(0,lim) , ylab="NASA-TLX Score", args.legend = list(x = "topright", inset = c(0, -0.15)), cex.names=0.8)
error.bar(ze_barplot, mean_Chart, se_Baseline)

######################################################
d <- Rmisc::summarySE(data.rating, measurevar = "Rating", groupvars = c("Furniture"))
ggplot(d, aes(x = Furniture, y = Rating, fill = Furniture)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se), width = 0.4, alpha=0.9) + 
  scale_fill_manual(values=c("#66c2a5", "#fc8d62"))+ 
  labs(y = "User Rating", x = "") + 
  theme_minimal()


######################################################
d <- Rmisc::summarySE(data.rating, measurevar = "Rating", groupvars = c("Condition"))
ggplot(d, aes(x = Condition, y = Rating, fill = Condition)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_errorbar(aes(ymin=Rating-se, ymax=Rating+se), width = 0.4, alpha=0.9) + 
  scale_fill_manual(values=c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+ 
  labs(y = "User Rating", x = "") + 
  theme_minimal()

######################################################
d <- Rmisc::summarySE(data.locomotion.condition.eliminated, measurevar = "Rotation", groupvars = c("Condition"))
ggplot(d, aes(x = Condition, y = Rotation, fill = Condition)) + 
  geom_bar(stat = "identity", show.legend = FALSE) + 
  geom_errorbar(aes(ymin=Rotation-se, ymax=Rotation+se), width = 0.4, alpha=0.9) + 
  scale_fill_manual(values=c("#e41a1c", "#377eb8", "#984ea3", "#ff7f00"))+ 
  labs(y = "Head Rotation", x = "") + 
  theme_minimal()

######################################################
library(ggplot2)
d <- data.frame (
  Name = c("A", "B", "C"),
  Value = c(10, 3, 3)
)
ggplot(d, aes(fill=Name, y=Value)) + 
  geom_bar(position="stack", stat="identity")

library(tidyverse)

Ancestry <- data.frame(Race = c("A", "B", "C"), 
                       Proportion = c(10, 3, 3))

Ancestry <- Ancestry %>% 
  mutate(Year = "2006")

ggplot(Ancestry, aes(x = Year, y = Proportion, fill = Race)) +
  geom_col() +
  scale_fill_manual(values=c("#8da0cb", "#e78ac3", "#ffff99"))+ 
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL)


library(dplyr)
data.subjective %>% group_by(PID) %>% summarise(mean_mental = mean(Mental))
