mean.furniture.accuracy <- data.accuracy %>%
  group_by(Furniture) %>%
  summarise(avg= mean(Accuracy))

mean.layout.accuracy <- data.accuracy %>%
  group_by(Layout) %>%
  summarise(avg= mean(Accuracy))

### gender analysis
# male
modelAccuracy_Male <- aov(Accuracy~factor(Condition)+Error(factor(Participant)), data = maleDF)
effectsize(modelAccuracy_Male)
modelEucliError_Male <- aov(EuclideanError~factor(Condition)+Error(factor(Participant)), data = maleDF)
effectsize(modelEucliError_Male)

summary(modelAccuracy_Male)
summary(modelEucliError_Male)

# female
modelAccuracy_Female <- aov(Accuracy~factor(Condition)+Error(factor(Participant)), data = femaleDF)
effectsize(modelAccuracy_Female)
modelEucliError_Female <- aov(EuclideanError~factor(Condition)+Error(factor(Participant)), data = femaleDF)
effectsize(modelEucliError_Female)

summary(modelAccuracy_Female)
summary(modelEucliError_Female)

####### plot #####
### General plot ###
dfplotAccuracy <- data_summary(finishedDF, varname="Accuracy", 
                               groupnames="Condition")
pAccuracy <- ggplot(dfplotAccuracy, aes(x=Condition, y=Accuracy, fill=Condition)) + 
  geom_bar(stat="identity", color="black", size = 1, position=position_dodge(), width=.7) +
  geom_errorbar(aes(ymin=Accuracy-se, ymax=Accuracy+se), width=.2,
                position=position_dodge(.7), size = 1) + theme_minimal()
pAccuracy + theme(legend.position='none')+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + labs(y="Accuracy")
#-------

dfplotAccuracy_Modif <- data_summary(finishedDF, varname="Accuracy_Modif", 
                                     groupnames="Condition")
pAccuracy_Modif <- ggplot(dfplotAccuracy_Modif, aes(x=Condition, y=Accuracy_Modif, fill=Condition)) + 
  geom_bar(stat="identity", color="black", size = 1, position=position_dodge(), width=.7) +
  geom_errorbar(aes(ymin=Accuracy_Modif-se, ymax=Accuracy_Modif+se), width=.2,
                position=position_dodge(.7), size = 1) + theme_minimal()
pAccuracy_Modif + theme(legend.position='none')+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + labs(y="Modified Accuracy")
#-------

dfplotEucError <- data_summary(finishedDF, varname="EuclideanError", 
                               groupnames="Condition")
pEucError <- ggplot(dfplotEucError, aes(x=Condition, y=EuclideanError, fill=Condition)) + 
  geom_bar(stat="identity", color="black", size = 1, position=position_dodge(), width=.7) +
  geom_errorbar(aes(ymin=EuclideanError-se, ymax=EuclideanError+se), width=.2,
                position=position_dodge(.7), size = 1) + theme_minimal()
pEucError + theme(legend.position='none')+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + labs(y="Euclidean Error")


### Male plot ###
dfplotAccuracy_Male <- data_summary(maleDF, varname="Accuracy", 
                                    groupnames="Condition")
pAccuracy_Male <- ggplot(dfplotAccuracy_Male, aes(x=Condition, y=Accuracy, fill=Condition)) + 
  geom_bar(stat="identity", color="black", size = 1, position=position_dodge(), width=.7) +
  geom_errorbar(aes(ymin=Accuracy-se, ymax=Accuracy+se), width=.2,
                position=position_dodge(.7), size = 1) + theme_minimal()
pAccuracy_Male + theme(legend.position='none')+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + labs(y="Male Accuracy")


dfplotEucError_Male <- data_summary(maleDF, varname="EuclideanError", 
                                    groupnames="Condition")
pEucError_Male <- ggplot(dfplotEucError_Male, aes(x=Condition, y=EuclideanError, fill=Condition)) + 
  geom_bar(stat="identity", color="black", size = 1, position=position_dodge(), width=.7) +
  geom_errorbar(aes(ymin=EuclideanError-se, ymax=EuclideanError+se), width=.2,
                position=position_dodge(.7), size = 1) + theme_minimal()
pEucError_Male + theme(legend.position='none')+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + labs(y="Male Euclidean Error")


### Female plot ###
dfplotAccuracy_Female <- data_summary(femaleDF, varname="Accuracy", 
                                      groupnames="Condition")
pAccuracy_Female <- ggplot(dfplotAccuracy_Female, aes(x=Condition, y=Accuracy, fill=Condition)) + 
  geom_bar(stat="identity", color="black", size = 1, position=position_dodge(), width=.7) +
  geom_errorbar(aes(ymin=Accuracy-se, ymax=Accuracy+se), width=.2,
                position=position_dodge(.7), size = 1) + theme_minimal()
pAccuracy_Female + theme(legend.position='none')+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + labs(y="Female Accuracy")


dfplotEucError_Female <- data_summary(femaleDF, varname="EuclideanError", 
                                      groupnames="Condition")
pEucError_Female <- ggplot(dfplotEucError_Female, aes(x=Condition, y=EuclideanError, fill=Condition)) + 
  geom_bar(stat="identity", color="black", size = 1, position=position_dodge(), width=.7) +
  geom_errorbar(aes(ymin=EuclideanError-se, ymax=EuclideanError+se), width=.2,
                position=position_dodge(.7), size = 1) + theme_minimal()
pEucError_Female + theme(legend.position='none')+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + labs(y="Female Euclidean Error")

### Furniture ###
dfplotAccuracy_Furniture <- data_summary(finishedDF, varname="Accuracy", 
                                         groupnames="Furniture")
pAccuracy_Furniture <- ggplot(dfplotAccuracy_Furniture, aes(x=Furniture, y=Accuracy, fill=Furniture)) + 
  geom_bar(stat="identity", color="black", size = 1, position=position_dodge(), width=.7) +
  geom_errorbar(aes(ymin=Accuracy-se, ymax=Accuracy+se), width=.2,
                position=position_dodge(.7), size = 1) + theme_minimal()
pAccuracy_Furniture + theme(legend.position='none')+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + labs(y="Accuracy")

dfplotEucError_Furniture <- data_summary(finishedDF, varname="EuclideanError", 
                                         groupnames="Furniture")
pEucError_Furniture <- ggplot(dfplotEucError_Furniture, aes(x=Furniture, y=EuclideanError, fill=Furniture)) + 
  geom_bar(stat="identity", color="black", size = 1, position=position_dodge(), width=.7) +
  geom_errorbar(aes(ymin=EuclideanError-se, ymax=EuclideanError+se), width=.2,
                position=position_dodge(.7), size = 1) + theme_minimal()
pEucError_Furniture + theme(legend.position='none')+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + labs(y="Euclidean Error")

### Layout ###
dfplotAccuracy_Layout <- data_summary(finishedDF, varname="Accuracy", 
                                      groupnames="Layout")
pAccuracy_Layout <- ggplot(dfplotAccuracy_Layout, aes(x=Layout, y=Accuracy, fill=Layout)) + 
  geom_bar(stat="identity", color="black", size = 1, position=position_dodge(), width=.7) +
  geom_errorbar(aes(ymin=Accuracy-se, ymax=Accuracy+se), width=.2,
                position=position_dodge(.7), size = 1) + theme_minimal()
pAccuracy_Layout + theme(legend.position='none')+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + labs(y="Accuracy")

dfplotEucError_Layout <- data_summary(finishedDF, varname="EuclideanError", 
                                      groupnames="Layout")
pEucError_Layout <- ggplot(dfplotEucError_Layout, aes(x=Layout, y=EuclideanError, fill=Layout)) + 
  geom_bar(stat="identity", color="black", size = 1, position=position_dodge(), width=.7) +
  geom_errorbar(aes(ymin=EuclideanError-se, ymax=EuclideanError+se), width=.2,
                position=position_dodge(.7), size = 1) + theme_minimal()
pEucError_Layout + theme(legend.position='none')+ theme(axis.text=element_text(size=12), axis.title=element_text(size=14)) + labs(y="Euclidean Error")