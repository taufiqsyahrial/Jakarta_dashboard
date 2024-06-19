library(dplyr)
library(data.table)
library(caret)
library(xlsx)
library(ranger)
library(MLmetrics)



accidentData <- readRDS("accidentData.rds")%>%
  select(Date, Local_Authority_.District., Day_of_Week,
         TimeSegment, Month)%>%
  filter(!is.na(TimeSegment))%>%
  mutate(Area=as.character(Local_Authority_.District.),
         Month=as.character(Month),
         Day_of_Week=as.character(Day_of_Week),
         Date=as.Date(Date),
         TimeSegment=as.numeric(gsub(":.*", "", TimeSegment)),
         TimeSegment=TimeSegment%/%6+1)%>%
  group_by(Date, TimeSegment, Area)%>%
  summarise(Day_of_Week=names(which.max(table(Day_of_Week))),
            Month=names(which.max(table(Month))),
            AccidentCount=n())

allPermutations<-expand.grid(Date=unique(accidentData$Date), TimeSegment=unique(accidentData$TimeSegment),
                             Area=unique(accidentData$Area), Day_of_Week= unique(accidentData$Day_of_Week))


finalDataset<-allPermutations%>%left_join(accidentData, by=c("Date", "TimeSegment", "Area", "Day_of_Week"))%>%
  mutate(Area=as.factor(Area),
         Day_of_Week=as.factor(weekdays(Date)),
         Month=as.factor(month.abb[as.numeric(format(as.Date(Date), "%m"))]),
         AccidentCount=ifelse(is.na(AccidentCount), 0, AccidentCount))%>%
  select( -Date)%>%
  mutate(AccidentCountNumeric=ifelse(AccidentCount>0, 1, 0),
         AccidentCount=ifelse(AccidentCount>0, "Yes", "No"))

#build the model

set.seed(123)

trainIndex<-createDataPartition(finalDataset$AccidentCount, p=0.8, list=FALSE)

train<-finalDataset[trainIndex, ]
test<-finalDataset[-trainIndex, ]

train_control <- trainControl(method="none", sampling = "down", classProbs = TRUE)

grid <- expand.grid(mtry=2, splitrule=c("gini"), min.node.size=c(3))

model <- caret::train(x=train[,1:4], y=train[,5],
                      trControl=train_control, method="ranger",
                      tuneGrid=grid)

preds <- predict(model, test, type = "prob")


LogLoss(preds[,2], test$AccidentCountNumeric)

test$preds<-preds[,2]

length(which(test$preds>=0.5 & test$AccidentCountNumeric==1))/length(which(test$AccidentCountNumeric==1))
length(which(test$preds<0.5 & test$AccidentCountNumeric==0))/length(which(test$AccidentCountNumeric==0))

saveRDS(model, file = "model.rds")


