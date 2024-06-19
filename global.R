library(dplyr)
library(xlsx)
# read data from local rds files

accidentData <- readRDS("accidentData.rds")
riskModel <- readRDS("model.rds")

# create lists for dropdowns
dayList<-factor(unique(accidentData$Day_of_Week), levels = c("Monday", "Tuesday", "Wednesday",
                                                             "Thursday", "Friday", "Saturday",
                                                             "Sunday"))
severityList<-unique(accidentData$Accident_Severity)
lightList<-unique(accidentData$Light_Conditions)
roadList<-unique(accidentData$Road_Surface_Conditions)
speedList<-unique(accidentData$Speed_limit)
weatherList<-unique(accidentData$Weather_Conditions)
districtList<-unique(as.character(accidentData$Local_Authority_.District.))
yearList<-unique(accidentData$Year)

hourList<-c("12am-6am", "6am-12pm", "12pm-6pm", "6pm-12am")



