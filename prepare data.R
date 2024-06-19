library(dplyr)
library(lubridate)
library(data.table)
library(tidyverse)
library(hms)

accidentData <- read_csv("2015_2020June.csv")


accidentData<-accidentData%>%
  select(District, Severity, Date, Longitude, Latitude,CrashType, RoadClass, RoadType,
         LightCondition, Weather, RoadGeometry, RoadSurface, 
         RoadElevation, District, SpeedLimit) %>%
  mutate(Accident_Severity = as.factor (Severity)) %>%
  mutate(RoadType = as.factor (RoadType)) %>%
  mutate(Local_Authority_.District. = as.factor (District)) %>%
  mutate(Vehicle_Manoeuvre = as.factor (CrashType)) %>%
  mutate(Light_Conditions = as.factor (LightCondition)) %>%
  mutate(Road_Surface_Conditions = as.factor (RoadSurface)) %>%
  mutate(Weather_Conditions = as.factor (Weather)) %>%
  mutate(Speed_limit = as.factor (SpeedLimit)) %>%
  mutate(Month=factor(month.abb[as.numeric(format(as.Date(Date), "%m"))], levels=month.abb)) %>%
  mutate_at(vars(Date), dmy_hm) %>% 
  mutate_at(vars(Date), funs("date" = date(.), "time" = as.hms (.))) %>%
  mutate(Year = lubridate::year(Date), 
         Month = lubridate::month(Date, abbr= TRUE), 
         Day_of_Week = lubridate::wday(Date, label= TRUE, abbr= FALSE)) 
  

accidentData$Weather_Conditions <- fct_collapse(accidentData$Weather_Conditions,  Fine = c("Cerah"), Raining = c("Hujan/ Grimis", "Hujan disertai Angin Kencang"), 
             Fog_Cloudy = c("Berawan/ Mendung", "Berkabut/ Berasap"), Other = c("Tidak Diketahui"))

accidentData <- accidentData %>%
  mutate(TimeSegment=as.POSIXct(as.character(time), format = "%H:%M"))%>%
  mutate(TimeSegment=as.POSIXct(round(as.double(TimeSegment)/(60*60))*(60*60)+3600,origin=(as.POSIXct('1970-01-01'))))%>%
  mutate(TimeSegment=strftime(TimeSegment, format = "%H:%M"),
         Month=factor(month.abb[as.numeric(format(as.Date(Date), "%m"))], levels=month.abb))


library(forcats)
accidentData$Accident_Severity <-  fct_explicit_na(accidentData$Accident_Severity, "Ringan")
accidentData <- tibble::rowid_to_column(accidentData, "ID")

accidentData <- accidentData %>%
  mutate(Accident_Index= ID)

outliers <- boxplot(accidentData$Longitude, plot = FALSE)$out

accidentData[accidentData$Longitude %in% outliers, "Longitude"] = NA

outliers_lat <- boxplot(accidentData$Latitude, plot = FALSE)$out
accidentData[accidentData$Latitude %in% outliers_lat, "Latitude"] = NA

accidentData <- accidentData[!is.na(accidentData$Year),]

saveRDS(accidentData, file = "accidentData.rds")


