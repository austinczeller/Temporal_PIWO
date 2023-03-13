####INFO####
#This script load data for PIWO detections from Wildtrax
#We will also insert valuable information into the dataframe such as sunrise, elevation, and habitat
####Packages####
library(tidyverse)
library(suncalc)
library(lubridate)
library(circular)
library(fuzzyjoin)
library(elevatr)
library(wildRtrax)
####Load Data####
TagData<-rbind(read.csv("data/SpeciesRawDownload/ABMI_Zeller-Woodpecker_Visual_Scanning_Project-ABMI_2021_tag_details_report.csv",fileEncoding="UTF-8-BOM"),
               read.csv("data/SpeciesRawDownload/BU_Zeller-_Woodpeckers-_BU_2021_tag_details_report.csv",fileEncoding="UTF-8-BOM"),
               read.csv("data/SpeciesRawDownload/BU_AB_Woodpeckers-_Time_of_Day_tag_details_report.csv",fileEncoding = "UTF-8-BOM"))#this line is the time of day woodpecker project

TaskData<-rbind(read.csv("data/SpeciesRawDownload/ABMI_Zeller-Woodpecker_Visual_Scanning_Project-ABMI_2021_recording_task_report.csv",fileEncoding="UTF-8-BOM"),
                read.csv("data/SpeciesRawDownload/BU_Zeller-_Woodpeckers-_BU_2021_recording_task_report.csv",fileEncoding="UTF-8-BOM"),
                read.csv("data/SpeciesRawDownload/BU_AB_Woodpeckers-_Time_of_Day_recording_task_report.csv",fileEncoding = "UTF-8-BOM"))#this line is the time of day woodpecker project

TaskData<-TaskData%>%filter(status=="Transcribed")
TaskData$ID<-paste(TaskData$location, TaskData$recording_date)

TagData<-TagData%>%filter(species_code=="PIWO")
TagData$ID<-paste(TagData$location,TagData$recording_date)
#FILTERING OUT ANY DETECTIONS OVER 60secs
#TagData<-TagData%>%filter(!tag_start_s>60)

countData<-TagData%>%count(ID)
TagData<-TagData%>%mutate(vocalization_rate=countData$n[match(TagData$ID,countData$ID)])
TagData$vocalization_rate<-replace_na(TagData$vocalization_rate,0)

TagData<-TagData%>%group_by(ID)%>%mutate(abundance=max(individual_appearance_order))


#This is  Presence Absence Data
TaskData2<-TaskData%>%select(organization:latitude,daily_min_temp:daily_mean_temp,url)%>%
  mutate(species_code=TagData$species_code[match(TaskData$ID,TagData$ID)],
         vocalization=TagData$vocalization[match(TaskData$ID,TagData$ID)],
         vocalization_rate=TagData$vocalization_rate[match(TaskData$ID,TagData$ID)],
         abundance=TagData$abundance[match(TaskData$ID,TagData$ID)])
TaskData2$vocalization_rate<-replace_na(TaskData2$vocalization_rate,0)
TaskData2$abundance<-replace_na(TaskData2$abundance,0)
TaskData2$PIWO<-0
TaskData2$PIWO[grepl("PIWO",TaskData2$species_code)]=1


#Drumming Data
TaskData2$PIWOdrum<-0
TaskData2$PIWOdrum[grepl("Non-vocal",TaskData2$vocalization)]=1

PAData<-TaskData2%>%select(!species_code) #TIDY'D Pres/Absence PIWO Data

####Day of year####
PAData$julian<-yday(PAData$recording_date)
PAData$recording_date<-as.POSIXct(PAData$recording_date)
PAData$date<-as.Date(PAData$recording_date)

####Time Since Sunrise####
PAData$date<-as.Date(PAData$recording_date)
PAData$lat<-PAData$latitude
PAData$lon<-PAData$longitude

sunrisetimes<-unique(getSunlightTimes(data=PAData,keep="sunrise",tz="MST"))
sunrisetimes$ID<-paste(sunrisetimes$date,sunrisetimes$lat,sunrisetimes$lon)
PAData$ID<-paste(PAData$date,PAData$lat,PAData$lon)
PAData<-PAData%>%select(organization:julian)%>%mutate(
  sunrise=sunrisetimes$sunrise[match(PAData$ID,sunrisetimes$ID)])
PAData$TSSR<-TaskData$TSSR<-as.numeric(difftime(PAData$recording_date,PAData$sunrise,units="hours",tz="MST"))
PAData$roundTSSR<-round(PAData$TSSR)
PAData$TSSR<-circular(PAData$TSSR, units="hours")


####Writing a data csv####

write.csv(PAData,file="data/PAData.csv")

