#------------Cleaning Time of Day Data------------#

#####Packages####
library(tidyverse)
library(suncalc)
library(elevatr)
library(lubridate)
library(circular)
library(fuzzyjoin)
library(rnoaa)
library(scales)
library(mgcv)
library(tidymv)
library(rgdal)
library(sp)
library(gratia)
####Reading WT data####
TagData<-read.csv("data/SpeciesRawDownload/BU_AB_Woodpeckers-_Time_of_Day_tag_details_report.csv",fileEncoding="UTF-8-BOM")
TaskData<-read.csv("data/SpeciesRawDownload/BU_AB_Woodpeckers-_Time_of_Day_recording_task_report.csv",fileEncoding="UTF-8-BOM")
SnowcoverData<-read.csv("data/Snowcover_all_locations_ABwoodpeckers.csv")

TaskData<-TaskData%>%filter(status=="Transcribed")
TaskData$ID<-paste(TaskData$location, TaskData$recording_date)

TagData<-TagData%>%filter(species_code=="PIWO")
TagData$ID<-paste(TagData$location,TagData$recording_date)
countData<-TagData%>%filter(vocalization=="Non-vocal")%>%count(ID)
TagData<-TagData%>%mutate(vocalization_rate=countData$n[match(TagData$ID,countData$ID)])
TagData$vocalization_rate<-replace_na(TagData$vocalization_rate,0)
####Combine Tag and Task Data####
Data<-TaskData%>%select(organization:latitude,daily_min_temp:hourly_weather_attributes,url)%>%
  mutate(species_code=TagData$species_code[match(TaskData$ID,TagData$ID)],
         vocalization=TagData$vocalization[match(TaskData$ID,TagData$ID)],
         vocalization_rate=TagData$vocalization_rate[match(TaskData$ID,TagData$ID)])
Data$vocalization_rate<-replace_na(Data$vocalization_rate,0)
####Extract PIWO Detection####
Data$PIWO<-0
Data$PIWO[grepl("PIWO",Data$species_code)]=1
#Drumming Data
Data$PIWOdrum<-0
Data$PIWOdrum[grepl("Non-vocal",Data$vocalization)]=1
Data$PIWOcall<-0
Data$PIWOcall[grepl("Call",Data$vocalization)]=1
PAData<-Data%>%select(!species_code) #TIDY'D Pres/Absence PIWO Data
####Date Data Cleanup####
PAData$julian<-yday(PAData$recording_date)
PAData$recording_date<-as.POSIXct(PAData$recording_date)

PAData$date<-as.Date(PAData$recording_date)
PAData$lat<-PAData$latitude
PAData$lon<-PAData$longitude
####Sunrise times and sun position####

sunrisetimes<-unique(getSunlightTimes(data=PAData,tz="MST"))
sunrisetimes$ID<-paste(sunrisetimes$date,sunrisetimes$lat,sunrisetimes$lon)

azimuth<-unique(getSunlightPosition(data=PAData,keep="azimuth"))
azimuth$ID<-paste(azimuth$date,azimuth$lat,azimuth$lon)
PAData$ID<-paste(PAData$date,PAData$lat,PAData$lon)

PAData<-PAData%>%select(organization:julian)%>%mutate(azimuth=azimuth$azimuth[match(PAData$ID,azimuth$ID)],
                                                      sunrise=sunrisetimes$sunrise[match(PAData$ID,sunrisetimes$ID)])

PAData$hour<-hour(PAData$recording_date)
#use abs() function in for absolute value this would be time FROM sunrise rather than time SINCE sunrise
PAData$TSSR<-TaskData$TSSR<-as.numeric(difftime(PAData$recording_date,PAData$sunrise,units="hours",tz="MST"))

####Monthly average snow cover####
PAData$month<-month(PAData$recording_date)
PAData$SCID<-paste(PAData$month,PAData$latitude,PAData$longitude)
SnowcoverData$SCID<-paste(SnowcoverData$month,SnowcoverData$latitude,SnowcoverData$longitude)
PAData<-PAData%>%mutate(snowcover=SnowcoverData$snc[match(PAData$SCID,SnowcoverData$SCID)])
####Elevation assign####
#Rearrange table for elevatr package#
col_order<- c("longitude","latitude","location")
elevation_locations<-PAData[col_order]
#Extract elevations using elevatr#
projection<-'EPSG:4326'
spatial_data<-get_elev_point(locations=elevation_locations,prj=projection,src="aws",overwrite = T)
write.csv(spatial_data,"data/TODelevations.csv")
elevations<-read.csv("data/TODelevations.csv")
PAData$elevation<-elevations$elevation[match(PAData$location,elevations$location)]
####Normal Weather Data####
TempData<-read.csv("data/combinedweatherstationdata.csv")
m2<-read.csv("data/weatherstationsandlocations.csv")
TempData$date <- as.POSIXct(TempData$Date..Local.Standard.Time., format = "%Y-%m-%d")
TempData$julian<- yday(TempData$date)
TempData$AvgTemp <- paste(TempData[,4])
PAData$stationID <-m2$ARD_Name[match(PAData$location,m2$location)]

PAData$ID<-paste(PAData$stationID,PAData$julian)
TempData$ID<-paste(TempData$Station.Name,TempData$julian)
PAData$NormalTemp <-TempData$Air.Temp..Avg..Long.Term..C.[match(PAData$ID,TempData$ID)]
PAData$NormDev<-PAData$daily_mean_temp - PAData$NormalTemp
####Green up Dates#### 
#Data from https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MCD12Q2
Greenup_dates<-rbind(read.csv("data/Greenups/2015GUP.csv"),
                     read.csv("data/Greenups/2016GUP.csv"),
                     read.csv("data/Greenups/2017GUP.csv"),
                     read.csv("data/Greenups/GUP2019plus.csv"))
PAData$Greenup<-Greenup_dates$Greenup_1[match(PAData$location,Greenup_dates$location)]
PAData$Greenup<-as.Date(PAData$Greenup,origin=as.Date("1970-01-01"))
PAData$Greenup<-yday(PAData$Greenup)
PAData$MidGUP<-Greenup_dates$MidGreenup_1[match(PAData$location,Greenup_dates$location)]
PAData$MidGUP<-as.Date(PAData$MidGUP,origin=as.Date("1970-01-01"))
PAData$MidGUP<-yday(PAData$MidGUP)
PAData$GUPpeak<-Greenup_dates$Peak_1[match(PAData$location,Greenup_dates$location)]
PAData$GUPpeak<-as.Date(PAData$GUPpeak,origin=as.Date("1970-01-01"))
PAData$GUPpeak<-yday(PAData$GUPpeak)


####Assign high low lat####


####Only locations with PIWO detected####
a<-PAData%>%group_by(location)%>%summarise(PIWO=sum(PIWO))
b<-a%>%filter(PIWO>=1)
PAData<-PAData%>%filter(location%in%b$location)
####Tidy up data frame####
TidyData<-PAData%>%select(location,latitude,longitude,recording_date,PIWO,PIWOdrum,PIWOcall,drum_rate=vocalization_rate,hour,TSSR,julian,
                          elevation,snowcover,daily_min_temp,daily_max_temp,daily_mean_temp,daily_precipitation_mm,azimuth,
                          NormalTemp,NormDev,Greenup,MidGUP,GUPpeak)
write.csv(TidyData,"data/TidyTODdata.csv")

####Explore data####
nPIWOdrum<-sum(TidyData$PIWOdrum)
nPIWOdrum
nPIWO<-sum(TidyData$PIWO)
nPIWO
nlocations<-length(unique(TidyData$location))
nlocations

