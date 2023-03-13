#------------Cleaning Time of Year Data------------#

####Packages####
library(tidyverse)
library(suncalc)
library(elevatr)
library(lubridate)

####Reading WT data####
TagData<-read.csv("data/SpeciesRawDownload/ABMI_Zeller-Woodpecker_Visual_Scanning_Project-ABMI_2021_tag_details_report.csv",fileEncoding="UTF-8-BOM")
TaskData<-read.csv("data/SpeciesRawDownload/ABMI_Zeller-Woodpecker_Visual_Scanning_Project-ABMI_2021_recording_task_report.csv",fileEncoding="UTF-8-BOM")
SnowcoverData<-read.csv("data/Snowcover.csv")

TaskData<-TaskData%>%filter(status%in%c("Transcribed","Bad Weather"))
TaskData$ID<-paste(TaskData$location, TaskData$recording_date)

TagData<-TagData%>%filter(species_code=="PIWO")
TagData<-TagData%>%filter(tag_start_s<=60) #makes it so we are only looking at 1 min recordings
TagData$ID<-paste(TagData$location,TagData$recording_date)
countData<-TagData%>%filter(vocalization=="Non-vocal")%>%count(ID)
TagData<-TagData%>%mutate(vocalization_rate=countData$n[match(TagData$ID,countData$ID)])
TagData$vocalization_rate<-replace_na(TagData$vocalization_rate,0)
####Combine Tag and Task Data####
Data<-TaskData%>%select(organization:latitude,daily_min_temp:daily_mean_temp,url,
                        daily_precipitation_mm,hourly_wind_speed,daily_snow_on_ground_cm)%>%
  mutate(species_code=TagData$species_code[match(TaskData$ID,TagData$ID)],
         vocalization=TagData$vocalization[match(TaskData$ID,TagData$ID)],
         drum_rate=TagData$vocalization_rate[match(TaskData$ID,TagData$ID)],
         snowcover=SnowcoverData$snc[match(TaskData$latitude, SnowcoverData$latitude)])
Data$drum_rate<-replace_na(Data$drum_rate,0)
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
PAData$week<-as.integer(week(PAData$recording_date))
PAData$week_2<-PAData$week^2
PAData$recording_date<-as.POSIXct(PAData$recording_date)

PAData$date<-as.Date(PAData$recording_date)
PAData$lat<-PAData$latitude
PAData$lon<-PAData$longitude


####Monthly average snow cover####
PAData$month<-month(PAData$recording_date)
PAData$SCID<-paste(PAData$month,PAData$latitude,PAData$longitude)
SnowcoverData$SCID<-paste(SnowcoverData$month,SnowcoverData$latitude,SnowcoverData$longitude)
PAData<-PAData%>%mutate(snowcover=SnowcoverData$snc[match(PAData$SCID,SnowcoverData$SCID)])
####Sunrise####
sunrisetimes<-unique(getSunlightTimes(data=PAData,tz="MST"))
sunrisetimes$ID<-paste(sunrisetimes$date,sunrisetimes$lat,sunrisetimes$lon)

azimuth<-unique(getSunlightPosition(data=PAData,keep="azimuth"))
azimuth$ID<-paste(azimuth$date,azimuth$lat,azimuth$lon)
PAData$ID<-paste(PAData$date,PAData$lat,PAData$lon)

PAData<-PAData%>%select(organization:month)%>%mutate(azimuth=azimuth$azimuth[match(PAData$ID,azimuth$ID)],
                                                     sunrise=sunrisetimes$sunrise[match(PAData$ID,sunrisetimes$ID)])

PAData$hour<-hour(PAData$recording_date)
#use abs() function in for absolute value this would be time FROM sunrise rather than time SINCE sunrise
PAData$TSSR<-TaskData$TSSR<-as.numeric(difftime(PAData$recording_date,PAData$sunrise,units="hours",tz="MST"))

####Elevation assign####
#Rearrange table for elevatr package#
col_order<- c("longitude","latitude","location")
elevation_locations<-PAData[col_order]
elevation_locations<-elevation_locations%>%filter(!is.na(latitude))
#Extract elevations using elevatr#
projection<-'EPSG:4326'
spatial_data<-get_elev_point(locations=elevation_locations,prj=projection,src="aws",overwrite = T)
write.csv(spatial_data,"data/TOYelevations.csv")
elevations<-read.csv("data/TOYelevations.csv")
PAData$elevation<-elevations$elevation[match(PAData$location,elevations$location)]
####Normal Weather####
TempData<-read.csv("data/combinedweatherstationdata.csv")
m2<-read.csv("data/weatherstationsandlocations.csv")
TempData$date <- as.POSIXct(TempData$Date..Local.Standard.Time., format = "%Y-%m-%d")
TempData$julian<- yday(TempData$date)
TempData$AvgTemp <- paste(TempData[,4])
PAData$stationID <-m2$ARD_Name[match(PAData$location,m2$location)]

PAData$ID<-paste(PAData$stationID,PAData$julian)
TempData$ID<-paste(TempData$Station.Name,TempData$julian)
PAData$NormalTemp <-TempData$Air.Temp..Avg..Long.Term..C.[match(PAData$ID,TempData$ID)]
PAData$NormDev
####Green up Dates####
#Data from https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MCD12Q2
Greenup_dates<-read.csv("data/MasterGreenup1.csv")
PAData$Greenup<-Greenup_dates$greenup[match(PAData$location,Greenup_dates$location)]
PAData$Greenup<-as.Date(PAData$Greenup,origin=as.Date("1970-01-01"))
PAData$Greenup<-yday(PAData$Greenup)
PAData$DaysSinceGUP<-PAData$julian - PAData$Greenup

####Day length####
sunrisetimes<-unique(getSunlightTimes(data=PAData,tz="MST"))
sunrisetimes$ID<-paste(sunrisetimes$date,sunrisetimes$lat,sunrisetimes$lon)

PAData$ID<-paste(PAData$date,PAData$lat,PAData$lon)

PAData<-PAData%>%select(organization:DaysSinceGUP)%>%mutate(sunset=sunrisetimes$sunset[match(PAData$ID,sunrisetimes$ID)],
                                                            sunrise=sunrisetimes$sunrise[match(PAData$ID,sunrisetimes$ID)])
PAData$daylength<-difftime(PAData$sunset,PAData$sunrise,units="hours")


####Only locations with PIWO detected####
a<-PAData%>%group_by(location)%>%summarise(PIWO=sum(PIWO))
b<-a%>%filter(PIWO>=1)
PAData<-PAData%>%filter(location%in%b$location)
####Tidy up data frame####
TidyData<-PAData%>%select(location,latitude,longitude,recording_date,PIWO,PIWOdrum,PIWOcall,drum_rate,julian,week,
                          elevation,snowcover,daily_min_temp,daily_max_temp,daily_mean_temp,NormalTemp, TSSR, azimuth,
                          daily_precipitation_mm, DaysSinceGUP, daylength)
#TidyData<-na.omit(TidyData)
write.csv(TidyData,"data/TidyTOYdata.csv")


####Explore data####
nPIWOdrum<-sum(TidyData$PIWOdrum)
nPIWOdrum
nPIWO<-sum(TidyData$PIWO)
nPIWO
nlocations<-length(unique(TidyData$location))
nlocations
