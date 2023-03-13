#--------Time of day generalized additive models-------#
library(tidyverse)
library(mgcv)
library(tidymv)

#1. Wrangling----
TODdata<-read.csv("data/TidyTODdata.csv")
TODdata$location<-as.factor(TODdata$location)
a<-TODdata%>%group_by(location)%>%summarise(PIWO=sum(PIWO))
b<-a%>%filter(PIWO>=1)
TODdata<-TODdata%>%filter(location%in%b$location)
TODdata$NormDev<-TODdata$daily_mean_temp - TODdata$NormalTemp
#2. Single Variable GAMs----
hourGAM<-gam(PIWOdrum~s(hour,bs="cc")+s(location,bs="re"),data=TODdata,family="binomial", method="REML")
summary(hourGAM)
gam.check(hourGAM)

TSSRGAM<-gam(PIWOdrum~s(TSSR,bs="cc")+s(location,bs="re"),data=TODdata,family="binomial",method="REML")
summary(TSSRGAM)
gam.check(TSSRGAM)

#3. AIC Single Variables----
AIC(hourGAM,TSSRGAM)

#4. GAM Interaction with Latitude----
HourxLatGAM<-gam(PIWOcall~s(hour,bs='cc')+s(location,bs="re")+s(latitude)+ti(hour,latitude,bs=c('cc','tp')),data=TODdata,family="binomial",method="REML")
summary(HourxLatGAM)

TSSRxLatGAM<-gam(PIWOcall~s(TSSR,bs='cc')+s(location,bs="re")+s(latitude)+ti(TSSR,latitude,bs=c('cc','tp')),data=TODdata,family="binomial",method="REML")
summary(TSSRxLatGAM)

#5. AIC Latitude Interaction----
AIC(HourxLatGAM,TSSRxLatGAM)

#6. Get Optimum----
tssr_seq <- seq(min(TODdata$TSSR), max(TODdata$TSSR),length.out=length(TODdata$TSSR))
newdata <- expand.grid(TSSR = tssr_seq, location = unique(TODdata$location))
newdata <- newdata[rep(seq_len(nrow(newdata)), each = length(tssr_seq)), ]
predicted_values <- predict(TSSRGAM, newdata = newdata, type = "response")
max_index <- which.max(predicted_values)
peak_tssr <- newdata$TSSR[max_index]
print(peak_tssr)