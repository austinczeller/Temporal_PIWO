#--------Time of year generalized additive models-------#
library(tidyverse)
library(mgcv)
library(tidymv)
library(lubridate)

#1. Wrangling----
TOYdata<-read.csv("data/TidyTOYdata.csv")
TOYdata$location<-as.factor(TOYdata$location)
a<-TOYdata%>%group_by(location)%>%summarise(PIWO=sum(PIWO))
b<-a%>%filter(PIWO>=1)
TOYdata<-TOYdata%>%filter(location%in%b$location)#TOYdata 2 is only sites where PIWO was detected
TOYdata$NormDev<-TOYdata$daily_mean_temp - TOYdata$NormalTemp
TOYdata$year<-year(TOYdata$recording_date)
#TOYdata<-TOYdata%>%filter(year<=2018)#filters only for years we have the GUP dates for

#2. Single Variable GAMs----
julian<-gam(PIWOdrum~s(julian)+s(location,bs="re"), data=TOYdata,family="binomial")
summary(julian)
gam.check(julian)

maxtemp<-gam(PIWOdrum~s(daily_max_temp)+s(location,bs="re"), data=TOYdata,family="binomial")
summary(maxtemp)
gam.check(maxtemp)

meantemp<-gam(PIWOdrum~s(daily_mean_temp)+s(location,bs="re"),data=TOYdata,family="binomial")
summary(meantemp)
gam.check(meantemp)

mintemp<-gam(PIWOdrum~s(daily_min_temp)+s(location,bs="re"), data=TOYdata,family="binomial")
summary(mintemp)
gam.check(mintemp)

sinceGUP<-gam(PIWOdrum~s(DaysSinceGUP)+s(location,bs="re"), data=TOYdata,family="binomial")
summary(sinceGUP)
gam.check(sinceGUP)

daylen<-gam(PIWOdrum~s(daylength)+s(location,bs="re"), data=TOYdata,family="binomial")
summary(daylen)
gam.check(daylen)

normdev<-gam(PIWOdrum~s(NormDev)+s(location,bs="re"), data=TOYdata,family="binomial")
summary(normdev)
gam.check(normdev)

elevation<-gam(PIWOdrum~s(elevation)+s(location,bs="re"),data=TOYdata,family="binomial")
summary(elevation)
gam.check(elevation)

snowcover<-gam(PIWOdrum~s(snowcover)+s(location,bs="re"),data=TOYdata,family="binomial")
summary(snowcover)
gam.check(snowcover)

TSSR<-gam(PIWOdrum~s(TSSR,bs="cc"),data=TOYdata,family='binomial')
summary(TSSR)
gam.check(TSSR)

#3. AIC Single Variables----
AIC(maxtemp,mintemp,meantemp,normdev,snowcover)
AIC(julian,daylen,sinceGUP)

#4. GAM Interaction with Latitude----
DOYxLAT<-gam(PIWOdrum~s(julian)+s(latitude)+ti(julian,latitude)+s(location,bs="re"),data=TOYdata,family="binomial")
summary(DOYxLAT)

DLxLAT<-gam(PIWOdrum~s(daylength)+s(latitude)+ti(daylength,latitude)+s(location,bs="re"),data=TOYdata,family="binomial")
summary(DLxLAT)

SNCxLAT<-gam(PIWOdrum~s(snowcover)+s(latitude)+ti(snowcover,latitude)+s(location,bs="re"),data=TOYdata,family="binomial")
summary(SNCxLAT)

MEANTEMPxLAT<-gam(PIWOdrum~s(daily_mean_temp)+s(latitude)+ti(daily_mean_temp,latitude)+s(location,bs="re"),data=TOYdata,family="binomial")
summary(MEANTEMPxLAT)

GUPxLAT<-gam(PIWOdrum~s(DaysSinceGUP)+s(latitude)+ti(DaysSinceGUP,latitude)+s(location,bs="re"),data=TOYdata,family="binomial")
summary(GUPxLAT)

NORMTEMPxLAT<-gam(PIWOdrum~s(NormalTemp)+s(latitude)+ti(NormalTemp,latitude)+s(location,bs="re"),data=TOYdata,family="binomial")
summary(NORMTEMPxLAT)

#5. AIC Latitude Interaction----
AIC(DLxLAT,SNCxLAT,MEANTEMPxLAT,GUPxLAT,NORMTEMPxLAT,DOYxLAT)
#6. Get Optimum----
#Get peak Julian Day
julian_seq <- seq(min(TOYdata$julian), max(TOYdata$julian),length.out=length(TOYdata$julian))
newdata <- expand.grid(julian = julian_seq, location = unique(TOYdata$location))
newdata <- newdata[rep(seq_len(nrow(newdata)), each = length(julian_seq)), ]
predicted_values <- predict(julian, newdata = newdata, type = "response")
max_index <- which.max(predicted_values)
peak_julian <- newdata$julian[max_index]
print(peak_julian)
#92.84

peak_julian=92.84
peakdf<-data.frame(julian=peak_julian,location = unique(TOYdata$location))
p<-predict(julian,peakdf,type="response")
mean(p)
#peak likelihood is about 9%

#Not sure we need this code, generated through Chat----
#Get the likelihood at that peak Julian day
# Compute predicted values and standard errors
pred <- predict(julian, newdata, se.fit = TRUE, type = "response")
se <- pred$se.fit

# Find the index of the peak julian date in the newdata dataframe
max_index <- which.max(pred$fit)

# Extract the predicted value and standard error at the peak julian date
peak_PIWOdrum_pred <- pred$fit[max_index]
peak_PIWOdrum_se <- se[max_index]

# Compute the confidence interval at the peak julian date
ci <- peak_PIWOdrum_pred + qt(0.975, julian$df.residual) * peak_PIWOdrum_se * c(-1, 1)

# Compute the likelihood of observing a PIWOdrum at the peak julian
peak_PIWOdrum_likelihood <- pnorm(0, peak_PIWOdrum_pred, peak_PIWOdrum_se, lower.tail = FALSE)

# Print the results
cat("Peak PIWOdrum value:", peak_PIWOdrum_pred, "\n")
cat("95% confidence interval:", ci[1], "-", ci[2], "\n")
cat("Likelihood of observing a PIWOdrum at the peak julian:", peak_PIWOdrum_likelihood, "\n")



model_p %>%
  ggplot(aes(julian, fit))

simplejulian<-gam(PIWOdrum~s(julian), data=TOYdata,family="binomial")
newd<-c()
newd<-data.frame(julian=seq(min(TOYdata$julian),max(TOYdata$julian),by=1),PIWOdrum=NA)
newd$PIWOdrum<-predict(simplejulian,newd)
ggplot(newd)+geom_line(aes(x=julian,y=PIWOdrum))
max(newd$PIWOdrum)

