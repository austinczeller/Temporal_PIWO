#Visits until detection
library(dplyr)
library(SciViews)
TODdata<-read.csv("data/TidyTODdata.csv")
TOYdata<-read.csv("data/TidyTOYdata.csv")
TOYdata$location<-as.character(TOYdata$location)
CombinedData<-bind_rows(TODdata,TOYdata)
#Filter data----
data<-CombinedData%>%filter(between(julian,95,130))%>%filter(between(hour,5,7))
data<-CombinedData
####Variables####
d<-0.05 #detection
obs<-count(data)
obs<-obs$n
PIWOdect<-sum(data$PIWOdrum)
P<-.09#dectection probabilty

####Equation####
VisitsRequired<-function(d,P){ln(d)*ln(1-P)^-1}

print(VisitsRequired)
VisitsRequired(d,.09)#95% for peak DOY
VisitsRequired(d,.0)
