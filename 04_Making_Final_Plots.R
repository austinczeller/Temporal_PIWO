#--------Making Final Plots--------#
library(tidyverse)
library(scales)
TOYData<-read.csv("data/TidyTOYdata.csv")
TODdata<-read.csv("data/TidyTODdata.csv")

fig3<-ggplot(TOYdata)+
  geom_smooth(aes(x=julian,y=PIWOdrum),method = "gam",se=T,formula = y ~ s(x, bs = "tp"))+
  ylab("Detection probabilty")+
  coord_cartesian(ylim=c(0,.10))+
  scale_x_continuous("Date", breaks=c(46,74,105,135,166,196),
                     labels=c("15 Feb","15 Mar","15 Apr","15 May", "15 Jun", "15 Jul"))+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
fig3




TSSRplot<-ggplot(TODdata)+geom_smooth(aes(x=TSSR,y=PIWOdrum),method = "gam",formula=y~s(x,bs="cp"),se=T)+
  ylab("Detection probabilty")+
  xlab("Hours from sunrise")+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #coord_cartesian(ylim=c(0,.075))+
  scale_x_continuous(breaks=breaks_width(2))
TSSRplot
Hourplot<-ggplot(TODdata)+geom_smooth(aes(x=hour,y=PIWOdrum),formula=y~s(x,bs="cp"),method = "gam",se=T)+
  ylab("Detection probabilty")+
  xlab("Hour")+
  theme_bw()+theme(legend.title = element_blank())+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  #coord_cartesian(ylim=c(0,.075))+
  scale_x_continuous(breaks=breaks_width(2))
Hourplot
TSSRplot

par(mfrow=c(Hourplot, TSSRplot))
Hourplot

TSSRplot
?par
