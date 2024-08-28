#Final Temporal Variation Objective 2 Analysis#
library(tidyverse)
library(mgcv)
library(lme4)
library(glmnet)
#Read the data####
all.data<-read.csv("data/newTOYdata.csv")
all.data$location<-as.factor(all.data$location)
#Determine the environmental factors that drive variation in Pileated Woodpecker drumming behaviour on a seasonal basis
#Days from GUP and mean temp does has some NA values so we filter some data out for ease of comparison
data<-all.data%>%filter(!is.na(DaysSinceGUP))%>%filter(!is.na(daily_mean_temp))
#write.csv(data,"data2.csv")
#Create Models####
#NULL model
nullm<-gam(PIWOdrum~s(location,bs="re"),data,family='binomial')
summary(nullm)

#julian
julian<-gam(PIWOdrum~s(julian)+s(location,bs="re"), data,family="binomial")
summary(julian)

#day length
daylength<-gam(PIWOdrum~s(daylength)+s(location,bs="re"), data,family="binomial")
summary(daylength)

#mean temp
meantemp<-gam(PIWOdrum~s(daily_mean_temp)+s(location,bs="re"),data,family="binomial")
summary(meantemp)

#green up
sinceGUP<-gam(PIWOdrum~s(DaysSinceGUP)+s(location,bs="re"), data,family="binomial")
summary(sinceGUP)

#normal expected temp
normtemp<-gam(PIWOdrum~s(NormalTemp)+s(location,bs="re"), data,family="binomial")
summary(normtemp)

#snowcover
snowcover<-gam(PIWOdrum~s(snowcover)+s(location,bs="re"),data,family="binomial")
summary(snowcover)

#Compare Models####
#AIC
AIC(nullm,julian,daylength,meantemp,normtemp,sinceGUP,snowcover)

#Multivariate static and flexible####
staticandflex<-gam(PIWOdrum~s(daily_mean_temp)+s(daylength)+s(location,bs='re'),data, family='binomial')
summary(staticandflex)
AIC(staticandflex)
#Ridge Regression####
set.seed(1)
y<-data$PIWOdrum
data<-data%>%mutate(julian_2=(julian^2),daily_mean_temp_2=(daily_mean_temp^2),daylength_2=(daylength^2))
x<-data.matrix(data[,c('daylength','daylength_2','daily_mean_temp','daily_mean_temp_2')])
cv_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model$lambda.min
best_lambda
best_model<-glmnet(x,y,alpha=0,lambda=best_lambda)
coef(best_model)
summary(best_model)


#Ridge regression Rsquared####
#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)
#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

ridge_se <- function(xs,y,yhat,my_mod){
  # Note, you can't estimate an intercept here
  n <- dim(xs)[1]
  k <- dim(xs)[2]
  sigma_sq <- sum((y-yhat)^2)/ (n-k)
  lam <- my_mod$lambda.min
  if(is.null(my_mod$lambda.min)==TRUE){lam <- 0}
  i_lams <- Matrix(diag(x=1,nrow=k,ncol=k),sparse=TRUE)
  xpx <- t(xs)%*%xs
  xpxinvplam <- solve(xpx+lam*i_lams)
  var_cov <- sigma_sq * (xpxinvplam %*% xpx %*% xpxinvplam)
  se_bs <- sqrt(diag(var_cov))
  print('NOTE: These standard errors are very biased.')
  return(se_bs)
}



#Daily variation####
obj3data<-all.data%>%filter(!is.na(daily_precipitation_mm))%>%filter(!is.na(NormDev))


precip<-glmer(PIWOdrum~daily_precipitation_mm+(1|location),obj3data,family='binomial')
summary(precip)

normdev<-glmer(PIWOdrum~NormDev+(1|location),obj3data,family='binomial')
summary(normdev)

NULLthree<-glmer(PIWOdrum~(1|location),obj3data,family='binomial')
summary(NULLthree)


combo<-glmer(PIWOdrum~NormDev+daily_precipitation_mm+(1|location),obj3data,family='binomial')
summary(combo)

interact<-glmer(PIWOdrum~NormDev+daily_precipitation_mm+(1|location)+I(NormDev*daily_precipitation_mm),obj3data,family='binomial')
summary(interact)
AIC(NULLthree,precip,normdev,combo,interact)


meantemp_plot<-ggplot(data)+geom_smooth(aes(x=daily_mean_temp,y=PIWOdrum))+xlab("Mean Temperature (C)")+ylab("Detection Probability")+theme_classic()
plot(meantemp_plot)
ggsave("figure5.png",meantemp_plot)
