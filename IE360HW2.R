library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(RColorBrewer)
library(zoo)
library(GGally)
library(xts)

#DATA PREPARATION
data<-read.csv("C:\\Users\\ilker\\Documents\\IE360_Spring22_HW2_data.csv")
str(data)
is.ts(data)
data<-data%>%dplyr::rename(UGS=Unleaded.Gasoline.Sale..UGS.,NLPG=X..LPG.Vehicles..NLPG.,PU=Price.of.Unleaded.Gasoline..PU.,
                           PG=Price.of.Diesel.Gasoline..PG., NUGV=X..Unleaded.Gasoline.Vehicles..NUGV.,NDGV=X..of.Diesel.Gasoline.Vehicles..NDGV.)%>%
  mutate(Quarter=as.Date(as.yearqtr(Quarter,format="%Y_Q%q")))
data$UGS<-as.numeric(gsub(" ", "", data$UGS))
data$NLPG<-as.numeric(gsub(" ", "", data$NLPG))
data$NUGV<-as.numeric(gsub(" ", "", data$NUGV))
data$GNP.Agriculture<-as.numeric(gsub(" ", "", data$GNP.Agriculture))
data$GNP.Commerce<-as.numeric(gsub(" ", "", data$GNP.Commerce))
data$GNP.Total<-as.numeric(gsub(" ", "", data$GNP.Total))
str(data)

#DATA VISUALIZATION
ggplot(data, aes(Quarter,UGS))+geom_line(color="#E46726")+theme_grey()+
  ggtitle("Quarterly Unleaded Gasoline Sales Between 2000/Q1 to 2006/Q4")+xlab("Years")+ylab("UGS")
#CORRELATIONS
library(ggcorrplot)
#autocorelations
acf(data$UGS,lag.max = 12,na.action = na.pass)

#BUILDING REGRESSION MODEL
#first check correlation between independent variables
corr_analysis=cor(data[c(1:28),c(2:11)])
ggcorrplot(corr_analysis,hc.order=TRUE,type="lower",lab=TRUE, colors=c("#6D9EC1","white","#E46726"),ggtheme = ggplot2::theme_gray)
#also for distributions and better pairwise observations
ggpairs(data[c(1:28),c(2:11)],ggplot2::aes(fill="#C4961A"))+scale_fill_manual(values = "#00AFBB")+ggtitle("Correlation and Distribution Relation of each Parameter")
ggplot(data,aes(data$NDGV,data$UGS))+geom_point()

#Adding trend and seasonality
library(data.table)
is.data.frame(data)
trend<-c(1:nrow((data)))
data$trend<-trend
data$Seasons<-as.factor(quarter(data$Quarter))
#setDT(data)
#first regression model with only seasonality and trend
lm1=lm(UGS~trend+Seasons,data=data)
summary(lm1)
#residual check for the first linear model
library(forecast)
checkresiduals(lm1)

#Second linear model 
#LAG1
lag1<-shift(data$UGS, n=1L, fill=NA)
data$lag1<-lag1
lmlag1=lm(UGS~trend+Seasons+lag1,data=data)
summary(lmlag1)
checkresiduals(lmlag1)
#LAG2
lag2<-shift(data$UGS, n=2L, fill=NA)
data$lag2<-lag2
lmlag2=lm(UGS~trend+Seasons+lag2,data=data)
summary(lmlag2)
checkresiduals(lmlag2)
#LAG4
lag4<-shift(data$UGS, n=4L, fill=NA)
data$lag4<-lag4
lmlag4=lm(UGS~trend+Seasons+lag4,data=data)
summary(lmlag4)
checkresiduals(lmlag4)

##Adding variables with high correlation values 

##Add NUGV
lm2=lm(UGS~trend+Seasons+NUGV,data=data)
summary(lm2)
checkresiduals(lm2)
##add NDGV
lm3=lm(UGS~trend+Seasons+NDGV+lag2+NUGV,data=data)
summary(lm3)
checkresiduals(lm3)
#add PG
lm4=lm(UGS~trend+Seasons+NDGV+PG+NUGV+lag1+lag2,data=data)
summary(lm4)
plot(lm4)
checkresiduals(lm4)
lm7=lm(UGS~trend+Seasons+NDGV+PG+NUGV+lag1,data=data)
summary(lm7)
plot(lm7)
checkresiduals(lm7)
##add GNP.Agriculture
lm5=lm(UGS~trend+Seasons+NDGV+PG+GNP.Agriculture+lag4,data=data)
summary(lm5)
plot(lm5)
checkresiduals(lm5)
#add NLPG
lm6=lm(UGS~trend+Seasons+NDGV+PG+NLPG,data=data)
summary(lm6)
checkresiduals(lm6)

#best one 
#add PG
lm4=lm(UGS~trend+Seasons+NDGV+PG+lag2+NUGV+lag1,data=data)
summary(lm4)
checkresiduals(lm4)

#Fitting the Model and Prediction 
tmp=copy(data)
is.data.frame(tmp)
actual<-tmp$UGS
predicted<-predict(lm4,tmp)
tmp$actual<-actual
tmp$predicted<-predicted

#plot the predicted value and the actual ones
ggplot(tmp[c(3:28),], aes(x=Quarter))+geom_line(aes(y=actual, color="real"))+geom_line(aes(y=predicted, color="prediction"))+theme_grey()+
  ggtitle("Quarterly Unleaded Gasoline Sales Between 2000/Q1 to 2006/Q4 Predicted and Actual Values")+xlab("Years")+ylab("UGS")


predictions=predict(lm4,newdata =data[29:32,])
predictions

tmp[ 30, "lag1"] = 662156.1 
tmp$predictions2<-predict(lm4,tmp)

tmp[ 31, "lag1"] = tmp$predictions2[30]
tmp[ 31, "lag2"]= 662156.1
tmp$predictions2<-predict(lm4,tmp)

tmp[ 32, "lag1"] = tmp$predictions2[31]
tmp[ 32, "lag2"]= tmp$predictions2[30]
tmp$predictions2<-predict(lm4,tmp)

#With forecasted values
ggplot(tmp[c(3:32),], aes(x=Quarter))+geom_line(aes(y=actual, color="real"))+geom_line(aes(y=predictions2, color="prediction"))+theme_grey()+
  ggtitle("Quarterly Unleaded Gasoline Sales Between 2000/Q1 to 2006/Q4 Predicted and Actual Values")+xlab("Years")+ylab("UGS")

results<-data.frame(`predicted UGS sales`=tmp$predictions2[29:32],row.names=c("2007-Q1","2007-Q2","2007-Q3","2007-Q4"))
results




#Fitting the Model and Prediction 
tmp2=copy(data)
is.data.frame(tmp)
actual<-tmp2$UGS
predicted<-predict(lm7,tmp2)
tmp2$actual<-actual
tmp2$predicted<-predicted

#plot the predicted value and the actual ones
ggplot(tmp2[c(3:28),], aes(x=Quarter))+geom_line(aes(y=actual, color="real"))+geom_line(aes(y=predicted, color="prediction"))+theme_grey()+
  ggtitle("Quarterly Unleaded Gasoline Sales Between 2000/Q1 to 2006/Q4 Predicted and Actual Values")+xlab("Years")+ylab("UGS")


predictions=predict(lm4,newdata =data[29:32,])
predictions

tmp[ 30, "lag1"] = 662156.1 
tmp$predictions2<-predict(lm4,tmp)

tmp[ 31, "lag1"] = tmp$predictions2[30]
tmp[ 31, "lag2"]= 662156.1
tmp$predictions2<-predict(lm4,tmp)

tmp[ 32, "lag1"] = tmp$predictions2[31]
tmp[ 32, "lag2"]= tmp$predictions2[30]
tmp$predictions2<-predict(lm4,tmp)

#With forecasted values
ggplot(tmp[c(3:32),], aes(x=Quarter))+geom_line(aes(y=actual, color="real"))+geom_line(aes(y=predictions2, color="prediction"))+theme_grey()+
  ggtitle("Quarterly Unleaded Gasoline Sales Between 2000/Q1 to 2006/Q4 Predicted and Actual Values")+xlab("Years")+ylab("UGS")