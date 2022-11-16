

install.packages("forecast")
install.packages("tseries")
library(forecast)
library(tseries)


Path<-"D:/R assignments/Case Study 5"
setwd(Path)
getwd()

data<-read.csv("1sales.csv",header = TRUE)
TSdata=data

data

head(TSdata)
dim(TSdata)
str(TSdata)
summary(TSdata)
colSums(is.na(TSdata))
names(TSdata)[c(1:2)]=c("date","sales")
class(TSdata)


class(TSdata$date)

TSdata$date <- as.Date(TSdata$date, format = "%b-%y")


TSdata=ts(TSdata[,2],start=c(2003,1),frequency=12)
class(TSdata)
start(TSdata)
end(TSdata)
frequency(TSdata)
str(TSdata)
TSdata


plot(TSdata,ylab="Sales", xlab="Year",main="Sales between 2003-2017",col="blue")
abline(reg = lm(TSdata~time(TSdata)))
cycle(TSdata)
plot(aggregate(TSdata,FUN=mean))


plot(log10(TSdata),ylab="log(Sales)",xlab="Year",main="log(Sales) between 2003-2017",col="orange")
plot(diff(TSdata,differences = 1),ylab="Diff(Sales)",xlab="Year",main="Diff(Sales) between 2003-2017",col="orange")

plot(diff(log10(TSdata),differences = 1),ylab="Diff(Sales)",xlab="Year",main="Diff(Log(Sales)) between 2003-2014",col="orange")


LDTSdata=diff(log10(TSdata),differences = 1)
adf.test(LDTSdata,alternative="stationary")
kpss.test(LDTSdata)


par(mfrow=c(1,2))
acf(diff(log10(TSdata)),main="ACF plot")
pacf(diff(log10(TSdata)),main="PACF plot")


ARIMAFit=arima((log10(TSdata)),c(0,1,1))#p=0, d = 1, q = 1
ARIMAFit1=arima((log10(TSdata)),c(0,1,2))#p=0, d = 1, q = 2
ARIMAFit2=arima((log10(TSdata)),c(1,1,1))#p=1, d = 1, q = 1
ARIMAFit3=arima((log10(TSdata)),c(2,1,12))#p=1, d = 1, q = 2


summary(ARIMAFit)
summary(ARIMAFit1)
summary(ARIMAFit2)
summary(ARIMAFit3)



require(forecast)
ARIMAFit1=auto.arima(log10(TSdata),approximation=TRUE,trace=TRUE)
summary(ARIMAFit1) #Best model is found to be ARIMA(0,2,1)
ARIMAFit1$residuals


pred=predict(ARIMAFit1,n.ahead=36)
pred


par(mfrow=c(1,1))
plot(TSdata,type="l",xlim=c(2003,2020),ylim=c(1,120000),xlab="Year",ylab="Sales")
lines(10^(pred$pred),col="red")


plot(TSdata,type="l",xlim=c(2003,2020),ylim=c(1,260000),xlab = "Year",ylab = "Sales")
lines(10^(pred$pred),col="red")
lines(10^(pred$pred+2*pred$se),col="blue")
lines(10^(pred$pred-2*pred$se),col="green")


pred = predict(ARIMAFit1, n.ahead = 36)
write.csv(pred,"predict.csv")


normal_result=10^pred$pred 
class(normal_result)
normal_result_df<-as.data.frame(normal_result)
Date_Pred_seq<-NULL
Date_Pred_seq<-as.data.frame(seq(as.Date("2017/02/01"),as.Date("2020/01/01"),by = "month"))
final_result_df = cbind(Date_Pred_seq,normal_result_df)
library(dplyr)
colnames(final_result_df)<-c("Date","Sales_Predicted")
final_result_df
write.csv(normal_result,"finalpredict.csv", row.names = FALSE)


plot(normal_result)
