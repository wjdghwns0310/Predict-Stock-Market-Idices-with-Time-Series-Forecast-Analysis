
#-----Step 1 Specify time series----------------------------------------------------------------
## read and clean data
mydata = read.csv('CAC 40 Historical Data Weekly.csv', header = TRUE)
#mydata = na.omit(mydata)
mydata$Close = as.numeric(mydata$Close)

## End of Day Index from 1991 Jan 1st to 2020 December 22th
cac40.ts = ts(mydata$Close, frequency = 52, c(1991,1))

## a few seasonal dips are observed every 5~7 years.
plot(cac40.ts)
#there's a trend, no obvious seasonality
plot(diff(cac40.ts))
monthplot(diff(cac40.ts))

y<-diff(log(cac40.ts))
plot(y)


#-----Step 2 model estimation, validation ----------------------------------------------------------------
## decide MA() or AR() by acf or pacf
par(mfrow = c(1,2))
acf(y)
Box.test(y)
pacf(y)
par(mfrow = c(1,1))

## MA3
model1 = arima(log(cac40.ts), order = c(0,1,3))
summary(model1)

#ma3 is significant

# model1 validation - residuals acf chart & box test
acf(model1$residuals)
Box.test(model1$residuals)
#p-value is 0.9373, no significant correlation between residuals;
#ma3 model is validated

## ar3
model2 = arima(log(cac40.ts), order = c(3,1,0))
summary(model2)
#ar3 is significant
# model1 validation - residuals acf chart & box test
acf(model2$residuals)
Box.test(model2$residuals)
#p-value is 0.9836, no significant correlation between residuals;
#ar3 model is validated

#-----Step 3 model comparison----------------------------------------------------------------
#in-sample
BIC(model1);AIC(model1)
BIC(model2);AIC(model2)
#model2-AR3's BIC is smaller, thus AR3 is better
#model2-AR3's AIC is smaller as well. 

#out-sample
#model1 - ma3
y=log(cac40.ts)
S=round(0.75*length(y))
h=1
error1.h=c()
for (i in S:(length(y)-h))
{
  model1.sub=arima(y[1:i],order=c(0,1,3))
  predict.h=predict(model1.sub,n.ahead=h)$pred[h]
  error1.h=c(error1.h,y[i+h]-predict.h)
}

MAE1<-mean(abs(error1.h)); MAE1
MSE1<-mean(abs(error1.h)^2); MSE1
length(error1.h)
#model2 - ar3
y=log(cac40.ts)
S=round(0.75*length(y))
h=1
error2.h=c()
for (i in S:(length(y)-h))
{
  model2.sub=arima(y[1:i],order=c(3,1,0))
  predict.h=predict(model2.sub,n.ahead=h)$pred[h]
  error2.h=c(error2.h,y[i+h]-predict.h)
}

MAE2<-mean(abs(error2.h)); MAE2
#MAE2 is sightly higher than MAE1 - let's do a DM test to see whether the difference is significant
MSE2<-mean(abs(error2.h)^2); MSE2
#MSE2 turns out to be greater than MSE1. DM test is needed.
library(forecast)
dm.test(error1.h,error2.h,h=h,power=1)
dm.test(error1.h,error2.h,h=h,power=2)
#p=value=0.7035, thus MAE1 and MAE2 are not significantly different; so aren't MSE1 MSE2
#As model2 - AR3's BIC is smaller, we take AR3 as final model

#Let's double check with auto-modeling method
myforecast=forecast(log(cac40.ts),method='arima')
myforecast$model
plot(myforecast, xlim = c(2019, 2022))
#the best model is ar3

#-----Step 4 prediction & visualization----------------------------------------------------------------
#prediction for the next 8 weeks
myforecast<-predict(model2,n.ahead=8)

expected=exp(myforecast$pred)
alpha=0.05;q=qnorm(1-alpha/2)
lower=exp(myforecast$pred-q*myforecast$se);
upper=exp(myforecast$pred+q*myforecast$se);
cbind(lower,expected,upper)

plot(cac40.ts,xlim=c(2019,2022))
lines(expected,col="red")
lines(lower,col="blue")
lines(upper,col="blue")
# The red line gives the point forecasts
# Blue line is the 95% prediction interval
# Note that the prediction interval (corresponding to the blue lines) 
# widens if the forecast horizon increases, as expected.
# The result is not informative, can't help to make a decision

#-----Further exploration----------------------------------------------------------------
#we shorten the time series, only using data from 2016-01-05 to 2020-12-22
mydata2 = read.csv('CAC 40 Historical Data Weekly shorter.csv', header = TRUE)
cac40.ts2 = ts(mydata2$Close, frequency = 52, c(2016,1))
length(cac40.ts2)
## a few seasonal dips are observed every 5~7 year.
plot(cac40.ts2)
y2<-diff(log(cac40.ts2))
plot(y2)
acf(y2)
Box.test(y2)
#p>0.05, not significant, the time series are white noises, unpredictable
myforecast2=forecast(log(cac40.ts2),method='arima')
myforecast2$model
#auto-modeling recommends a random walk model