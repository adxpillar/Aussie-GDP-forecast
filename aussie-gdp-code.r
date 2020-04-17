# Forecasting growth rate of REAL GDP for Australia 

library(fpp)
library(tseries)
show(AUSdata)
aussie <- c(AUSdata[,2:6])

#---------Inflation

inflation <- ts(aussie$inflation, start=1990,frequency = 4)
show(inflation)
plot(inflation,col=2)

inf_training <- window(inflation, start = (1990), end=c(2015,4))
inf_training 

inf_test <- window(inflation, start = 2016)
inf_test

# fit, forecast, accuracy
fitt5 <- auto.arima(inf_training)
summary(fitt5)
res_5 <- residuals(fitt5)
plot(res_5)
acf(res_5)
pacf(res_5)
#par(mfrow=c(3,1))
fcst_5 <- forecast(fitt5,h=12)
fcst_5
plot(fcst_5)
accuracy(forecast(fitt5, h=8), inf_test)

#--------Productivity

productivity <- ts(aussie$productivity,start = 1990,frequency = 4)
plot(productivity,col=3)

prod_training <- window(productivity, start = (1990), end=c(2015,4))
prod_training 


prod_test <- window(productivity, start = 2016)
prod_test

# fit, forecast, accuracy
fitt4 <- auto.arima(prod_training)
summary(fitt4)
res_4 <- residuals(fitt4)
plot(res_4)
acf(res_4)
pacf(res_4)
#par(mfrow=c(3,1))
fcst_4 <- forecast(fitt4,h=12)
fcst_4
plot(fcst_4)
accuracy(forecast(fitt4, h=8), prod_test)

#--------------labor 

labor <- ts(aussie$labor,start = 1990,frequency = 4)
plot(labor,col=4)

labor_training <- window(labor, start = (1990), end=c(2015,4))
labor_training 

labor_test <- window(labor, start = 2016)
labor_test

# fit, forecast, accuracy
fitt3 <- auto.arima(labor_training)
summary(fitt3)
res_3 <- residuals(fitt3)
plot(res_3)
acf(res_3)
pacf(res_3)
##par(mfrow=c(3,1))
fcst_3 <- forecast(fitt3,h=12)
fcst_3
plot(fcst_3)
accuracy(forecast(fitt3, h=8), labor_test)


#-------------Openness

openness <- ts(aussie$openness,start = 1990,frequency = 4)
plot(openness,col=6)

openness_training <- window(openness, start = (1990), end=c(2015,4))
openness_training 

openness_test <- window(openness, start = 2016)
openness_test

# fit, forecast, accuracy
fitt2 <- auto.arima(openness_training)
summary(fitt2)
res_2 <- residuals(fitt2)
plot(res_2)
acf(res_2)
pacf(res_2)
# par(mfrow=c(3,1))
fcst_2 <- forecast(fitt2,h=12)
fcst_2
plot(fcst_2)
accuracy(forecast(fitt2, h=8), openness_test)

#-----------GDPrate 

gdprate <- ts(aussie$`GDP`,start = 1990,frequency = 4)
plot(gdprate,col=9)
# GDP Training 
gdprate_training <- window(gdprate, start = (1990), end=c(2015,4))
gdprate_training 
# GDP Test
gdprate_test <- window(gdprate, start = 2016)
gdprate_test
  # fit, forecast, accuracy
fitt1 <- auto.arima(gdprate_training)
summary(fitt1)
res_1 <- residuals(fitt1)
plot(res_1)
acf(res_1)
pacf(res_1)
#par(mfrow=c(3,1))
fcst_1 <- forecast(fitt1,h=12)
fcst_1
plot(fcst_1)
accuracy(forecast(fitt1, h=8), gdprate_test)


#------Regression Model

tslm(formula = gdprate ~ inflation + openness + labor + productivity)

#---------Using exogenous variables to forecast dependent variable 

g <- fcst_1$mean
g
o <- fcst_2$mean
o
l <- fcst_3$mean
l
p <- fcst_4$mean
p
i <- fcst_5$mean
i

pack_1 <- cbind(openness_test,labor_test,inf_test,prod_test)
pack_2 <- cbind(gdprate_test,openness_test,labor_test,inf_test,prod_test)

packed <- ts.intersect(g,o,l,p,i)
aussie_joined <- rbind(pack_2,packed)
class(aussie_joined)

aussie2 <- ts(aussie_joined, start = (2016),end=c(2018,4),frequency = 4)
aussie2

Dynamic_fitt_aussie2 <- auto.arima(aussie2[,1], xreg = (aussie2[,2:5]))
summary(Dynamic_fitt_aussie2)
checkresiduals(Dynamic_fitt_aussie2)


regressionn_model_errors <- residuals(Dynamic_fitt_aussie2, type ="regression")
regressionn_model_errors
plot(regressionn_model_errors)

aus_arima_errors <- residuals(Dynamic_fitt_aussie2, type ="innovation")
aus_arima_errors
plot(aus_arima_errors)

error_combination <- cbind(regressionn_model_errors, aus_arima_errors)
plot(error_combination)

#######---------forecast 3 years ahead 
#dynamic forecast

dynamic_fcast <- forecast(Dynamic_fitt_aussie2, xreg=(aussie2[,2:5]))
dynamic_fcast
plot(dynamic_fcast)
