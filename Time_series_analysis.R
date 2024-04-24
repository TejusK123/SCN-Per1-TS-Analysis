rm(list = ls())
library(forecast)
library(ggplot2)
library(car)
library(patchwork)


setwd("C:/bernd/2023-2024/MAT_482/time_series_code")

fulldata <- read.csv('SCNonlyintensity.csv')

#Setting up training and testing data
clipped.data <- fulldata[1:1440, 1:3]
testdata <- fulldata[1441:1685, 1:3]




#Converting data to timeseries objects
SCNtimeseries = ts(clipped.data$Intensity.rfu, start = 0, frequency = 240)
testdata.ts <- ts(testdata$Intensity.rfu, start = 6, freq = 240)


#getting SST
SST <- sum((SCNtimeseries - mean(SCNtimeseries))^2)


#Preliminary Data Visualization######################

#######Technically, not a scatter plot, just a plot.
#######Could simply avoid words with "plot" in the titles.
#scatter of clipped data
(ggplot(data = clipped.data, aes(x = Time.hrs., y = Intensity.rfu))
  + geom_point() 
 + ggtitle("Scatterplot of Light Intensity of Venus Fluorophore in Mouse Suprachiasmatic Nucleus")
 )

#Timeseries plot of data
autoplot(SCNtimeseries) + xlab("Time(Days)") + ylab("Intenstiy(RFU)") + ggtitle("TimeSeries of Light Intensity of Venus Fluorophore in Mouse Suprachiasmatic Nucleus")


#decomposition of timeseries
plot(decompose(SCNtimeseries))
#######definitely a nice periodic part
#######interesting how the many (240) parameters for the seasonal fit might be needed. :) 

#################################################


#Beginning seasonal indicator regression####################################3



#seems neat to manually adjust the importance of trend
#with the exponent but this will definetely make future
#predictions out of this data way wrong
#(as time goes on, the prediction will shoot to the moon)
#######Nice work on this fit. I had forgotten (or did not know) that the seasonal
#######fit need not be done by hand. Is this in forecast?
#######How many parameters does the trend fit take versus a time fit?
fit2 <- tslm(SCNtimeseries ~ trend + I(trend ^ 3) + season)

t1 <- forecast(fit2, h = 240)
forecastreg <- predict(fit2, newdata = t1$newdata)


pred <- forecast(fit2, h = 240)


####seasonal regression plots##

ggplot(data = SCNtimeseries, aes(x = x, y = y)) + geom_line() + xlim(c(0,7.1)) + ylim(c(1000,1800)) + geom_line(data = t1$mean, col = 'red') + xlab("Time(Days)") + ylab("Intensity(RFU)") + ggtitle("Seasonal Regression Model Forecast") + geom_line(data = ts(t1$fitted, start = 0, frequency = 240), col = 'blue', size = 1, alpha = 0.3) 
autoplot(resid(fit2)) + geom_hline(yintercept = 0, col = 'blue') + ggtitle("Residuals of Seasonal Regression") + ylab("Residuals") + xlab("Time(Days)")
autoplot(acf(resid(fit2), lag.max = 240)) + ggtitle("Autocorrelation Seasonal Regression Residuals (lags 1 - 240)")
#####

#residual analysis

resids <- resid(fit2)
########I had figured that resid gives a time series. :)
residsts <- ts(data = resids, start = 0, freq = 240)

#goodness of fit/explained variance
SSE_reg <- sum((SCNtimeseries - t1$fitted)^2)
R2_reg <- 1 - (SSE_reg/SST)


########Autoregressive part

arresids = ar(residsts)
########Can just use the output of ar(). 
########Then again, this way the code translates to get.best.arima
########Arima() is part of forecast, arima() is regular R
arresids_auto <- Arima(residsts, order = c(arresids$order,0,0))
########14 feels like a high order, but you have a lot of data.

predar <- predict(arresids, n.ahead = 240)


new_forecast_w_ar <- forecast(arresids_auto, h = 240)$mean + t1$mean
new_predictions_w_ar <- arresids_auto$fitted + fit2$fitted.values


####plots

ggplot(data = SCNtimeseries, aes(x = x, y = y)) + geom_line() + xlim(c(0,7.1)) + ylim(c(1000,1800)) + geom_line(data = new_forecast_w_ar, col = 'red') + xlab("Time(Days)") + ylab("Intensity(RFU)") + ggtitle("Seasonal Regression Model With Autoregressive Residual + Forecast") + geom_line(data = new_predictions_w_ar, col = 'blue', size = 1, alpha = 0.3)

ar_plus_reg_resids <- SCNtimeseries - new_predictions_w_ar
autoplot(ar_plus_reg_resids) + ylab("Residual") + xlab("Time(Days)") + ggtitle("Residuals of Combined Seasonal Regression AR model") + geom_hline(yintercept = 0, col = 'blue')
autoplot(acf(resid(arresids_auto), lag.max = 240)) + ggtitle("Autocorrelation AR Residuals (lags 1 - 240)")
######11 of 240 is well within the 5% threshold, even if the different format
######with many more points and no 1 (and the one outlier) made me think
######differently first.

#############


#goodness of fit/explained variance
SSE_reg_ar <- sum((SCNtimeseries - new_predictions_w_ar)^2)
R2_reg_ar <- 1 - (SSE_reg_ar/SST)


###########################arima

#########No need to recompute, but different name can help keep
#########things organized. 
ts_resids_fit2 <- ts(resid(fit2), start = 0, frequency = 240)


#Found auto.arima to be more efficient than the best.fit function from the CM book
########Works for me as long as it works. The way to go when using programming 
########languages is to find the best packages. For teaching, I did not 
########want to use too many blcok boxes, and get.best.arima() nicely shows
########how a black box like auto.arima() likely works. :) 
########Stopped here because code ran too long. Missing the updates how far the code has gone, I guess.  ;) 
########How long does this run on what type of machine?
########That will help me next time. :) 
########Remainder looks good. Not expecting issues. 
best_fit_D1_seas <- auto.arima(ts_resids_fit2, D = 1)
#Results in ARIMA(4,1,3)(0,1,0)





new_resids_forecast <- forecast(best_fit_D1_seas)$mean
new_predict_w_arima <- best_fit_D1_seas$fitted + t1$fitted
new_forecast_w_arima <- t1$mean + new_resids_forecast


####Plots###

ggplot(data = SCNtimeseries, aes(x = x, y = y)) + geom_line() + xlim(c(0,7.1)) + ylim(c(1000,1800)) + geom_line(data = ts(new_forecast_w_arima, start = 6, frequency = 240), col = 'red') + xlab("Time(Days)") + ylab("Intensity(RFU)") + ggtitle("Seasonal Regression Model With ARIMA residuals Forecast") + geom_line(data = new_predict_w_arima, col = 'blue', size = 1, alpha = 0.3)

arima_plus_reg_resids <- SCNtimeseries - new_predict_w_arima
autoplot(arima_plus_reg_resids) + ggtitle("Combined Regression plus ARIMA residuals") + ylab("Residuals") + xlab("Time(days)") + geom_hline(yintercept = 0, col = 'blue')
autoplot(acf(arima_plus_reg_resids, lag.max = 240)) + ggtitle("Autocorrelation ARIMA residuals (lags 1 - 240)")

#########


#goodness of fit/explained variance
SSE_reg_arima <- sum((SCNtimeseries - new_predict_w_arima)^2)
R2_reg_arima <- 1 - (SSE_reg_arima/SST)




##nnet_nonlinearity_accounting#####

#Inspiration from a hybrid ARIMA-ANN model (Reference in submitted paper)
nnet_arima_resids <- nnetar(resid(best_fit_D1_seas), P = 2,p = 2, repeats = 200, size = 2)
forecast_nnet <- forecast(nnet_arima_resids, h = 240)
forecast_nnet$fitted[is.na(forecast_nnet$fitted)] <- 0
new_resids_forecast_nnet <- forecast_nnet$mean


new_predict_w_arima_nnet <- best_fit_D1_seas$fitted + t1$fitted + forecast_nnet$fitted
new_forecast_w_arima_nnet <- new_resids_forecast + new_resids_forecast_nnet + t1$mean

########plots#####

ggplot(data = SCNtimeseries, aes(x = x, y = y)) + geom_line() + xlim(c(0,7.1)) + ylim(c(1000,1800)) + geom_line(data = ts(new_forecast_w_arima_nnet, start = 6, frequency = 240), col = 'red') + xlab("Time(Days)") + ylab("Intensity(RFU)") + ggtitle("Seasonal Regression Model With ARIMA-ANN residuals Forecast") + geom_line(data = new_predict_w_arima_nnet, col = 'blue', size = 1, alpha = 0.3) 

arimaANN_plus_reg_resids <- SCNtimeseries - new_predict_w_arima_nnet
autoplot(arimaANN_plus_reg_resids) + ggtitle("Combined Regression plus ARIMA residuals") + ylab("Residuals") + xlab("Time(days)") + geom_hline(yintercept = 0, col = 'blue')
#######Wow, much better autocorrelations.
autoplot(acf(resid(nnet_arima_resids)[481:1440], lag.max = 240)) + ggtitle("Autocorrelation ARIMA-ANN residuals (lags 1-240)")
####################

#goodness of fit/explained variance
SSE_reg_arimaANN <- sum((SCNtimeseries - new_predict_w_arima_nnet)^2)
R2_reg_arimaANN <- 1 - (SSE_reg_arimaANN/SST)





#ORACLE Plot###############

p1_diff <- (autoplot(testdata.ts) 
  + autolayer(new_forecast_w_arima_nnet) 
  + autolayer(new_forecast_w_arima) 
  + autolayer(new_forecast_w_ar)
  + autolayer(t1$mean, alpha = 0.5) 
  + scale_color_manual(values = c('purple', 'red', 'blue', 'green'))
  #+ geom_rect(aes(xmin = 6.7, xmax = 6.77, ymin = 1450, ymax = 1470), col = 'black', alpha = 0)
  + ggtitle("Different Models Compared to the Oracle")
  +ylab("Intensity(rfu)")
  + geom_rect(aes(xmin = 6.4, xmax = 6.55, ymin = 1475, ymax = 1575), col = 'black', alpha = 0)
  #+ ylim(1200, 1700)
  )


p2_top_diff <- ggplot(testdata.ts, aes(x = x, y = y)) + xlab("Time") + ylab("Intensity(rfu)") + geom_line(col = 'black') + geom_line(data = new_forecast_w_arima, aes(x = x, y = y), col = 'red') + geom_line(data = new_forecast_w_arima_nnet, aes(x = x, y = y), col = 'blue') + xlim(6.4, 6.55) + ylim(1475, 1575) + geom_line(data = t1$mean, aes(x = x, y = y), col = 'green') + geom_line(data = new_forecast_w_ar, aes(x = x, y = y), col = 'purple')

(p1_diff 
  + geom_path(aes(x,y,group=grp), 
            data=data.frame(x = c(6.4, 6.23, 6.55, 6.8), y=c(1475,1445,1475,1445),grp=c(1,1,2,2)),
            linetype='dashed') 
 + annotation_custom(ggplotGrob(p2_top_diff), xmin = 6.23, xmax = 6.8, ymin = 1250, ymax = 1445) 
  +geom_rect(aes(xmin = 6.23, xmax = 6.8, ymin = 1250, ymax = 1445), col = 'black', linetype = 'dashed', alpha = 0)
  
)





reg_difference <- testdata.ts - t1$mean
ar_difference <- testdata.ts - new_forecast_w_ar
arima_difference <- testdata.ts - new_forecast_w_arima
ANN_difference <- testdata.ts - new_forecast_w_arima_nnet

p1 <- autoplot(reg_difference) + geom_hline(yintercept = 0, col = 'blue') + labs(caption = "Sum_Squares: 440819.9")
p2 <- autoplot(ar_difference) + geom_hline(yintercept = 0, col = 'blue') + labs(caption = "Sum_Squares: 452081.1")
p3 <- autoplot(arima_difference) + geom_hline(yintercept = 0, col = 'blue') + labs(caption = 'Sum_Squares: 153931.9')
p4 <- autoplot(ANN_difference) + geom_hline(yintercept = 0, col = 'blue') + labs(caption = 'Sum_Squares: 145488.3')


p5 <- p1 + p2 + p3 + p4 + plot_annotation(title = 'Comparison of differences between oracle and model forecasts')
p5



#time + time^2, d = 1 == 739.7989
#time + time^3, d = 1 == 628.2936 AIC 8786
#time, d = 1 == 916.5558
#time + time^3, d = 2 == 3356.449 AIC 8784
#time + time^3 + nnet_resids, d = 1, == 597.645
#######################