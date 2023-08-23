library(forecast)

MetGlobalData<-read.csv(file.choose(), header=TRUE, sep=",")

MetGlobal_ts <- ts(MetGlobalData$anomaly_c + 14.0,start=1850, frequency=12)

par(mfrow=c(1,3))
plot(MetGlobal_ts, xlab="Year",
     ylab="Global Temp")
plot(log(MetGlobal_ts), xlab="Year",
     ylab="log Global Temp")
plot(diff(log(MetGlobal_ts),12), xlab="Year",
     ylab="Annual change in monthly log Global Temp")

fit <- stl(log(MetGlobal_ts), t.window=12, s.window="periodic", robust=TRUE)
plot(fit)

# auto-correlation function
Acf(MetGlobal_ts,main="") # data "as is"
Acf(log(MetGlobal_ts),main="") # log-transformed data
Acf(diff(log(MetGlobal_ts),12),main="") # difference-12 log data

# partial auto-correlation function
par(mfrow=c(1,2))
Acf(diff(log(MetGlobal_ts),12),main="")
Pacf(diff(log(MetGlobal_ts),12),main="") 

fit <- auto.arima(MetGlobal_ts,seasonal=FALSE)
fit

fit <- auto.arima(MetGlobal_ts,seasonal=TRUE)
fit

par(mfrow=c(1,1))
Acf(residuals(fit))
plot(forecast(fit,918))
