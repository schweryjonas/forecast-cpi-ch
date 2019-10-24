# Time series analysis - project
# consumer price indices in Switzerland

# Data extraction and plot of raw series 
raw.data <- read.csv("C:/studium/statistik/master/second-semester/time-series-analysis/ts-project/cpi_ch_oecd.csv", header = TRUE)
summary(raw.data)

# fix the LOCATION string we read from the csv file
colnames(raw.data)[c(1,7)] <- c("LOCATION", "CPI_VALUE")
attach(raw.data)


# load package to transform the data
#install.packages("dplyr") #-> if package dplyr not installed
library(dplyr) 

# extract the relevant columns and observations for switzerland and 
# get rid of the metadata 
cpi.switzerland <- raw.data %>% 
                      select( TIME, CPI_VALUE ) %>% 
                          filter( grepl("CHE", LOCATION) )  
cpi.switzerland                           

# check the min and max value for the date
head(cpi.switzerland)
tail(cpi.switzerland)
# we have monthly cpi data for switzerland from January 1956 to December 2018

# convert cpi.switzerland to a ts object
( ts.cpi.switzerland <- ts(cpi.switzerland, start = c(1956,1), end = c(2018, 12), frequency = 12) )

# check the raw time series
ts.cpi <- ts.cpi.switzerland[,2]
plot.ts(ts.cpi, ylab="inflation rate in %", xlab="")
# at the first glance, there is no seasonality and we can identify
# a negative linear trend
# the variance decreases with time

# choice of trend/seasonality removal method 
# trend
spline.cpi.switzerland <- smooth.spline(ts.cpi)
lines(spline.cpi.switzerland, col="red")
# Then it is also possible to check weather there is a linear trend or quadratic trend, etc.
spline2.cpi.switzerland <- smooth.spline(ts.cpi, df = 2)
spline3.cpi.switzerland <- smooth.spline(ts.cpi, df = 3)
lines(spline2.cpi.switzerland, col="blue")
lines(spline2.cpi.switzerland, col="green")
# Check if we have a linear trend
t   <- c(1:length(ts.cpi))
summary(lm.cpi <- lm(ts.cpi ~ t))
# check if we have a quadratic trend
tt  <- t^2
summary(lm2.cpi <- lm(ts.cpi ~ t+tt))
# check if we have a polynomial trend of degree 3
ttt <- t^3
summary(lm3.cpi <- lm(ts.cpi ~ t+tt+ttt))
# conclusion: there is a significant polynomial trend of degree 3
# therefore it seems not appropriate to remove the trend with linear regression and we
# should go for the differentiation method


# Differentiation: lag 1
d1.cpi <- diff(ts.cpi, lag=1)
plot(d1.cpi, ylab="Lag-1 difference of inflation rate", xlab="", type="l")

# check if we have a significant seasonality
season.model <- model.matrix(~ 1 + factor(cycle(ts.cpi)))[,-1]
dimnames(season.model)[[2]] <- c("February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

summary(test1 <- lm(ts.cpi ~ season.model))
t <- 1:length(ts.cpi)
summary(test2 <- lm(ts.cpi ~ t + season.model))
tt <- t^2
summary(test3 <- lm(ts.cpi ~ t + tt + season.model))
ttt <- t^3
summary(test4 <- lm(ts.cpi ~ t + tt + ttt + season.model))
# so there is no seasonality in the data

# So, our Xt is the series d1.cpi, which we got after differentiating once
Xt <- d1.cpi

# check if our Xt is centered
mu <- mean(Xt)
mu
# the mean is slightly positive, so we substract the mean of Xt to get a zero-mean
# time series
Xt <- Xt - mu
plot(Xt, type="l")
mean(Xt)

# Ljung-Box test to check if our time series is a white noise
# test for 100 lags
n <- 100
box.results <- numeric(n)
for (i in 1:n) {
  box.results[i] <- Box.test(x = Xt, lag=i, type = "Ljung-Box")$p.value
}
box.results < 0.05
mean(box.results < 0.05)
# we clearly reject the HO, that our time series Xt is an iid-noise

# now we can use the adf test, to check if we really have a stationary time series
# H0: presence of unit root in the time series
# H1: the time series is stationary
# install.packages("tseries")
library(tseries)
adf.test(Xt)
# since the pvalue is smaller than 0.01, we can reject H0 and conclude that our time
# series Xt is stationary



# guessing a model with the ACF/PACF
mx= 80 # max lags to plot
# sample autocovariance function
acf(Xt, lag.max=mx, xaxt="n", xlab="Lag (months)", main="")
axis(1, at=0:mx/12, labels=0:mx)

# sample acf
pacf(x = Xt, lag.max = mx, xaxt="n", xlab = "Lag (months)", main="")
axis(1, at=0:mx/12, labels=0:mx)
# the sample act clearly shows significant spikes at lag1 1, 12, 24, 36, ..
# see jonas_schwery.pdf for comments about the guess

# lets choose a model
library(itsmr)
library(forecast)
auto.arima(y=Xt,d=0,D=0,max.p=12,max.q=12,max.P=12,max.Q=5,max.order=5,stepwise=TRUE,trace=TRUE,seasonal=TRUE, allowmean = FALSE)
model <- Arima(y = Xt,order = c(5,0,5),seasonal = c(1,0,4),include.constant = FALSE)
model
# there seems to be an issue with the standard deviations
model$coef
model$sigma2
model$var.coef 
sqrt(diag(model$var.coef))
# we have negative values in the diagonal of the variance covariance matrix, this should not happen

# but check agian the Q's, since the last 2 coefficients of Q don't seem significant, they are very low
max.Q <- 4
AIC <- matrix(0, nrow = 1, ncol=max.Q)
for(Q in 1:max.Q) {
    AIC[1,Q] <- Arima(y=Xt,order = c(5,0,5),seasonal = c(1,0,Q),include.constant = FALSE)$aicc
}
AIC
# the evaluation of the AICc showed that one can decrease the parameters for Q to 2
# so the final model is 
model <- Arima(y = Xt,order = c(5,0,5),seasonal = c(1,0,2),include.constant = FALSE)
model
# and we have positive values in the diagonal if the variance covariance matrix


# check if the model is valid
plotc(model$residuals)
mx=24
acf(model$residuals,lag.max=mx, xaxt="n", xlab="Lag (months)", main="") 
axis(1, at=0:mx/12, labels=0:24)

plot(sapply(1:24, function(x)Box.test(model$residuals, lag =x, type = "Ljung-Box")$p.value), main = "", xlab = "lag", ylab="p-value", ylim=c(0,1))
abline(h = 0.05, col="blue", lty = 2)

tsdiag(model)
# the model is valid:
# - we can not identify a pattern in the standardized residulas
# - the acf of the residulas is clearly a white noise
# - for all the tested lags in the Ljung-Box test, we can not reject
#   H0, that the residulas is an iid noise

# now we can predict one year ahead, and so we get 12 predictions for the months 
# of the year 2019
h <- 12
pred <- predict(object=model,n.ahead = h)

t <- 1:length(Xt)
n <-length(Xt)
plot(x=t[650:n] , y=as.vector(Xt[650:n]) , xlim = c(650,n+h), ylim = c(-1,1), type="l" , lwd=2,
     main="Forecast of the residual time series", xlab="t", ylab="")
lines(x=n:(n+h) , y = c(Xt[n],pred$pred) , col="red" , lwd=2 )
lines(x= (n+1):(n+h) , y = pred$pred + 1.96*pred$se , col="blue" , lwd=2  , lty = 2)
lines(x= (n+1):(n+h) , y = pred$pred - 1.96*pred$se , col="blue" , lwd=2  , lty = 2)

qqnorm(y = model$residuals)
qqline(y = model$residuals)
shapiro.test(model$residuals)
# we conclude that the residuals of the model are not normally distributed
# so we can't interpret the confidence bands as 95% confidence bands

# get the original time series back
# y_hat_t1 = yt + zt1 + mu
# y_hat_t2 = y_hat_t1 + zt2 + mu = yt + zt1 + zt2 + 2*mu = ...
ofc <- numeric(h)
ofc[1] <- ts.cpi[length(ts.cpi)] + pred$pred[1] + mu
for (i in 2:h) {
  ofc[i] <- ofc[i-1] + pred$pred[i] + mu
}
ofc

plot(x=t[649:756] , y=as.vector(ts.cpi[649:756]) , xlim = c(650,n+h), ylim = c(-2,2), type="l" , lwd=2,
     main="Forecast of the original time series", ylab="inflation rate in %", xaxt = "n")
axis(1, at=seq(660, 760, 20), labels=c("12-2010", "08-2012", "04-2014", "12-2015", "08-2017", "04-2019"))
lines(x=(n):(n+h) , y = c(as.vector(ts.cpi[n]),ofc) , col="red" , lwd=2 )
lines(x= (n+1):(n+h) , y = ofc + 1.96*pred$se , col="green" , lwd=2  , lty = 2)
lines(x= (n+1):(n+h) , y = ofc - 1.96*pred$se , col="green" , lwd=2  , lty = 2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend("bottomright", legend=c("Forecasted inflation rate", "Conficence bands"),
       col=c("red", "green"), lty=1:2, cex=0.8, box.lty=0)

round(ofc, 2)


# SARIMA - predict some data we already know 
# predict some data we know already with our model
cpi.switzerland2 <- cpi.switzerland[1:(nrow(cpi.switzerland)-12),]
head(cpi.switzerland2)
tail(cpi.switzerland2)
# extract the known months from the year 2018
known.months <- cpi.switzerland[(nrow(cpi.switzerland)-11):nrow(cpi.switzerland),]
known.months <- known.months[,2][1:12]

# now we have data from January 1956 until December 2017
# and forecast the year 2018
# since these values are known, we can access the accuracy of the prediction
ts.cpi.switzerland <- ts(cpi.switzerland2, start = c(1956,1), end = c(2017, 12), frequency = 12)
ts.cpi <- ts.cpi.switzerland[,2]
plot.ts(ts.cpi, main = "The raw CPI time series", ylab="CPI in %")
d1.cpi <- diff((ts.cpi), lag=1)
plot(d1.cpi, main="CPI, lag=1", ylab="diff pounds")

# center the series
Xt2 <- d1.cpi
mu2 <- mean(Xt2) 
mu2
Xt2 <- Xt2 - mu2

# SARIMA Model
model2 <- Arima(y = Xt2,order = c(5,0,5),seasonal = c(1,0,2),include.constant = FALSE)

plotc(model2$residuals)
acf(model2$residuals) 
tsdiag(model2) 

#12 months ahead
mypred2 <- predict(object=model2,n.ahead = h)
pred2 <- mypred2$pred
se2 <- mypred2$se

# SARIMA plot
plot(x=t[650:n] , y=as.vector(Xt[650:n]) , xlim = c(650,n+h), ylim = c(-1,1), type="l" , lwd=1,
     main="Forecast of the resudual time series (SARIMA)", xlab="t", ylab="")
lines(x=n:(n+h) , y = c(Xt[n], pred2) , col="red" , lwd=2 )

# original forecast of SARIMA model
ofc2 <- numeric(h)
ofc2[1] <- ts.cpi[length(ts.cpi)] + pred2[1] + mu2
for (i in 2:h) {
  ofc2[i] <- ofc2[i-1] + pred2[i] + mu2
}
ofc2
n <- length(ts.cpi)
t <- 1:n
plot(t[650:n], ts.cpi[650:n], type="l", xlim = c(650,n+h), ylim=c(min(ts.cpi), 2),
     main = "SARIMA forecast of the original time series", xlab = "t", ylab="inflation rate in %", xaxt="n")
axis(1, at=seq(660, 740, 20), labels=c("12-2010", "08-2012", "04-2014", "12-2015", "08-2017"))
lines(n:(n+h), c(ts.cpi[n],ofc2), col="red")
lines(n:(n+h), c(ts.cpi[n], known.months), col="blue")
lines(x= (n+1):(n+h) , y = ofc2 + 1.96*se2 , col="green" , lwd=2  , lty = 2)
lines(x= (n+1):(n+h) , y = ofc2 - 1.96*se2 , col="green" , lwd=2  , lty = 2)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend(680,2, legend=c("Forecasted inflation rate","Observed inflation rate", "Conficence bands"),
       col=c("red","blue", "green"), lty=c(1,1,2), cex=0.8, box.lty=0)
round(ofc2-known.months,2)

# in average we are 0.2% away form the known value
sum(abs(ofc2-known.months))/12


