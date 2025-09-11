######################################################################################
##############  Lab7: Forecasting: Seasonal Transfer Function Models  ################
######################################################################################

library(MLTools)
library(fpp2)
library(ggplot2)
library(TSA)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(Hmisc) # for computing lagged variables

## Set working directory ---------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.table("Seasonal_TF_1.dat",header = TRUE, sep = "")
colnames(fdata) <- c("Y","X")
# Convert to time series object
fdata_ts <- ts(fdata, frequency = 1)
head(fdata_ts)
autoplot(fdata_ts, facets = TRUE)

# Create time series and scale values 
y <- fdata_ts[,1]/10000
x <- fdata_ts[,2]/10000

ggtsdisplay(y, lag=50)
ts.plot(y, lwd=2)
abline(v = (1:8) * 24, col="red") 

freq <- 24
fdata_ts <- ts(fdata, frequency = freq)
# Create time series and scale values 
y <- fdata_ts[,1]/10000
x <- fdata_ts[,2]/10000

ggtsdisplay(y, lag= 4 * freq)


y.tr <- subset(y, end = 950)
y.ts <- subset(y, start = 951)

x.tr <- subset(x, end = 950)
x.ts <- subset(x, start = 951)

## Identification and fitting process -------------------------------------------------------------------------------------------------------

#### Fit initial FT model with large s
# This arima function belongs to the TSA package
TF.fit <- arima(y.tr,
                order=c(1, 0, 0),
                seasonal = list(order=c(1, 1, 0), period=freq),
                xtransf = x.tr,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) # summary of training errors and estimated coefficients
coeftest(TF.fit) # statistical significance of estimated coefficients
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y.tr,x.tr,TF.fit,lag.max = 100)
#NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.

# Check numerator coefficients of explanatory variable
TF.Identification.plot(x.tr,TF.fit)


#### Fit arima noise with selected
xlag = Lag(x.tr,0)   # b
xlag[is.na(xlag)]=0
arima.fit <- arima(y.tr,
                   order=c(0,0,1),
                   seasonal = list(order=c(1,1,0), period=24),
                   xtransf = xlag,
                   transfer = list(c(0, 0)), #List with (r,s) orders
                   include.mean = FALSE,
                   method="ML")
summary(arima.fit) # summary of training errors and estimated coefficients
coeftest(arima.fit) # statistical significance of estimated coefficients
# Check residuals
CheckResiduals.ICAI(arima.fit, lag=50)


### Cross correlation residuals - expl. variable
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x.tr)
########

# Check fitted
autoplot(y.tr, series = "Real", size = 2, alpha = 0.75, color="red")+
  forecast::autolayer(fitted(arima.fit), series = "Fitted", size = 0.5, color = "blue")

####################################################################################################
# Forecasting with TF models
####################################################################################################

ind_TS <- 951:length(y)

# Same scaling as before
#y.ts <- ts(y[ind_TS])
#x.ts <- ts(x[ind_TS,]/100)
#x.ts <- ts(x[ind_TS])
autoplot(cbind(y.ts,x.ts), facets = TRUE)

y.TS.est <- rep(NA, length(y.ts))

for (i in seq(length(y.tr)+1, length(fdata$Y), 1)){		# loop for validation period
  y.TS.est[i] <- TF.forecast(y.old = subset(y,end=i-1), 	# past values of the series
                             x.old = subset(x,end=i-1), 	# past values of the explanatory variables
                             x.new = subset(x,start = i,end=i), # new values of the explanatory variables
                             model = arima.fit, 		# fitted transfer function model
                             h=1) 				#Forecast horizon
}

y.TS.est <- na.omit(y.TS.est)
autoplot(ts(y.ts), series = "Real", size= 3, color="blue", alpha=0.5)+
  forecast::autolayer(ts(y.TS.est), series = "Fitted", color="yellow")


