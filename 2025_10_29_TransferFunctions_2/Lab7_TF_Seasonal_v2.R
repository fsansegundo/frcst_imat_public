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
fdata_ts <- ts(fdata, frequency = 24)
autoplot(fdata_ts, facets = TRUE)
# Create time series and scale values 
y <- fdata_ts[,1]/100000
x <- fdata_ts[,2]/100000

ggtsdisplay(y, lag=50)

y.TR <- subset(y, end = length(y) - 96) 
y.TV <- subset(y, start = length(y) - 95)

x.TR <- subset(x, end = length(y) - 96)
x.TV <- subset(x, start = length(y) - 95)


## Identification and fitting process -------------------------------------------------------------------------------------------------------

#### Fit initial FT model with large s
# This arima function belongs to the TSA package
TF.fit <- arima(y.TR,
                order=c(1, 0, 0),
                seasonal = list(order=c(1, 0, 0), period=24),
                xtransf = x.TR,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)
#NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.

# Seasonal differencing required
TF.fit <- arima(y.TR,
                order=c(1, 0, 0),
                seasonal = list(order=c(1, 1, 0), period=24),
                xtransf = x.TR,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
# Check regression error to see the need of differentiation
TF.RegressionError.plot(y,x,TF.fit,lag.max = 100)
#NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.

# Check numerator coefficients of explanatory variable
TF.Identification.plot(x,TF.fit) # b =0, r =0, s=0


#### Fit arima noise with selected
xlag = Lag(x,0)   # b
xlag[is.na(xlag)]=0
arima.fit <- arima(y,
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
ccf(y = res, x = x)
########

# Check fitted
autoplot(y, series = "Real")+
  forecast::autolayer(fitted(arima.fit), series = "Fitted")


#Check training errors of the model
accuracy(fitted(arima.fit),y.TR)

#################################################################################
## Forecast for new data with h = 1 
#################################################################################
autoplot(cbind(y.TV,x.TV), facets = TRUE)

h <- 2

y.TV.est <- y * NA
for (i in seq(length(y.TR) + 1, length(y) - h, 1)){# loop for validation period
  y.TV.est[i] <- TF.forecast(y.old = subset(y,end=i-1), #past values of the series
                             x.old = subset(x,end=i-1), #Past values of the explanatory variables
                             x.new = subset(x,start = i,end=i), #New values of the explanatory variables
                             model = arima.fit, #fitted transfer function model
                             h=h)[h] #Forecast horizon
}

y.TV.est <- na.omit(y.TV.est)
autoplot(ts(y.TV), series = "Real", size=2, alpha=0.5, color = "blue")+
  forecast::autolayer(ts(y.TV.est), series = "Fitted", color="red")

accuracy(y.TV.est*100000,y.TV*100000)



