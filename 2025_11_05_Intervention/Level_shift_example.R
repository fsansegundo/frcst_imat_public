library(forecast)
library(tidyverse)
library(MLTools)
library(fpp2)
library(TSA)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(Hmisc) # for computing lagged variables
library(lubridate)
library(readxl)



######################################################################################################
######################################################################################################
#  Level Shift (LS) Example
######################################################################################################
######################################################################################################


set.seed(123)
n <- 200

# Simulate a clean ARIMA(1,0,1) process
phi <- 0.6
theta <- 0.3
e <- rnorm(n)
y_clean <- numeric(n)
y_clean[1] <- e[1]
for (t in 2:n) {
  y_clean[t] <- phi*y_clean[t-1] + e[t] + theta*e[t-1]
}

# Introduce a level shift at time T = 100
T <- 100
omega <- 10  # shift magnitude
yLS <- y_clean
yLS[T:n] <- yLS[T:n] + omega  # permanent shift from T onward

yLS <- ts(yLS)
y_clean <- ts(y_clean)

autoplot(yLS, series = "Series with Level Shift") +
  autolayer(y_clean, series = "Clean Series", alpha = 0.5) +
  ggtitle("Time Series with Level Shift at t=100")


# Fit a baseline ARIMA model (ignoring the level shift)

ggtsdisplay(yLS, main="Time Series with LS")
ggtsdisplay(diff(yLS), main="Time Series with LS")
yLS_arima <- Arima(yLS, order = c(4, 1, 0), include.mean = FALSE)

coeftest(yLS_arima)
CheckResiduals.ICAI(yLS_arima)

autoplot(residuals(yLS_arima)) + ggtitle("Residuals of LS ARIMA Model")
boxplot(residuals(yLS_arima), main = "Residuals of Baseline ARIMA Model")
which(abs(yLS_arima$residuals) > (3 * sd(yLS_arima$residuals)))


# tsouliers function in forecast package
forecast::tsoutliers(yLS)

yLS_adj <- tsclean(yLS)
autoplot(yLS_adj, series = "LS adjusted time series") +
  autolayer(yLS, series = "Non adjusted time series", alpha = 0.5) +
  ggtitle("Adjusted series with forecast tsoutliers function (non effective for LS)")

# Use tsoutliers library (different from the above) 
# to detect outliers including level shifts

library(tsoutliers)
out_ls <- tsoutliers::tso(yLS, tsmethod="auto.arima")
print(out_ls)
plot(out_ls)  # gives diagnostics and shows detected LS
y_tso <- out_ls$yadj
autoplot(y_tso, series = "tsoutliers adjusted series") +
  autolayer(yLS, series = "Non adjusted time series", alpha = 0.5) +
  ggtitle("Adjusted series after LS Removal with tsoutliers package")


# Transfer function model with step function

# Create a regressors vector for the level shift
T <- 100
xLS <- rep(0, n)
xLS[T:n] <- 1
xLS <- ts(xLS)
autoplot(xLS) + ggtitle("Level Shift Regressor")


TF_LS <- arima(yLS,
                order=c(1, 0, 0),
                xtransf = xLS,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")


# Check regression error to see the need of differentiation
TF.RegressionError.plot(yLS, xLS, TF_LS, lag.max = 20)

TF_LS <- arima(yLS,
                order=c(1, 1, 0),
                xtransf = xLS,
                transfer = list(c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")


# Check regression error to see the need of differentiation
TF.RegressionError.plot(yLS, xLS, TF_LS, lag.max = 20)
#NOTE: If this regression error is not stationary in variance,boxcox should be applied to input and output series.


# Check numerator coefficients of explanatory variable
TF.Identification.plot(xLS, TF_LS)

#### Fit arima noise with selected
xlag = Lag(xLS, 0)   # b
xlag[is.na(xlag)]=0
TF_LS <- arima(yLS,
                order=c(4,1,0),
                xtransf = xLS,
                transfer = list(c(0, 0)), #List with (r,s) orders
                include.mean = FALSE,
                method="ML")
summary(TF_LS) # summary of training errors and estimated coefficients
coeftest(TF_LS) # statistical significance of estimated coefficients
autoplot(TF_LS)
# Check residuals
CheckResiduals.ICAI(TF_LS, lag=50)

### Cross correlation residuals - expl. variable
res <- residuals(TF_LS)
res[is.na(res)] <- 0
ccf(y = res, x = xLS)

autoplot(yLS, series = "Observed") +
  autolayer(fitted(TF_LS), series = "LS intervention") +
  autolayer(yLS_arima$fitted, series = "Arima") + 
  theme(legend.position = "top")




autoplot(yLS, series = "Observed") +
  autolayer(y_tso, series = "tsoutliers package adjustment") + 
  theme(legend.position = "top")


autoplot(yLS, series = "Observed") +
  autolayer(fitted(TF_LS)) + 
  theme(legend.position = "top")

coefficients(TF_LS) # Transfer function
coefficients(yLS_arima) # Arima for unadjusted time series  
  

