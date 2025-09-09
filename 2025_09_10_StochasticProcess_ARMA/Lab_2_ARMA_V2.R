#####################################################################
##########       Lab Practice 2:      ARMA models         ###########
#####################################################################

library(MLTools)
library(fpp2)
library(tidyverse)
library(readxl)
library(lmtest) #contains coeftest function

## Set working directory ---------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#
# White Noise
#

# We will create a gaussian white noise time series. In order to do that we 
# get a sample of n random values from a standard normal. 

n <- 150
z <- rnorm(n, mean = 0, sd = 1) 

# 0 and 1 are the default values for rnorm so this is equivalent to rnorm(n)

head(z, 30)

# Now we use this to define a ts  object. Note that now we are
# **not** providing the frequency, start, etc. In this case, the ts function will 
# create a time index using the natural numbers t = 1, 2, 3, ...

w <- ts(z)
head(w, 25)

# time plot of the white noise time series:

autoplot(w) +
  ggtitle("White noise") + 
  geom_point(aes(x = 1:n, y = z), size=1.5, col="blue")


# ACF and PACF of white noise
ggtsdisplay(w, lag.max = 20)

# 
# Random walks
#
# A random walk is an stochastic process usually defined by the recursive equation:
# y_t = k + y_{t-1} + w_t
# where w_t is white noise. The value k is the drift constant.
# If we set y_0 = 0 an equivalent definition of random walk is:
# y_t = k\cdot t + w_1 + w_2 + ... + w_t
# 
# Let us simulate n values of a random walk with drift:

n = 1000
set.seed(2024)

# Let k be the drift constant
k = 0.1

# Create the white noise time series values:

w = 10 * rnorm(n)

# and then

rw_ts = ts(k * (1:n)  + cumsum(w))

# The time plot, it does not look like noise any more!

autoplot(rw_ts) +
  ggtitle("Random walk with drift") 

# ACF and PACF of random walk
ggtsdisplay(rw_ts, lag.max = 50)

# The presence of the linear trend translates into a slow decay in the ACF

# For a seasonal series the ACF function also displays patterns related to the seasonal period:

autoplot(AirPassengers)
sp = 12

# ACF and PACF
ggtsdisplay(AirPassengers)

# To examine the ACF values we can use:
Acf(AirPassengers,lag.max = 2 * sp, plot = FALSE)

# And to visualize the correlation between lagged versions of the time series:

gglagplot(AirPassengers, seasonal = FALSE, do.lines = FALSE, colour = FALSE)

# We can also examine a time plot of the series and its lagged version to understand what is happening:

k <- 7
lagged <- stats::lag(AirPassengers, k)
AirPassengers_lag <- cbind(Original = AirPassengers, lagged = lagged)
head(AirPassengers_lag, k + 2)

ts.plot(AirPassengers_lag,
        lty = 1:2,
        main = "AirPassengers and lagged Series", 
        ylab = "Passengers", xlab = "Time")
legend("topleft", legend = c("Original", "lagged"), 
       lty = 1:2)

#
# ARMA processes
#

## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read_excel("ARMA_series.xls")
#fdata <- read_excel("ARMA_Hackaton_data.xls")

# Convert to time series object
fdata_ts <- ts(fdata)

head(fdata_ts)

# index to select a time series
y <- fdata_ts[ ,2]


## Identification and fitting frocess -------------------------------------------------------------------------------------------------------

# ACF and PACF of the time series -> identify significant lags and order
ggtsdisplay(y, lag.max = 20)

# Fit model with estimated order
arima.fit <- Arima(y, order=c(1,0,1), include.mean = TRUE)

summary(arima.fit) # summary of training errors and estimated coefficients

coeftest(arima.fit) # statistical significance of estimated coefficients

autoplot(arima.fit) # root plot

# Check residuals
CheckResiduals.ICAI(arima.fit, bins = 100, lags = 20)

# If residuals are not white noise, change order of ARMA
ggtsdisplay(residuals(arima.fit), lag.max = 20)

#######

# Check fitted forecast
autoplot(y, series = "Real") +
  forecast::autolayer(arima.fit$fitted, series = "Fitted")

#Perform future forecast
y_est <- forecast::forecast(arima.fit, h=5)
autoplot(y_est)


## Simulate ARMA time series -------------------------------------------------------------------------------------------------------
sim_ts <- arima.sim(n = 250, 
                 list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
                 sd = sqrt(0.1796))

