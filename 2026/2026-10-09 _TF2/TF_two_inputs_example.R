#################################################################################
##############        Forecasting:     FT         ###############################
##############     ----------- solution ---------    ############################
#################################################################################


library(MLTools)
library(fpp2)
library(ggplot2)
library(TSA)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(Hmisc) # for computing lagged variables

## Set working directory ---------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# # Code to import data from Python statsmodels via reticulate
# library(reticulate)
# library(tidyverse)
# use_condaenv("fc", required = TRUE)
# 
# # Import the statsmodels module
# sm <- import("statsmodels.api")
# # Load the dataset into a pandas DataFrame
# macro_data <- py_to_r(sm$datasets$macrodata$load_pandas()$data)
# macro_data <- macro_data %>% 
#   select(year, quarter, realgdp, realcons, unemp)
# 
# write.csv(macro_data, file = "macro_data.csv", row.names = FALSE)



## Load dataset -------------------------------------------------------------------------------------------------------
fdata <- read.csv("macro_data.csv")

# View first few rows
head(fdata)

# Convert to time series object
freq <- 4
fdata_ts <- ts(fdata[,-(1:2)], frequency = freq)

head(fdata_ts)

autoplot(fdata_ts, facets = TRUE)

ggAcf(fdata_ts[,1], lag= 15 * freq)


# Scales
scale_y <- 1000
scale_x <- c(1000,1)

# Create time series and scale values 
y <- fdata_ts[ ,1]/scale_y
x1 <- fdata_ts[ , 2]/scale_x[1]
x2 <- fdata_ts[ , 3]/scale_x[2]


x <- cbind(x1, x2)
fdata_ts <- cbind(y, x1, x2)
head(fdata_ts)

autoplot(fdata_ts, facets = TRUE)

y_orig <- fdata_ts[,1]

# Length of training and validation subsets
len_TV <- 12
len_TR <- nrow(fdata_ts) - len_TV

# Use it to create train and test subsets
ind <- seq(1:nrow(fdata))
ind_TR <- ind <= len_TR
ind_TV <- !ind_TR

# Training subset
y.tr <- ts(y[ind_TR], frequency = freq)
x.tr <- ts(x[ind_TR,], frequency = freq)

autoplot(cbind(y.tr,x.tr), facets = TRUE)

# Test subset

y.ts <- ts(y[ind_TV], frequency = freq) 
# ¡Cuidado, no hemos usado start!
head(y.ts)
y.ts <- ts(y[ind_TV], frequency = freq,
           start = end(y.tr) + c(0,1)) 
head(y.ts)

# sometimes you don't have the test output
# but we assume here that you have the test inputs
x.ts <- ts(x[ind_TV,], frequency = freq,
           start = end(x.tr) + c(0,1))
head(x.ts)

autoplot(tail(y.tr, 50), series = "Training", color="blue") + 
  forecast::autolayer(y.ts, series = "Test", color = "red") + 
  geom_vline(xintercept = time(tail(y.tr, 10)), linetype="dashed", size=0.5)

# Model identification 

ggtsdisplay(y.tr, lag=50)


# BoxCox.lambda.plot(y.tr, window = freq)
BoxCox.lambda.plot(y.tr)

## Identification and fitting process -------------------------------------------------------------------------------------------------------
#(The different steps followed are represented for teaching purposes)


## FIRST --------------------------------------------
#### Fit initial FT model with large s
#This arima function belongs to the TSA package
TF.fit <- arima(y.tr,
                xtransf = x.tr,
                order=c(1,0,0),
                # seasonal = list(order=c(1,0,0), period=freq),
                transfer = list(c(0,9),c(0,9)),
                method="ML")

TF.RegressionError.plot(y.tr,x.tr,TF.fit,lag.max = 50)
#Regular differentiation is needed and coefficients of explanatory variables cannot be interpreted

## SECOND --------------------------------------------
#### Fit initial FT model with large s including regular differentiation
TF.fit <- arima(y.tr,
                xtransf = x.tr,
                order=c(1,1,0),
                # seasonal = list(order=c(1,0,0),period=freq),
                transfer = list(c(0,9),c(0,9)),
                method="ML")
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) #statistical significance of estimated coefficients
#Check regression error to see the need of differentiation
TF.RegressionError.plot(y.tr,x.tr,TF.fit,lag.max = 50)
#Regression error is stationary, then, we can continue
#NOTE: If this regression error was not stationary in variance,boxcox should be applied to input and output series.

#Check numerator coefficients of explanatory variable
TF.Identification.plot(x.tr,TF.fit)


## Transfer function parameters for more than one input

# Choose the values below using the identification plots
#x1:   
b1=0; r1=1; s1=0;
#x2:   
b2=0; r2=0; s2=0;


# Next apply the lags to the inputs
xlag <- x.tr
xlag[,1] <- Lag(x.tr[,1],b1) # b1
xlag[,2] <- Lag(x.tr[,2],b2) # b2

xlag[is.na(xlag)]=0

# and fit the arima model with those parameters
# and the ARMA structure (p, d, q)(P, D, Q)_s

arima.fit <- arima(y.tr,
                   xtransf = xlag,
                   order=c(0, 1, 1),
                   # seasonal = list(order=c(0,0,0),period=freq),
                   transfer = list(c(r1,s1), c(r2,s2)),
                   method="ML")


summary(arima.fit) #summary of training errors and estimated coefficients
coeftest(arima.fit) #statistical significance of estimated coefficients
#Check residuals
CheckResiduals.ICAI(arima.fit, lag=50)
#All coefficients significant --> Good.
#All correlations are small   --> Good.


#Check Cross correlation residuals - expl. variables
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x.tr[,1])
ccf(y = res, x = x.tr[,2])

########
#All expl.variables seem not correlated with residuals -> Good 


## Finished model identification

#Check fitted
autoplot(y.tr, series = "Real")+
  forecast::autolayer(fitted(arima.fit), series = "Fitted")

#Chech training errors of the model
accuracy(fitted(arima.fit),y.tr)


# Here for h=12 we forecast all the 12 values  at once
y_est <- TF.forecast(y.old = y.tr, #past values of the series
                     x.old = x.tr, #Past values of the explanatory variables
                     x.new = x.ts, #New values of the explanatory variables
                     model = arima.fit, #fitted transfer function model
                     h=12) #Forecast horizon


y_est

y_est <- ts(y_est, frequency = freq,
                start = end(y.tr) + c(0,1))


# Plot the forecast and real values

autoplot(y.ts, series = "Test set (real values)") +
  forecast::autolayer(y_est , series = "Forecast h=1 for the test") +
  forecast::autolayer( tail(y.tr, 24), series = "Training data" )

# Accuracy in test
accuracy(y_est * scale_y, y.ts * scale_y)

# Saved values
write.table(y_est * scale_y, file = "Pred_TF_2inputs.dat", 
            col.names = FALSE, row.names = FALSE)
getwd()
