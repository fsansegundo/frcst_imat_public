#' ---
#' title: "Lab. 8 Dynamic regression models, forecasting example"
#' author: "Análisis y Predicción de Series Temporales, 3º B IMAT"
#' date: "2024-11"
#' output: 
#'  html_document:
#'    toc: true
#'    toc_depth: 4
#'    toc_float:
#'      toc_collapsed: true
#'    toc-location: left
#' 
#' ---
#' 
#' 

#' 
#' # Preliminaries
#' 
#' ### Load libraries
#' 

#+ echo = FALSE, message = FALSE

if(require("klippy"))klippy(position = "r")

#+ message = FALSE


library(MLTools)
library(fpp2)
library(ggplot2)
library(tidyverse)
library(TSA)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(Hmisc) # for computing lagged variables

#' **Set working directory**

try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))


#' **Load datasets**
#' 
#' We have received the training and test data in separate files, wherein the test data is 
#' limited to the exogenous variables (x variables).
#' 
#' The training data is read as follows:
fdata_TR <- read_csv("https://gist.githubusercontent.com/fsansegundo/4bdccfde57696118164f88a55c7d85ef/raw/4a9cf86f907a61fa47631094a7e84c7ca322d098/macro_econ_TR.csv")
head(fdata_TR)
tail(fdata_TR)

#' and similarly for the test data
fdata_TS <- read_csv("https://gist.githubusercontent.com/fsansegundo/4bdccfde57696118164f88a55c7d85ef/raw/4a9cf86f907a61fa47631094a7e84c7ca322d098/macro_econ_TS.csv")
head(fdata_TS)

#' After inspecting the data to examine the date information we remove the 
#' corresponding columns from both datasets:
#' 
#' The training data
fdata_TR <- fdata_TR[,-(1:2)]
head(fdata_TR)

#' and the test data
fdata_TS <- fdata_TS[,-(1:2)]

#' And we use that information to convert the training data to time series objects, 
#' starting with the frequency
freq <- 4

#' Make sure that the start value is correct here
fdata_ts <- ts(fdata_TR, start = c(1959, 1), frequency = freq)

#' And let us plot the training time series (input and output)
autoplot(fdata_ts, facets = TRUE)


#' If scaling is needed we apply it here
fdata_ts[,1] <- fdata_ts[,1] / 1000
fdata_ts[,2] <- fdata_ts[,2] / 1000
fdata_ts[,3] <- fdata_ts[,3] / 1000

#' Here we need to identify the column corresponding to the output variable y and 
#' the columns(s) for the output variable(s) x

x.tr <- fdata_ts[,2:3]
y.tr <- fdata_ts[,1]

#' Let us examine graphically all these time series
autoplot(fdata_ts, facets = TRUE)

#' In the above plot we look for trend and seasonality. To further explore this we
#' can examine the ACF of the y series
ggtsdisplay(y.tr)

#' In this example there does not appear to be seasonality (in fact, the original 
#' data is seasonally adjusted, [see here](https://www.statsmodels.org/dev/datasets/generated/macrodata.html)) 
#' but the trend is obvious, so we apply a regular diference
ggtsdisplay(diff(y.tr, lag = 1, differences = 1), lag.max = 50)

#' If you examine the above plot yoou will notice that the regularly differenced 
#' time series still seems to have a moderate upward trend. So we decided to apply 
#' a second regular difference. 
ggtsdisplay(diff(y.tr, lag = 1, differences = 2), lag.max = 50)
#' So we set the regular difference order d (the seasonal difference order D
#' is commented as it is not needed in this example):

d <-  2
# D <- 0

#' Now the series seems stationary and we proceed to identify and fit a TF model.
#' 
#' # Identification and fitting process 
#' 
#' #### Fit initial FT model with large s
#' 
#' Note that we use `c(0,9), c(0,9)` in the transfer argument below because we 
#' have two input variables. We do not need the seasonal part of the 
#' model, but we need to apply the differences we have selected before.

TF.fit <- arima(y.tr,
                order=c(1, d, 0),
                #seasonal = list(order=c(1,D,0), period=freq),
                xtransf = x.tr,
                transfer = list(c(0,9), c(0,9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")
summary(TF.fit) 

#' ### Check the statistical significance of estimated coefficients
coeftest(TF.fit) 

#' ### Check regression error 
#' 
#' The plot below is used to decide about the order of the model for the ARIMA
#' noise
TF.RegressionError.plot(y.tr, x.tr, TF.fit,lag.max = 20)

#' In this particular example we propose this structure (the seasonal order
#' is commented as it is not needed in this example):
#' 
p <- 0
q <- 1
# P <- 0
# Q <- 0
#' 
#' ### Transfer function identification
#' 
#' And this plot (or plots) is used to decide about the values of 
#' (b, r, s) in the transfer function(s) for the input variable(s)
#' 
TF.Identification.plot(x.tr,TF.fit)

#' For this example we propose

# First input variable
b1 <- 0
r1 <- 0
s1 <- 0
# Second input variable
b2 <- 0
r2 <- 0
s2 <- 0


#' 
#' # Fit transfer function model with the selected parameters
#' 
#' ### Lagged input variables
#' 
#' To apply possibly different lags to each input variable we make a copy of the 
#' input variables time series and apply the lags sequentially to each column

xlag <- x.tr

xlag[ ,1]  <-  Lag(xlag[ ,1], b1)
xlag[ ,2]  <-  Lag(xlag[ ,2], b2)

#' Next we fill the missing lagged data with 0s
xlag[is.na(xlag)] <-  0
head(xlag)

#' ### Fit of the model

arima.fit <- arima(y.tr,
                   order=c(p, d, q),
                   #seasonal = list(order=c(P, D, Q),period=freq),
                   xtransf = xlag,
                   transfer = list(c(r1, s1), c(r2, s2)), #List with (r,s) orders
                   include.mean = FALSE,
                   method="ML")

summary(arima.fit) 

#' ### Check the statistical significance of estimated coefficients

coeftest(arima.fit) 

#' In this example they are all significant

#' ### Check the residuals

CheckResiduals.ICAI(arima.fit, lag=25)

#' And the residuals look like gaussian white noise, as confirmed by the 
#' Ljung-Box test p-value.

#' ### Cross correlation between residuals and exogenous variable(s)


res <- residuals(arima.fit)
res[is.na(res)] <- 0

#' In examples with more than one exogenous variable we need to examine the 
#' cross correlation for each of them

ccf(y = res, x = x.tr[,1])
abline(v = -5:5, col="orange", lty = 2)

ccf(y = res, x = x.tr[,2])
abline(v = -5:5, col="orange", lty = 2)

#' And in this example we find no significant cross correlation values. 


#' ### Graphical check of the fitted values 

autoplot(y.tr, series = "Real", color="black", size = 1)+
  forecast::autolayer(fitted(arima.fit), series = "Fitted", 
                      size = 4, color = "tan", alpha=0.5)

#' ### Accuracy in training
accuracy(y.tr, fitted(arima.fit))

#' 
#' # Forecasting with TF models
#' 
#' In order to perform the forecasting loop (with horizon h = 1) we will create 
#' a time series that is a copy of the training output values y.TR but adding 
#' extra positions at the end to store the predictions as we generate them 
#' (initially they are set to NA)

#' It is created as a vector
y.TS.est <- c(y.tr, rep(NA, nrow(fdata_TS))) 

#' But converted as a time series with the same time index as y.TR
y.TS.est <- ts(y.TS.est, start = start(y.tr), frequency = freq)

#' The resulting output data looks like this
head(y.TS.est, 3 * freq)
tail(y.TS.est, 3 * freq)

#' For the input variables we use rbind (row-bind) to join the training and 
#' test data into a single data frame 
x.TS.est <- rbind(fdata_TR[, 2:3], fdata_TS)

#' That now is endowed with time series structure
x.TS.est<- ts(x.TS.est, start = c(1959, 1), frequency = freq)

#' If **scaling** was applied in training it is **important** to reapply **the 
#' same** here.
x.TS.est[,1] <- x.TS.est[,1] / 1000
x.TS.est[,2] <- x.TS.est[,2] / 1000

#' The resulting input data looks like this
head(x.TS.est)
tail(x.TS.est)

#' Now we are ready for the **forecasting loop**. Note that the values are 
#' stored in the last positions of y.TS.est as they are generated, and that the
#' y.old and x.old get extended as the loop travels through the test set.
#' 
for (i in seq(length(y.tr) + 1, length(y.TS.est), 1)){		# loop for validation period
  y.TS.est[i] <- TF.forecast(y.old = subset(y.TS.est, end=i-1), 	# past values of the series
                             x.old = subset(x.TS.est, end=i-1), 	# past values of the explanatory variables
                             x.new = subset(x.TS.est, start = i, end=i), # new values of the explanatory variables
                             model = arima.fit, 		# fitted transfer function model
                             h=1) 				#Forecast horizon
}

#' The predicted values are in the last positions of y.TS.est (as many as test 
#' set data rows)
y_TS_pred <- tail(y.TS.est, nrow(fdata_TS))
y_TS_pred

#' We save them into a csv file (with no header for the hackaton)
prediction <- data.frame(y_TS_pred)
prediction
write_csv(prediction, "Lab8_Macroeconomy_Data_Prediction.csv", col_names = FALSE)


