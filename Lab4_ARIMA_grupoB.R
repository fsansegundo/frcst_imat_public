#' ---
#' title: "Lab. 4 ARIMA models"
#' author: "Análisis y Predicción de Series Temporales, 3º B IMAT"
#' date: "2024-09-20"
#' bibliography: "../../forecasting.bib"
#' output: 
#'  html_document:
#'    toc: true
#'    toc_depth: 2
#'    toc_float:
#'      toc_collapsed: true
#'    toc-location: left
#'  # word_document: default
#' 
#' ---


#' # Preliminaries
#' 
#' ### Load libraries
#' 
#' If you get an error go to the packages panel, click install and 
#' type the name of the library (you only need to do this once).

#+ message = FALSE

library(MLTools)
library(fpp2)
library(readxl)
library(tidyverse)
library(lmtest)  # contains coeftest function
library(tseries) # contains adf.test function
library(TSstudio)

#' ### Set the working directory 

#+ eval = FALSE

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#' # Reading time series data and EDA

#' ### Load dataset 

fdata <- read_excel("ARIMA_example.xlsx")

#' Visualize the first rows:

head(fdata)

#' 
#' ### Convert it to a time series object   
#' 
#' Assume that there are no time gaps, and use the natural numbers as index. 
fdata_ts <- ts(fdata)
head(fdata_ts)

#' and we get some basic information
#' 

ts_info(fdata_ts)

#' We use the column index to select a time series
y <- fdata_ts[ , 1]

#' # Stationarity and Model Identification

#' ### ACF and PACF plots of the time series 
#' 
#' We use the following plots to assess the stationarity of the time series.
#' With ggtsdisplay we get a time plot and both the ACF and PACF.

#+ fig.width=12, fig.height=8
ggtsdisplay(y)


#' ### Dealing with non homogeneous variance (heteroskedasticity)
#' 
#' We are going to decide if we want to apply a Box-Cox transformation

#+ fig.width=12, fig.height=6
Lambda <- BoxCox.lambda.plot(y, window.width = 10)

# Lambda <- BoxCox.lambda(y) #other option

#' z is the transformed time series if Box-Cox is applied.   
#' **Note:** If you decide that you do not want to use Box-Cox, set 
#' `useLambda <- FALSE`
#' in the next code line:
#' 

useLambda <- TRUE

if(useLambda) {
  z <- BoxCox(y, Lambda)
} else {
  z <- y
  Lambda <- 1
}


#' We can plot together the original and transformed time series to see 
#' the result of the transformation.

#+ fig.width=12, fig.height=8
#+ 
autoplot(y, linewidth=0.25, color = "red") + 
  autolayer(z, color = "blue", alpha=0.5)


#' ### Deciding if differencing is needed
#' 

#' When the ACF decreases very slowly the time series needs differentiation. We 
#' analyze the ACF of the (possibly Box-Cox transformed) time series.

#+ fig.width=12, fig.height=8
ggtsdisplay(z, lag.max = 25)

#' Two alternative ways of checking for stationarity: the first one is to use 
#' the augmented Dickey-Fuller (adf) test of the null: *data non-stationary and 
#' non seasonal* (see [[@hyndman2021fpp3], Unit Root Tests and the video 
#' therein](https://otexts.com/fpp3/stationarity.html#unit-root-tests))

adf.test(z, alternative = "stationary")

#' Check the size of the p-value. The fpp3 book also discusses the kpss test, 
#' that we can apply uncommenting the next code line.

# tseries::kpss.test(z, null = "Trend")

#' A second way: the `ndiffs` function uses the previous tests to suggest a 
#' differencing order that will make the time series stationary:
 
ndiffs(z)

#' Either way, now you need to decide the number `d` of differences to apply, 
#' if any:

d <- 1


#' If differencing is needed we apply it here:

Bz <- if(d > 0){
  diff(z, differences = 1) 
} else {
    z
  }

#' 
#' # ARIMA model selection and fit
#' 

#' We use the ACF and PACF of the Bz time series to identify the significant 
#' lags and propose the ARMA structure of the model:


#+ fig.width=12, fig.height=8
ggtsdisplay(Bz, lag.max = 25)


#' Now we propose a structure of the model by selecting the values of (p, d, q)
#' where d is for the number of differences (already decided).

p <- 2
q <- 0

#' This is the chosen (p, d, q) structure:

cat(paste0(c("p", "d", "q")," = ", c(p, d, q)))

#' We fit the model with the above values. Observe that we apply it to the 
#' original (untransformed, undifferenced) time series y, because the `Arima`
#' function includes arguments for both Lambda and d.
#' 
#' **Note:** also remember that if you want a constant term you need to 
#' include it here.

arima.fit <- Arima(y,
                   order = c(p, d, q), 
                   lambda = Lambda,
                   #lambda = 1,
                   include.constant = TRUE)

#' # Model Diagnosis

#' ### Summary of training errors and estimated coefficients

summary(arima.fit) 

#' ### Statistical significance of estimated coefficients
#' 
#' If some coefficient is non significant you should reconsider the 
#' choice of model order.

coeftest(arima.fit) 

#' The following root plot is used to assess the stationarity and invertibility
#' of the model.

#+ fig.width=4, fig.height=4
autoplot(arima.fit) 

#' #### Check residuals
#' 
#' Again, if the residuals are not white noise, you should reconsider the 
#' choice of the ARMA model order.

#+ fig.width=12, fig.height=8
CheckResiduals.ICAI(arima.fit, bins = 100, lag = 25)

#' **Check the p-value of the Ljung-Box test** above. The null hypothesis for this 
#' test can be rephrased as *the series is not white noise*.

#' 
#' # Fitted values and forecast
#' 
#' #### Check  the fitted values against the real ones

#+ fig.width=12, fig.height=6
autoplot(y, series = "Real", size=2, alpha=0.5) +
  forecast::autolayer(arima.fit$fitted, series = "Fitted")

#' ###  Perform a true (future) forecast 
#' 
#' We need to select the prediction horizon h 

y_est <- forecast(arima.fit, h=12)

#' The result is a `forecast` type object. It contains not only the forecasted
#' values, but also uncertainty intervals and extra information about the fit. 
#' Printing the forecast object shows some of this information:

y_est

#' The forecast object can also be plotted. 
#' 
#' 
 
#+ fig.width=12, fig.height=6
autoplot(y_est)

#' In the following plot we subset the original time series to improve the
#' visualization of the h forecasted values.

#+ fig.width=12, fig.height=6
autoplot(subset(y, start = 900)) +
  autolayer(y_est)


#' 
#' # Additional Code
#' 

#' 
#' ## Arma model selection with train/test split and performance measures
#'
#' Let us go over the same dataset but this time we will use functions from other 
#' libraries to perform a basic train/test split and get some performance measures.


n <- length(y)
n_test <- floor(0.2 * n)


library(TSstudio)
y_split <- ts_split(y, sample.out = n_test)

y_TS <- y_split$test
y_TR <- y_split$train

#' Now we will briefly go over the same model identification and fitting steps, 
#' but using the training values.

#' ### Box-Cox

#+ fig.width=12, fig.height=8
Lambda <- BoxCox.lambda.plot(y_TR, window.width = 10)

useLambda <- TRUE

if(useLambda) {
  z <- BoxCox(y_TR, Lambda)
} else {
  z <- y_TR
  Lambda <- 1
}

#' ### Deciding if differencing is needed
#' 

#' When the ACF decreases very slowly the time series needs differentiation. We 
#' analyze the ACF of the possibly Box-Cox transformed time series.

#+ fig.width=12, fig.height=8
ggtsdisplay(z, lag.max = 25)

#' ADF test for stationarity
adf.test(z, alternative = "stationary")

#' Also check ndiffs
ndiffs(z)

#' Choose value of d
d <- 1

#' If differencing is needed we apply it here:

Bz <- if(d > 0){
  diff(z, differences = 1) 
} else {
  z
}

#' 
#' ### ARIMA model selection and fit
#' 

#' ACF and PACF of Bz

#+ fig.width=12, fig.height=8
ggtsdisplay(Bz, lag.max = 25)


#' Now we propose a structure  (p, d, q)  
#' d selected above

p <- 2
q <- 1

#' Fit the model 

arima.fit <- Arima(y_TR,
                   order=c(p, d, q), 
                   lambda = Lambda,
                   include.constant = TRUE)

#' ### Model Diagnosis

#' Summary of training errors and estimated coefficients

summary(arima.fit) 

#' Statistical significance of estimated coefficients

coeftest(arima.fit) 

#' Root plot

#+ fig.width=4, fig.height=4
autoplot(arima.fit) 

#' Check residuals

#+ fig.width=12, fig.height=6
CheckResiduals.ICAI(arima.fit, bins = 100, lag = 25)

#' ### Fitted values and forecast

#+ fig.width=12, fig.height=6
autoplot(y_TR, series = "Training", color = "blue", size=2, alpha=0.7) +
  autolayer(arima.fit$fitted, series = "Fitted") 
  

# Perform a future forecast 
y_frcst <- forecast(arima.fit, h=n_test)

#' Plot of the final part of the training set and the forecast

#+ fig.width=12, fig.height=6
autoplot(subset(y_TR, start = 550), series = "Training", color = "blue", size=2, alpha=0.7) +
  autolayer(subset(arima.fit$fitted, start = 550), series = "Fitted")  + 
  autolayer(y_frcst, series ="Forecast") + 
  autolayer(y_TS, series = "Test")


#' ## Using tslm to create a baseline model
#' 
#' Quoting Hyndman's fpp3:  
#' *The basic concept is that we forecast the time series of interest y
#' assuming that it has a linear relationship with other time series x*  
#' The time series x may even be one of the components of y, e.g. the trend. 
#' We will explore this idea below.
#'  
#' 

#' We fit a linear model with the `tslm`  function. You can read about `tslm` in 
#' [Chapter 5 of FPP2 (Hyndman's book, 2nd ed.)](https://otexts.com/fpp2/regression.html#regression). 
#' In the third edition it has been replaced with [TSLM](https://otexts.com/fpp3/regression.html#regression).
#' 
#' In this case we use the `trend` keyword to indicate that we use the trend as 
#' input variable. 

tslm_model <- tslm(y_TR ~ trend)

#' Check the significance of the coefficients
summary(tslm_model)

#' We can now use this linear model to do a forecast
y_tslm <- forecast(tslm_model, h=n_test)

# And explore the forecast graphically

#+ fig.width=12, fig.height=6
autoplot(subset(y_TR, start = 550), series = "Training", color = "blue") +
  autolayer(y_tslm, series ="TSLM") + 
  autolayer(y_frcst, series ="Forecast", alpha = 0.25) + 
  autolayer(y_TS, series = "Test")

#' Zooming in the beginning of the forecast

#+ fig.width=12, fig.height=6
autoplot(subset(y_TR, start = 790), series = "Training", color = "blue") +
autolayer(window(y_frcst$mean, end=820), series="Arima") + 
  autolayer(window(y_tslm$mean, end=820), series="tslm") +
  ylab(" ")


#' ## Model Comparison

#' Now we have trained two models, and we can compare them using their performance 
#' on the test set. 

forecast::accuracy(y_frcst, y_TS)
forecast::accuracy(y_tslm, y_TS)

#' Some explicit formulas:
# mean(abs(residuals(tslm_model)))
# sqrt(mean(residuals(tslm_model)^2))

#' ## Why Arima?
#' 
#' 
#' As you can see the error of the linear model is worse than that of the 
#' Arima model. A exploration of the residuals of the linear model shows that 
#' they are clearly not white noise.

ggtsdisplay(residuals(tslm_model))

#' That means that the linear model is not able to extract as much information 
#' from the time series as Arima does. 


#' 
#' ## References
#' 
