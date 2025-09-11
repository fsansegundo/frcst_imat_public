#' ---
#' title: "Lab 10: Nonlinear forecasting:  TF and Feature Engineering"
#' author: "Análisis y Predicción de Series Temporales, 3º B IMAT"
#' date: "2024-11-08"
#' output: 
#'  html_document:
#'    toc: true
#'    toc_depth: 4
#'    toc_float:
#'      toc_collapsed: true
#'    toc-location: left
#'  # word_document: default
#' ---
#' 


#################################################################################
##############      Lab 10: Nonlinear forecasting     ############################
##############        TF + Feature Engineering      #############################
##############     ----------- solution ---------    ############################
#################################################################################
library(fpp2)
library(lmtest)
library(tseries) #contains adf.test function
library(TSA)
library(readxl)
library(MLTools)
library(imputeTS)

## Set working directory ---------------------------------------------------------------------------------------------
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))

# Lectura datos -------------------------------------------------------------------------------------------------
fdata <- read_excel("DAILY_DEMAND_TR_NA.xlsx")

#Convert to time series object
fdata_ts <- ts(fdata, frequency=7)
autoplot(fdata_ts, facets = TRUE)

# Imputation of missing values
ggplot_na_distribution(fdata_ts[,4])

imp_seas <- na_seasplit(fdata_ts)
ggplot_na_imputations(fdata_ts[,4], imp_seas[,4])

imp_kalman <- na_kalman(fdata_ts, model = "auto.arima")
ggplot_na_imputations(fdata_ts[,4], imp_kalman[,4])

fdata_ts <- imp_seas

#Create time series 
y <- fdata_ts[,2]
x <- fdata_ts[,c(3,4)]
autoplot(cbind(y,x), facets = TRUE)

#Scale
y.tr <- y/100
x.tr <- x/1
autoplot(cbind(y.tr,x.tr), facets = TRUE)


## Identification and fitting process -------------------------------------------------------------------------------------------------------
#(The different steps followed are represented for teaching purposes)

## FIRST --------------------------------------------
#### Fit initial FT model with large s
#This arima function belongs to the TSA package
TF.fit <- arima(y.tr,
                xtransf = x.tr,
                order=c(1,0,0),
                seasonal = list(order=c(1,0,0),period=7),
                transfer = list(c(0,9),c(0,9)),
                method="ML")
#Check regression error to see the need of differentiation
TF.RegressionError.plot(y.tr,x.tr,TF.fit,lag.max = 50)
#Regular differentiation is needed and coefficients of explanatory variables cannot be interpreted


## SECOND --------------------------------------------
#### Fit initial FT model with large s
TF.fit <- arima(y.tr,
                xtransf = x.tr,
                order=c(1,1,0),
                seasonal = list(order=c(1,0,0),period=7),
                transfer = list(c(0,9),c(0,9)),
                method="ML")
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) #statistical significance of estimated coefficients
#Check regression error to see the need of differentiation
TF.RegressionError.plot(y.tr,x.tr,TF.fit,lag.max = 50)
#Regression error is stationary, then, we can continue
#NOTE: If this regression error was not stationary in variance,boxcox should be applied to input and output series.
# Try AR(1)7 for seasonal component and ARMA(2,1) for regular component?
#Check numerator coefficients of explanatory variable
TF.Identification.plot(x.tr,TF.fit)
#WD:    b=0, r=0, s=0
#TEMP:  b=0, r=0, s=1

## THIRD --------------------------------------------
#### Fit model with selected values and estimate initial ARIMA structure
TF.fit <- arima(y.tr,
                xtransf = x.tr,
                order=c(2,1,1),
                seasonal = list(order=c(1,0,0),period=7),
                transfer = list(c(0,0),c(0,1)),
                method="ML")
#Check regression error to identify correlation structure
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) #statistical significance of estimated coefficients
#Check residuals
CheckResiduals.ICAI(TF.fit, lag=50)

PlotModelDiagnosis(x.tr, y.tr, fitted(TF.fit), together = TRUE)


###############################################################################
# Generate new temperature variables
###############################################################################
#Plot relation between temperature and demand
ggplot(fdata)+geom_point(aes(x=TEMP, y=DEM))

#We can see that there is a non-linear relation between them.
#Create new time series splitting the temperatures
fdata$tcold <- sapply(fdata$TEMP,min,17)
fdata$thot <- sapply(fdata$TEMP,max,22)

fdata_ts <- ts(fdata, frequency=7)
imp_seas <- na_seasplit(fdata_ts)
fdata_ts <- imp_seas


#Create time series 
y <- fdata_ts[,2]
x <- fdata_ts[,c(3,5,6)]
autoplot(cbind(y,x), facets = TRUE)

#Scale
y.tr <- y/100
x.tr <- x/1
autoplot(cbind(y.tr,x.tr), facets = TRUE)


## Identification and fitting process -------------------------------------------------------------------------------------------------------
#(The different steps followed are represented for teaching purposes)

## FIRST --------------------------------------------
#### Fit initial FT model with large s
#This arima function belongs to the TSA package
TF.fit <- arima(y.tr,
                xtransf = x.tr,
                order=c(1,0,0),
                seasonal = list(order=c(1,0,0),period=7),
                transfer = list(c(0,9),c(0,9),c(0,9)),
                method="ML")
#Check regression error to see the need of differentiation
TF.RegressionError.plot(y.tr,x.tr,TF.fit,lag.max = 50)
#Regular differentiation is needed and coefficients of explanatory variables cannot be interpreted

## SECOND --------------------------------------------
#### Fit initial FT model with large s
TF.fit <- arima(y.tr,
                xtransf = x.tr,
                order=c(1,1,0),
                seasonal = list(order=c(1,0,0),period=7),
                transfer = list(c(0,9),c(0,9),c(0,9)),
                method="ML")
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) #statistical significance of estimated coefficients
#Check regression error to see the need of differentiation
TF.RegressionError.plot(y.tr,x.tr,TF.fit,lag.max = 50)
#Regression error is stationary, then, we can continue
#NOTE: If this regression error was not stationary in variance,boxcox should be applied to input and output series.
# Try AR(1)7 for seasonal component and AR(2) for regular component?
#Check numerator coefficients of explanatory variable
TF.Identification.plot(x.tr,TF.fit)
#WD:    b=0, r=0, s=0
#tcold: b=0, r=1, s=0
#thot:  b=0, r=1, s=0


## THIRD --------------------------------------------
#### Fit model with selected values and estimate initial ARIMA structure
TF.fit <- arima(y.tr,
                xtransf = x.tr,
                order=c(2,1,0),
                seasonal = list(order=c(1,0,0),period=7),
                transfer = list(c(0,0),c(1,0),c(1,0)),
                method="ML")
#Check regression error to identify correlation structure
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) #statistical significance of estimated coefficients
#Check residuals
CheckResiduals.ICAI(TF.fit, lag=50)


## FOURTH --------------------------------------------
#### lets try MA/(2) instead of AR(2)
#### start with (0,1,2)(1,0,0)7
TF.fit <- arima(y.tr,
                   xtransf = x.tr,
                   order=c(0,1,2),
                   seasonal = list(order=c(1,0,0),period=7),
                   transfer = list(c(0,0),c(1,0),c(1,0)),
                   method="ML")
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) #statistical significance of estimated coefficients
#Check residuals
CheckResiduals.ICAI(TF.fit, lag=50)
#If residuals are not white noise, change order of ARMA
#Remaning correlations in lags 4 and 5 and 14. Increase orders


## FIFTH --------------------------------------------
#### Try (0,1,5)(2,0,0)7
TF.fit <- arima(y.tr,
                xtransf = x.tr,
                order=c(0,1,5),
                seasonal = list(order=c(2,0,0),period=7),
                transfer = list(c(0,0),c(1,0),c(1,0)),
                method="ML")
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) #statistical significance of estimated coefficients
#Check residuals
CheckResiduals.ICAI(TF.fit, lag=50)
#If residuals are not white noise, change order of ARMA
#Correlations are modeled, But many MA terms not significant. Try AR alternative


## SIXTH --------------------------------------------
#### Try (5,1,0)(2,0,0)7
TF.fit <- arima(y.tr,
                   xtransf = x.tr,
                   order=c(5,1,0),
                   seasonal = list(order=c(2,0,0),period=7),
                   transfer = list(c(0,0),c(1,0),c(1,0)),
                   method="ML")
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) #statistical significance of estimated coefficients
#Check residuals
CheckResiduals.ICAI(TF.fit, lag=50)
#All coefficients significant --> Good.
#All correlations are small   --> Good.

#Check Cross correlation residuals - expl. variables
res <- residuals(TF.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x.tr[,1])
ccf(y = res, x = x.tr[,2])
ccf(y = res, x = x.tr[,3])
########
#X1 seems correlated with residuals -> modify TF

## SEVENTH --------------------------------------------
#### Try (5,1,0)(2,0,0)7 and modify TF for X1
TF.fit <- arima(y.tr,
                   xtransf = x.tr,
                   order=c(5,1,0),
                   seasonal = list(order=c(2,0,0),period=7),
                   transfer = list(c(0,1),c(1,0),c(1,0)),
                   method="ML")
summary(TF.fit) #summary of training errors and estimated coefficients
coeftest(TF.fit) #statistical significance of estimated coefficients
#Check residuals
CheckResiduals.ICAI(TF.fit, lag=50)
#All coefficients significant ???
#All correlations are small   --> Good.

#Check Cross correlation residuals - expl. variables
res <- residuals(TF.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x.tr[,1])  # --> much better
ccf(y = res, x = x.tr[,2])
ccf(y = res, x = x.tr[,3])


## Finished model identification

#Check fitted
autoplot(y.tr, series = "Real")+
  forecast::autolayer(fitted(TF.fit), series = "Fitted")

#Chech training errors of the model
accuracy(fitted(TF.fit),y.tr)

PlotModelDiagnosis(x.tr, y.tr, fitted(TF.fit), together = TRUE)


#################################################################################
## Forecast for new data with h = 7
#################################################################################

x.tv <- read_excel("DAILY_DEMAND_TV.xlsx")
#divide temperatures for validation
x.tv$tcold <- sapply(x.tv$TEMP,min,17)
x.tv$thot <- sapply(x.tv$TEMP,max,22)

x.tv <- ts(x.tv[,c(2,4,5)])
#Obtain forecast for horizon = 7 using the trained parameters of the model
val.forecast_h7 <- TF.forecast(y.old = y.tr, #past values of the series
                              x.old = x.tr, #Past values of the explanatory variables
                              x.new = x.tv, #New values of the explanatory variables
                              model = TF.fit, #fitted transfer function model
                              h=7) #Forecast horizon

val.forecast_h7 * 100

