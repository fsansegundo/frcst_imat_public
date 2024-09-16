#' ---
#' title: "Lab. 2 ARMA models"
#' author: "Análisis y Predicción de Series Temporales, 3º B IMAT"
#' date: "2024-09-013"
#' output: 
#'  html_document:
#'    toc: true
#'  # word_document: default
#' ---

#' 
#' 
#' # Lab. 2 ARMA models
#' 
#' 

#' # Preliminaries
#' 
#' ## Load libraries
#' 
#' If you get an error go to the packages panel, click install and 
#' type the name of the library (you only need to do this once).

#+ message = FALSE
library(fpp2)
library(tidyverse)
library(readxl)
library(lmtest) #contains coeftest function

#' To install the next one you need to: 
#' 
#'    + Download the file `MLTools_0.0.31.tar.gz` from Moodle ([R libraries section](https://sifo.comillas.edu/course/view.php?id=46499#section-9&module-3149241)).
#'    + Go to the packages panel, 
#'    + Click *Install*
#'    + Under *Install from* select *Package Archive File*
#'    + Navigate to the folder where you downloaded the package and select it.
#'    + Click install and wait for the setup to finish.

#+ message = FALSE
library(MLTools) 

#' ## Set the working directory 

#+ eval = FALSE

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#' # Generating a time series using the ARMA process
#' 
#' Recall the equation of the ARMA(p, q) process:
#' 
#' $$(1 - \phi_1 B - \phi_2 B^2)y_t = (1 - \theta_1 B - \theta_2 B^2)\epsilon_t$$


#' We use two vectors of coefficients for the AR and MA parts (with $p\leq 2, q\leq 2$).
#' See the code lines below and make sure you choose the signs correctly.

ar_cff <- c(0, 0)
ma_cff <- c(1/3, 0)

#' We will create a gaussian white noise time series. In order to do that we 
#' get a sample of $n$ random values from a standard normal  $Z \sim N(0, 1)$. 

set.seed(2024)

n <- 800

w <- ts(rnorm(n, mean = 0, sd = 1) )

#' Now we generate a time series $y_t$ using the ARMA(p, q) process. In order 
#' to start the process we assume that the values of $y_t, w_t$ for $t < 1$ are 0.

y <- numeric(n)

y[1] <- w[1]
y[2] <- w[2] + ar_cff[1] * y[1] - ma_cff[1] * w[1]

for(i in 3:n){
  y[i] <- ar_cff[1] * y[i - 1] + ar_cff[2] * y[i - 2] + 
    w[i] - ma_cff[1] * w[i - 1] - ma_cff[2] * w[i - 2]
}

y <- ts(y)

#+ fig.width=12, fig.height=6
ggtsdisplay(y, lag.max = 20)

#' ## ARMA model selection and fit
#' 
#' Now we propose a structure of the model by selecting the values of p and q

p <- 0
q <- 1

# We fit the model with estimated order

arima.fit <- Arima(y, order=c(p, 0, q), include.mean = FALSE)

# And we use `summary` to display the training errors and estimated coefficients.

summary(arima.fit) 

#' # Model diagnosis

#' The next function explores the statistical significance of the 
#' estimated coefficients

coeftest(arima.fit) 

#' We use the information to update the ARMA structure if needed.

#' Using autoplot with the fitted model results in a *root plot*
#' to check stationarity and invertibility

#+ fig.width=5, fig.height=5
autoplot(arima.fit) 

#' ## Checking the residuals
#' 
#' The residual plots can be used to check the hypothesis of the ARMA model:
#' 
#'  + The time plot of the residuals should look like white noise (no patterns)
#'  + The histogram and density curve help assess the normality.
#'  + The ACF and PACF should show no significant values (up to 5%), in particular in the first lags.
#'    

#+ fig.width=12, fig.height=6
CheckResiduals.ICAI(arima.fit, bins = 100, lag=20)

#' Remember: you should also check the p-value of the Ljung-Box test in the output of `CheckResiduals`.
#' 
#' An alternative residual plot is obtained with
#' 
#+ fig.width=12, fig.height=6
ggtsdisplay(residuals(arima.fit), lag.max = 20)


#' Keep in mind that if the residuals are not white noise, you should change the order of the ARMA model.
#' 

#' # Fitted time series and forecasting with the fitted model

#' We begin by reconstructing the series with the model.  That is, we obtain the fitted value at $t$
#' $$\hat{y}_{t|t-1}$$
#' for all $t$ in the training data.
#' 
#' The following plot compares the original time series with the reconstructed (fitted) values:

#+ fig.width=12, fig.height=5
autoplot(y, series = "Real") +
  forecast::autolayer(arima.fit$fitted, series = "Fitted")

#' Now, for a **true forecast of future values** we use the `horizon` parameter `h`:

y_est <- forecast::forecast(arima.fit, h=5)

#+ fig.width=12, fig.height=5
autoplot(y_est)

