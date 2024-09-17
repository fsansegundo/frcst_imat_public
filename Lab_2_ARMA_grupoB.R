#' ---
#' title: "Lab. 2 ARMA models"
#' author: "Análisis y Predicción de Series Temporales, 3º B IMAT"
#' date: "2024-09-013"
#' bibliography: "../../forecasting.bib"
#' output: 
#'  html_document:
#'    toc: true
#'    toc_depth: 2
#'  # word_document: default
#' 
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
library(TSstudio)

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

#' # Reading time series data and EDA

#' ## Load dataset 

fdata <- read_excel("ARMA_series.xls")

#' Visualize the first rows:

head(fdata)

#' ## Convert it to a (multi)time series object
#' 
#' In this case:
#' 
#'  + We assume that there are no time gaps
#'  + There is no temporal information in the data, so we use the natural numbers
#'  as index. This is the default in `ts` if no additional arguments are used.

fdata_ts <- ts(fdata)

#' and we get some basic information
#' 

ts_info(fdata_ts)

#' We use the column index to select a time series
y <- fdata_ts[ ,2]


#' # Identification and fitting process

#' ## ACF and PACF plots of the time series 
#' 
#' We use them to identify significant lags and order

#+ fig.width=12, fig.height=6
ggtsdisplay(y, lag.max = 20)

#' ## ARMA model selection and fit
#' 
#' Now we propose a structure of the model by selecting the values of p and q

p <- 1
q <- 1

#' We fit the model with estimated order

arima.fit <- Arima(y, order=c(p, 0, q), include.mean = FALSE)

#' And we use `summary` to display the training errors and estimated coefficients.

summary(arima.fit) 

#' # Model diagnosis

#' The next function explores the statistical significance of the 
#' estimated coefficients

coeftest(arima.fit) 

#' We use the information to update the ARMA structure if needed.

#' Using autoplot with the fitted model results in a *root plot*
#' to check stationarity and invertibility. Keep in mind that this plots
#' show **inverse roots** (that is, $1/root$)

#+ fig.width=5, fig.height=5
autoplot(arima.fit) 

#' ## Check of the residuals
#' 
#' The residual plots can be used to check the hypothesis of the ARMA model:
#' 
#'  + The time plot of the residuals should look like white noise (no patterns)
#'  + The histogram and density curve help assess the normality.
#'  + The ACF and PACF should show no significant values (up to 5%), in particular in the first lags.
#'    

#+ fig.width=12, fig.height=8
CheckResiduals.ICAI(arima.fit, bins = 100, lag=20)

#' Remember: you should also check the p-value of the Ljung-Box test in the output of `CheckResiduals`.
#' 
#' An alternative residual plot is obtained with
#' 
#+ fig.width=12, fig.height=8
ggtsdisplay(residuals(arima.fit), lag.max = 20)


#' Keep in mind that if the residuals are not white noise, you should change the order of the ARMA model.
#' 

#' # Fitted time series and forecasting with the fitted model

#' We begin by reconstructing the series with the model.  That is, we obtain the fitted value at $t$
#' $$\hat{y}_{t|t-1}$$
#' for all $t$ in the training data.
#' 
#' The following plot compares the original time series with the reconstructed (fitted) values:

#+ fig.width=12, fig.height=8
autoplot(y, series = "Real") +
  forecast::autolayer(arima.fit$fitted, series = "Fitted")

#' Now, for a **true forecast of future values** we use the `horizon` parameter `h`:

y_est <- forecast::forecast(arima.fit, h = 5)

#+ fig.width=12, fig.height=8
autoplot(y_est)

#' # ARMA time series simulation
#' 
#' The series we have been using are synthetic time series, obtained as in the example below:

set.seed(2024)
sim_ts <- arima.sim(n = 250, 
                 list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
                 sd = sqrt(0.1796))

#' See [[@hyndman2021fpp3], sections 9.3 and 9.4](https://otexts.com/fpp3/AR.html) for the 
#' conditions that the model coefficients should verify.
#' 


#' 
#' # Additional Code
#' 

#' 
#' ## Arma model selection with train/test split and performance measures
#'
#' Let us go over the same dataset but this time we will use functions from other 
#' libraries to perform a basic train/test split and get some performance measures.


n <- length(y)
n_test <- floor(0.1 * n)

library(TSstudio)
y_split <- ts_split(y, sample.out = n_test)

y_TS <- y_split$test
y_TR <- y_split$train


#' Now we will briefly go over the same model identification and fitting steps, 
#' but using the training values.

#' 
#' #### ACF and PACF plots
#' 

#+ fig.width=12, fig.height=6
ggtsdisplay(y_TR, lag.max = 20)

#' 
#' #### ARMA model selection and fit
#' 

p <- 1
q <- 1

#' 
#' #### Model fit
#' 

arima.fit <- Arima(y_TR, order=c(p, 0, q), include.mean = FALSE)

#' 
#' #### Estimated coefficients.
#' 

summary(arima.fit) 

#' 
#' #### Model diagnosis
#' 

coeftest(arima.fit) 

#' 
#' #### Root plot
#' 

#+ fig.width=5, fig.height=5
autoplot(arima.fit) 

#' 
#' #### Check residuals
#' 

#+ fig.width=12, fig.height=8
CheckResiduals.ICAI(arima.fit, bins = 100, lag=20)

#' 
#' ### Model fitted values, forecast predictions and performance evaluation
#' 
#' Once we are satisfied with the model we can use it to get a true forecast 
#' covering the same time span as the test set:

y_fc <- forecast::forecast(arima.fit, h = n_test)

#' Keep in mind that this `forecast` object is more than just a set of values:
#' It is a list containing a lot of information about the model we fitted and
#' the forecast for the selected horizon, as suggested by this plot:

#+ fig.width=12, fig.height=8
autoplot(y_fc)

#' But if we want the forecasted values we can get them like this:
#' 

y_fc$mean

#' Note that as indicated by the plot these values decay to 0 very quickly.

#' We can also use `test_forecast` from TSstudio to get a plot of the actual 
#' values, the model's fitted values (for training or *in-sample*), and these 
#' forecasted values (for the test set or *out of sample*).


#+ fig.width=12, fig.height=8
test_forecast(actual = y,
              forecast.obj = y_fc,
              test = y_TS)

#' We can get the model's **performance measures** as follows:
#' 

forecast::accuracy(y_fc, y)

#' Ypu can check that e.g. the RSME obtained here is the same as the 
#' direct computation:

sqrt(mean((y_est$mean - y_TS)^2))

#' ## Generating a time series using the ARMA process
#' 
#' Recall the equation of the ARMA(p, q) process:
#' 
#' $$(1 - \phi_1 B - \phi_2 B^2)y_t = (1 - \theta_1 B - \theta_2 B^2)\epsilon_t$$
#' 
#' As a follow up pf the introduction to stochastic processes in the previous 
#' session we will see how to generate a time series that is a realization of 
#' one such process.


#' We use two vectors of coefficients for the AR and MA parts (with $p\leq 2, q\leq 2$).
#' See the code lines below and make sure you choose the signs correctly.

ar_cff <- c(0, 0)
ma_cff <- c(1/3, 0)
sd <- sqrt(0.1796)


#' We will create a gaussian white noise time series. In order to do that we 
#' get a sample of $n$ random values from a standard normal  $Z \sim N(0, 1)$. 

set.seed(2024)

n <- 800
n <- 250

w <- ts(rnorm(n, mean = 0, sd = sd) )

#' Now we generate a time series $y_t$ using the ARMA(p, q) process. In order 
#' to start the process we assume that the values of $y_t, w_t$ for $t < 1$ are 0.

y_sim <- numeric(n)

y_sim[1] <- w[1]
y_sim[2] <- w[2] + ar_cff[1] * y_sim[1] - ma_cff[1] * w[1]

for(i in 3:n){
  y_sim[i] <- ar_cff[1] * y_sim[i - 1] + ar_cff[2] * y_sim[i - 2] + 
    w[i] - ma_cff[1] * w[i - 1] - ma_cff[2] * w[i - 2]
}

y_sim <- ts(y_sim)

#' Our generated y_sim time series is not as good as the one we obtained 
#' previously with `arima.sim` but, as the following plot illustrates, it 
#' exhibits a very similar dynamic. 

#+ fig.width=12, fig.height=6
ggtsdisplay(y_sim, lag.max = 20)

#' ### Suggested exercise:
#' 
#'  + Can you identify and fit an ARMA model for `y_sim`?
#'  + Are the coefficients you find related to our choices in constructing `y_sim`?
#'  
#'  





#' 
#' # References
#' 
