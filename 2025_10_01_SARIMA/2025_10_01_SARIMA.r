#' ---
#' title: "Lab. 4 SARIMA models"
#' author: "Análisis y Predicción de Series Temporales, 3º B IMAT"
#' date: "2024-10-04"
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
#' 
#' 

#' # Preliminaries
#' 
#' ### Load libraries
#' 
#' If you get an error go to the packages panel, click install and 
#' type the name of the library (you only need to do this once).

#+ message = FALSE

library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)  #contains coeftest function
library(tseries) #contains adf.test function
library(tidyverse)
library(TSstudio)

## Set working directory ---------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#' # Car Registration Example

#' ### Load dataset 

fdata <- read_excel("CarRegistrations.xls")

#' Visualize the first rows:

head(fdata)


#' ## Convert character columns to R date type

fdata$Fecha <- as.Date(fdata$Fecha, format = "%d/%m/%Y")
head(fdata)

#' Order the table by date (using the pipe operator)

fdata <- fdata %>% arrange(Fecha)

# same as: fdata <- arrangeFecha# same as: fdata <- arrange(fdata,DATE)

#' ## Check for missing dates (time gaps)
#' 

range(fdata$Fecha)

#' Create a complete sequence of months with the same range and
#' compare it to the dates in our data.

date_range <- seq.Date(min(fdata$Fecha), max(fdata$Fecha), by = "months")

#' Now we do the comparison

date_range[!date_range %in% fdata$Fecha] 

#' We also check for duplicates
#' 

fdata %>% 
  select(Fecha) %>% 
  duplicated() %>% 
  which()


#' ## Check for missing data (NA) in time series value columns
#' 

sum(is.na(fdata$CarReg))


#' 
#' ### Convert it to a time series object   
#' 
#' This is monthly data, so the frequency is 12. The series start in January. 
#' If it starts in e.g. April we would use start = c(1960, 4)

freq <- 12
start_date  <-  c(1960, 1)
y <- ts(fdata$CarReg, start = start_date, frequency = freq)

#' ### Time plot:  

#+ fig.width=12, fig.height=4
autoplot(y) +
  ggtitle("Car registrations") +
  xlab("Year") + ylab("Number registered (thousands)")

#' 
#' ## Train/test split
#'

n <- length(y)

#' Leave 5 years for validation
n_test <- floor(5 * freq)

y_split <- ts_split(y, sample.out = n_test)
y.TR <- y_split$train
y.TV <- y_split$test


#' Alternatively, with subset
y.TR <- subset(y, end = length(y) - 5 * 12) 
y.TV <- subset(y, start = length(y) - 5 * 12 + 1)

#' And let us visualize the split.

#+ fig.width=12, fig.height=8
autoplot(y.TR, color = "orange") + 
  autolayer(y.TV, color = "blue")

#' 
#' # Identification and fitting process
#' 

#' 
#' ## Box-Cox transformation
#' 

Lambda <- BoxCox.lambda.plot(y.TR, window.width = 12)

z <- BoxCox(y.TR, Lambda)

#+ fig.width=12, fig.height=8
p1 <- autoplot(z)
p2 <- autoplot(y)
gridExtra::grid.arrange(p1, p2, nrow = 2 )

#' 
#' ## Differentiation
#' 
#' recall if the ACF decreases very slowly -> needs differentiation

#+ fig.width=12, fig.height=4
ggtsdisplay(z,lag.max = 100)

#' 
#' ### Seasonal Differentiation
#' 
#' It is generally better to start with seasonal differencing when a 
#' time series exhibits both seasonal and non-seasonal patterns. 

B12z<- diff(z, lag = freq, differences = 1)

#' We use the ACF and PACF to inspect the result

#+ fig.width=12, fig.height=4
ggtsdisplay(B12z,lag.max = 4 * freq) 

#' ### Regular Differentiation (without seasonal)

Bz <- diff(z,differences = 1)

#+ fig.width=12, fig.height=4
ggtsdisplay(Bz, lag.max = 4 * freq) 

#' ### Both regular & seasonal Differentiation
#' 

#+ fig.width=12, fig.height=4
B12Bz <- diff(Bz, lag = freq, differences = 1)
ggtsdisplay(B12Bz, lag.max = 4 * freq) 



#' Remember, when you apply both differences the order does not matter. Here we 
#' check that visually:

B_B12z<- diff(B12z, differences = 1)

#+ fig.width=12, fig.height=4
autoplot(B12Bz, color = "blue", size = 2) + autolayer(B_B12z, color = "orange", size = 0.5)



#' ### Model Order (p, d, q)(P, D, Q)_s selection

#' Now, using the results above, we select both the regular d and the seasonal D orders of differencing
#' and the ARMA structure, both regular and seasonal
#' 

#+ echo=FALSE
final <- TRUE

#+ eval = !final, echo=FALSE, results='asis'
cat(' <div style="color:red"><h5>The rest of this document will not work until
we select the right values. We will generate the final version in our
next session.</h5></div>')



#+ eval=final
p <- 0
d <- 1
q <- 1

P <- 1
D <- 1
Q <- 1


#' ## Fit seasonal model with estimated order

#+ eval = final
arima.fit <- Arima(y.TR,
                   order=c(p, d, q),
                   seasonal = list(order=c(P, D, Q), period=freq),
                   lambda = Lambda,
                   include.constant = FALSE)


#' 
#' # Model Diagnosis
#' 

#' ### Summary of training errors and estimated coefficients

#+ eval = final
summary(arima.fit) 

#' ### Statistical significance of estimated coefficients

#+ eval = final
coeftest(arima.fit) 

#' ### Root plot

#+ eval = final, fig.width=12, fig.height=4
autoplot(arima.fit)

#' ### Check residuals

#+ eval = final, fig.width=12, fig.height=4
CheckResiduals.ICAI(arima.fit, bins = 100, lag=100)

# If residuals are not white noise, change order of ARMA

#+ eval = final, fig.width=12, fig.height=4
ggtsdisplay(residuals(arima.fit), lag.max = 100)


#' ### Check the fitted values 
#' 

#+ eval = final, fig.width=12, fig.height=4
autoplot(y.TR, series = "Real") +
  forecast::autolayer(arima.fit$fitted, series = "Fitted")


#' 
#' # Future forecast and validation
#' 

#+ eval = final
y_est <- forecast::forecast(y.TR, model=arima.fit, h=freq)
head(y_est$mean, n = 12)

#+ eval = final, fig.width=12, fig.height=4
#+ 
#+ 
xmin <- 1995
xmax <- 1996
ymin <- 0
ymax <- Inf

autoplot(subset(y, start=length(y.TR) - 30, end=length(y.TR) + freq)) + 
  autolayer(y_est, alpha = 0.5) +
  annotate("rect", xmin=xmin, xmax=xmax, ymin=ymin, ymax= ymax, alpha=0.2, fill="orange") 

#' ## Validation error for h = 1  
#' 
#' 
#' Obtain the forecast in validation for horizon = 1 using the trained 
#' parameters of the model. We loop through the validation period, adding
#' one point of the time series each time and forecasting the next value (h = 1).

#+ eval = final
y.TV.est <- y * NA 


#+ eval = final
for (i in seq(length(y.TR) + 1, length(y), 1)){
  y.TV.est[i] <- forecast::forecast(subset(y, end=i-1), 
                          model = arima.fit,       
                          h=1)$mean                
}


#' Next we plot the series and both forecasts. We limit the values depicted in 
#' the plot to improve the visualization. As you can see the loop forecasts (with
#' h = 1) appear to be better than the h=12 forecasts in y_est.

#+ eval = final, fig.width=12, fig.height=6
autoplot(subset(y, start=length(y.TR) - 24, end = length(y.TR) + 12)) +
  forecast::autolayer(y_est$mean, color="blue") + 
  forecast::autolayer(subset(y.TV.est, start = length(y.TR) + 1, 
                             end = length(y.TR) + 12), color="red")


# autoplot(subset(y, start=length(y.TR) - 24)) +
#   forecast::autolayer(y_est$mean, color="blue") + 
#   forecast::autolayer(subset(y.TV.est, start=length(y.TR) + 1), color="red")

#' Finally we compute the validation errors
#+ eval = final
accuracy(subset(y.TV.est, start = length(y.TR) + 1), y)

#' Uncomment the following line to see the direct computation of RSME
#  sqrt(mean((y.TV.est - y.TV)^2))

# ------------------------------------------------------------------------------ 
# ------------------------------------------------------------------------------ 
# ------------------------------------------------------------------------------ 

#' 
#' # Additional Code
#' 

# ------------------------------------------------------------------------------ 
# ------------------------------------------------------------------------------ 
# ------------------------------------------------------------------------------ 

#' We have seen above that the result of using forecast is different from the 
#' result of the validation loop. Both are using the fitted model to forecast, 
#' so what is the difference? 
#' 
#' Below we will explicitly use the fitted model coefficients to obtain the 
#' forecasted values, both for the validation loop and for the `forecast` 
#' function. Keep in mind that the Arima function incorporates the BoxCox
#' transformation. So to make things simpler, we will work directly with 
#' z instead of y.
#' 
#' Recall that the equation of our model is:
#' $$
#' (1 - \Phi_1B^{12})(1 - B)(1 - B^{12})z_t = 
#' (1 - \theta_1 B)(1 - \Theta_1 B^{12})\epsilon_t
#' $$
#' But we can expand this equation and reorganize it into a forecasting equation:
#' $$
#' z_t = z_t = z_{t-1} + z_{t-12} - z_{t-13} + 
#' \Phi_s (z_{t-12} - z_{t-13} - z_{t-24} + z_{t-25}) + \\
#' \epsilon_t + \theta \epsilon_{t-1} + \Theta_s \epsilon_{t-12} + 
#' \theta \Theta_s \epsilon_{t-13}
#' $$
#' Similarly we can do the same with the error term to get
#' $$
#' \epsilon_t = z_t - z_{t-1} - z_{t-12} + z_{t-13} - 
#' \phi_s (z_{t-12} - z_{t-13} - z_{t-24} + z_{t-25}) - \\
#' \theta \epsilon_{t-1} - \Theta_s \epsilon_{t-12} - \theta \Theta_s \epsilon_{t-13}
#' $$
#' The right hand side of these equation only contains lags of $z_t$ and 
#' $\epsilon_t$. [Section 9.8 of @hyndman2021fpp3](https://otexts.com/fpp3/arima-forecasting.html#arima-forecasting)
#' explains how to apply this forecasting equations. In summary: when you start 
#' forecasting the first values you replace past values of $z$ with training 
#' set values and $\epsilon$ values with the residuals of the fitted model 
#' (also corresponding to training). As you move further into the test set, 
#' unknown values of $z$ and $\epsilon$ will be needed. 
#' The difference between `forecast` and the validation loop is in deciding what
#' values we use for $z$ and $\epsilon$ for $t$ values corresponding to 
#' the test set. Let us explore this in the code.

#' So we begin by splitting z into training and test.  
#' **Technical note:** Lambda was determined using only the training set, and
#' we apply the same Lambda to the test set to prevent data leakage.

#+ eval=final
z <- BoxCox(y, Lambda)
z.TR <- subset(z, end = length(y.TR))
z.TV <- subset(z, start = length(y.TR) + 1)

#' We fit a seasonal model to z with the same order we used for y,
#' but setting lambda equal to 1. 

#+ eval=final
arima.fit <- Arima(z.TR, 
                   order=c(p, d, q),
                   seasonal = list(order=c(P, D, Q), period=12),
                   lambda = 1, 
                   include.constant = FALSE)


#' In the code below you can verify that the model fit is equivalent to what we 
#' obtained above for y. 

#+ eval=final
summary(arima.fit) 

coeftest(arima.fit) 

#+ eval = final, fig.width=12, fig.height=4
autoplot(arima.fit) 

#+ eval = final, fig.width=12, fig.height=4
CheckResiduals.ICAI(arima.fit, bins = 100, lag=100)

#' Now let us begin using this model to forecast the test set values. 
#' 
#' We begin by using the `forecast` function to forecast **all the values** in 
#' the test set. That is, we set the forecasting horizon h equal to the length
#' of the test set,

#+ eval=final
z_est <- forecast::forecast(arima.fit, h=length(z.TV))

#' Next we will generate three additional versions of the forecasts, using a 
#' loop as we did before. 
#' 
#'  + The first forecast `z.TV.est` is exactly what we did before, but using 
#'  z instead of y. 
#'  + The second one `z.TV.est2` will use the two forecasting equations (with the 
#'  fitted coefficients) and will always use **actual values** of $z_t$ in the 
#'  test set. The error terms will also be updated with the second forecasting
#'  equation.  
#'  + The third one `z.TV.est3` will also use the forecasting equation, but it
#'  will use its own **forecasted values** of $z_t$ in the test set. The error terms 
#'  for $t$ in the test set will all be set to zero.

#' First we create empty time series to store the different forecasts.

#+ eval=final
z.TV.est <- z * NA
z.TV.est2 <- z * NA
z.TV.est3 <- z * NA

#' We make the training values of $z_t$ available to the forecasting equation.

#+ eval=final
z.TV.est[1:length(z.TR)] <- z.TR
z.TV.est2[1:length(z.TR)] <- z.TR
z.TV.est3[1:length(z.TR)] <- z.TR

#' Similarly, we prepare two versions of $\epsilon_t$ containing the training 
#' residuals, to be used by the second and third procedure. Even though they 
#' look initially the same, these error terms are updated differently: the 
#' second procedure really updates them with a forecast equation, whereas the 
#' third one leaves the test error values as 0.

#+ eval=final
w2 <- z * 0
w2[1:length(z.TR)] <- residuals(arima.fit)

w3 <- z * 0
w3[1:length(z.TR)] <- residuals(arima.fit)

#' We store the coefficients of the model (`_s` indicates seasonal)

#+ eval=final
Phi_s <- coefficients(arima.fit)["sar1"]

theta <- coefficients(arima.fit)["ma1"]
Theta_s <- coefficients(arima.fit)["sma1"]

#' And now we get the forecasts in a loop.

#+ eval=final
for (i in seq(length(z.TR) + 1, length(y), 1)){# loop for validation period
  
  # The first one is simply what we did in the validation loop above
  
  z.TV.est[i] <- forecast::forecast(subset(z, end=i-1), 
                                    model = arima.fit,  
                                    h=1)$mean           
  
  
  
  # In the second forecast procedure we use the two forecasting equations, with 
  # real values of z and updating the errors with the second equation. 
  
  z.TV.est2[i] <- z[i - 1] + z[i - 12] - z[i-13] + 
    Phi_s * (z[i-12] - z[i-13] - z[i - 24] + z[i-25]) + 
    w2[i] + theta * w2[i - 1] +  Theta_s * w2[i - 12] + theta * Theta_s * w2[i - 13]
  
  w2[i] = z[i] - z[i-1] - z[i-12] + z[i-13] -
    Phi_s * (z[i-12] - z[i-13] - z[i-24] + z[i-25]) - 
    theta * w2[i-1] - Theta_s * w2[i-12] - theta * Theta_s * w2[i-13]
  
  # And in the third one we update the forecasted values z.TV.est3 using their 
  # previous values and we do not update the error terms (they stay 0)
  

  z.TV.est3[i] <- z.TV.est3[i - 1] + z.TV.est3[i - 12] - z.TV.est3[i-13] + 
    Phi_s * (z.TV.est3[i-12] - z.TV.est3[i-13] - z.TV.est3[i - 24] + z.TV.est3[i-25]) + 
    w3[i] + theta * w3[i - 1] +  Theta_s * w3[i - 12] + theta * Theta_s * w3[i - 13]
  
}

#' Let us examine the results, comparing the first values for all the forecast 
#' procedures:

#+ eval=final
k <- 10

#' Using forecast function directly (we called this s_est above):

#+ eval=final
head(z_est$mean, k)

#' Using the validation loop as explained in today's session

#+ eval=final
subset(z.TV.est, start = length(z.TR) + 1, end = length(z.TR) + k)

#' Using the two forecast equations with actual z values and error updates

#+ eval=final
subset(z.TV.est2, start = length(z.TR) + 1, end = length(z.TR) + k)

#' Using only the forecasting equation for y and no error update

#+ eval=final
subset(z.TV.est3, start = length(z.TR) + 1, end = length(z.TR) + k)

#' The results indicate that, up to small rounding errors there are only
#' two different results:  
#' 
#'   + the validation loop is doing what we called the second procedure.
#'   + the `forecast` function is doing what twe called the third procedure.
#'   
#' The updated error values and the use of actual values of the time series 
#' explain why the validation loop produces more accurate values. This is 
#' illustrated in the plot below:


#+ eval = final, fig.width=12, fig.height=6

autoplot(subset(z, start = length(z.TR) - 30), series="Real") +
  forecast::autolayer(subset(z.TV.est, start = length(z.TR)), series="Forecasting Loop") +
  forecast::autolayer(subset(z.TV.est3, start = length(z.TR)), series="Forecast function")

#' Now we can also compare both approaches through their validation errors. 

#+ eval=final
accuracy(z_est, z)
accuracy(z.TV.est, z)

#' As expected, direct use of the `forecast` function leads to worse predictive 
#' performance in the test set. 

#' 
#' # References
#' 

