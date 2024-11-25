#' ---
#' title: "Lab. 7 Dynamic regression models"
#' author: "Análisis y Predicción de Series Temporales, 3º B IMAT"
#' date: "2024-10-25"
#' bibliography: "../forecasting.bib"
#' output: 
#'  pdf_document: default
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
#' ## Preliminaries
#' 
#' ### Load libraries
#' 

#+ echo = FALSE, message = FALSE

if(require("klippy"))klippy(position = "r")

#+ message = FALSE

library(MLTools)
library(fpp2)
library(ggplot2)
library(readxl)
library(lmtest)  
library(tseries) 
library(tidyverse)
library(TSstudio)
library(astsa)
library(TSA)
library(Hmisc) # for computing lagged variables


#' 
#' ### Set working directory 
#' 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#'
#' # Transfer Function Examples
#' 
#'  

#' 
#' We will generate some examples following Pankratz's formulation of dynamic regression models:
#' $$y[t] = c + \dfrac{\omega(B)}{\delta(B)} x[t - b] + v(t)$$
#' where $y$ is the output variable, $x$ is the input and $v$ is the autocorrelated ARMA noise.
#' 
#' ### First example
#' 
#' For the first example let the input variable x1 be white noise. 


# Generate random explanatory variable

set.seed(2024)
N <- 500
x1 <- rnorm(N)
x1 <- ts(x1)

#+ fig.width=12, fig.height=4
ggtsdisplay(x1, lag.max = 100, main = "x1", plot.type = "histogram")

#' We also simulate the **ARMA noise** $v(t)$. (try to simulate 10 * N samples and compare ACF).
set.seed(100)
ar_noise <- arima.sim(n = N, 
                      list(ar = c(0.8)),
                      sd = sqrt(0.25))

#+ fig.width=12, fig.height=4
ggtsdisplay(ar_noise,lag.max = 100, main = "ARMA noise")


#' **Exercise:** are the ACF and PACF what you expected?


#' Next we generate the output $y1$ following Pankratz's model:

y1 <- 0.7 * x1 + ar_noise

#' **Exercise:**  
#' 
#' + What is $\omega(B)$ here, what is its degree $s$?
#' + What is $\delta(B)$ here, what is its degree $r$?
#' + What is the lag $b$? What about $c$?
#' 
#' Next we create a multidimensional time series with these variables: input, 
#' output and noise

fdata1 <- cbind(x1, y1, n=ar_noise)
head(fdata1)

#' 
#' #### Cross-correlation function

#' In order to explore the correlation between one time series y and the lags 
#' of another time series x we can use this *cross correlation function* 
#' `ccf(y, x)`. For example, the first lag of the time series x1 is (trivially) 
#' perfectly correlated with x1. Let us explore this with ccf:

x1_lag1 <- Lag(x1, shift = 1)

#' This `Lag` function (from Hmisc) is different from the `lag` function (from 
#' stats) that we have used in previous sessions. It keeps the origin of the 
#' time series but fills the lagged positions with NAs.
 
head(x1_lag1)

#' **Exercise:** Look at the value of `Start` in the previous output. Now run   
#' `stats::lag(x1)` and compare the output.

head(stats::lag(x1, n = 1))

#' Thus we fill that missing value with zero.

x1_lag1[is.na(x1_lag1)] <- 0

#' And now we can plot the ccf values. We use abline to add some red dashed 
#' lines to help you identify the first lags (on both sides of zero)

#+ fig.width=12, fig.height=4
ccf(y = x1, x = x1_lag1, na.action = na.pass)
abline(v = -5:5, col="orange", lty = 2)

#' Alternatively, you can use this (without setting the NAs to 0)

#+ fig.width=12, fig.height=4
ccf(y = x1, x = x1_lag1, na.action = na.pass)
abline(v = -5:5, col="orange", lty = 2)

#' Either way, the ccf plot shows that (as expected) x1(t) has cross correlation 
#' equal to 1 with its lag x1(t - 1).  
#' 
#' **Notes about ccf:**  
#' 
#'  + In general **the lag k value returned by ccf(x, y) estimates the correlation
#'   between `x[t+k]` and `y[t]`**. When you apply it to the lag of x1, that is when
#'   `y[t] = x1[t]` and `x[t] = x1[t - 1]` then the value k units to the right
#'   shows the correlation between `x1[t]` and `x1[t - 1 + k]`. In this example, 
#'   where x1 is white noise the ony non-zero value occurs at k = 1.
#'  + You can use print(ccf(...)) to see the numeric values of the ccf
#'  
#' If we use ccf to explore the cross correlation between the output and input 
#' of this example we get a clear indication that they are correlated for t = 0.
#' This is a consequence of the equation model, but the (lack of) autocorrelation
#' of x1 also plays a role here.
#' 
 
#+ fig.width=12, fig.height=4
ccf(y = y1, x = x1)
abline(v = -5:5, col="orange", lty = 2)

#' 
#' #### Classical linear regression model 
#' 
#' If we try to study the relation of x1 and y1 with a 
#' classical (non dynamic) linear regression model:

lm1.fit <- lm(y1 ~ x1 - 1, data = fdata1 )
summary(lm1.fit)

#' You can see that even though the model coefficient is significant, the 
#' residuals of the model do not look like white noise:

#+ fig.width=12, fig.height=4
CheckResiduals.ICAI(lm1.fit, lag=100)

#' Therefore this model fails to extract all the information about y1. However, 
#' in this example there is no cross correlation between the residuals of the 
#' model and x1 (recall x1 is white noise, so it has no autocorrelation).

#+ fig.width=12, fig.height=4
ccf(y = residuals(lm1.fit), x = x1)
abline(v = -5:5, col="orange", lty = 2)

#' 
#' #### Transfer function model 
#' 
#' Let us return to the Pankratz's transfer function model connecting x1, y1 
#' and the ARMA noise. In order to fit this type of model we again use an 
#' `arima` function, but this one belongs to the TSA library:

arima1.fit <- TSA::arima(y1,
                    order=c(1,0,0),
                    #seasonal = list(order=c(0,0,0),period=24),
                    xtransf = x1, # input variable(s)
                    transfer = list(c(0,0)), # values of r and s
                    include.mean = FALSE,
                    method="ML")

#' **Note the xtransf and transfer arguments.** We will use this arguments of 
#' the function to describe the components of the Pankratz's formulation of this
#' model. Bo not worry if you do not understand this function right away, we 
#' will see the details in coming examples. 
#' 
#' The **diagnosis** of these models shares some aspects of the SARIMA model 
#' diagnosis we have discussed.

#' We begin by examining the summary of training errors and the significance of
#' the estimated coefficients

summary(arima1.fit) 

coeftest(arima1.fit) 

#' Note that the table now includes information about the coefficients of the 
#' transfer function: `T1-MA0` here.
#' 
#' Next we check if the residuals look like white noise

#+ fig.width=12, fig.height=4
CheckResiduals.ICAI(arima1.fit, lag=100)

#' Notice that they do, in contrast with the classical linear model. 
#' 
#' Finally, there is a **new step in the diagnosis of the model: the ccf of the
#' input with the residuals**. If the model is well fitted there should be no 
#' cross correlation left (as in this case).

#+ fig.width=12, fig.height=4
ccf(y = residuals(arima1.fit), x = x1)
abline(v = -5:5, col="orange", lty = 2)


#' 
#' ### Second example
#' 
#' For our second example we will keep the ARMA noise term, but we will consider
#' an input time series with autocorrelation, as illustrated by the ACF plot 
#' below:

x2 <- read.table("TEMP.dat", sep = "", header = TRUE)
x2 <- ts(x2$TEMP)

#+ fig.width=12, fig.height=4
ggtsdisplay(x2,lag.max = 100, main = "x2")


#' We create and output variable y2 as before.

y2 <- 0.7 * x2 + ar_noise

#' **Exercise:** Ask yourself the same questions as in the previous example. 
#' That is , think about $\omega(B)$ and its degree $s$, about $\delta(B)$ and 
#' its degree $r$ and also about the lag $b$.

#' We again create a multidimensional time series with these variables.

fdata2 <- cbind(x2, y2, n=ar_noise)
head(fdata2)

#' 
#' #### Cross-correlation function

#' If we examine the ccf between the input x2 and the output we get a much more
#' complicated picture:

#+ fig.width=12, fig.height=4
ccf(y = y2, x = x2)
abline(v = -5:5, col="orange", lty = 2)

#' **Exercise:** Why? What is different in this example?


#' 
#' #### Classical linear regression model 
#' 

#' And if we apply a classical linear regression

lm2.fit <- lm(y2 ~ x2 - 1, data = fdata2 )
summary(lm2.fit)

#' Then the residuals still do not look as they should

#+ fig.width=12, fig.height=4
CheckResiduals.ICAI(lm2.fit, lag=100)

#' But we have an additional problem. There is cross correlation between the 
#' residuals and the input:

#+ fig.width=12, fig.height=4
ccf(y = residuals(lm2.fit), x = x2)
abline(v = -5:5, col="orange", lty = 2)


#' 
#' #### Transfer function model 
#'
#'
#' To address the shortcomings of classical linear regression we fit a 
#' transfer function model:
#' 

arima2.fit <- arima(y2,
                    order=c(1,0,0),
                    #seasonal = list(order=c(0,0,0),period=24),
                    xtransf = x2,
                    transfer = list(c(0,0)), # values of r and s
                    include.mean = FALSE,
                    method="ML")

summary(arima2.fit) # summary of training errors and estimated coefficients

#' Again, do not worry if you do not understand all the details just yet. 
#' 
#' For the diagnosis we check the **significance of the coefficients**

coeftest(arima2.fit) 

#' We check if the **residuals look like gaussian white noise** (see the Ljung-Box 
#' test result)

CheckResiduals.ICAI(arima2.fit, lag=100)

#' And finally we **check the cross correlation between the residuals and 
#' the input**. 

#+ fig.width=12, fig.height=4
ccf(y = residuals(arima2.fit), x = x2)
abline(v = -5:5, col="orange", lty = 2)

#' As you can see, this model passes all our checks.
#' 
#' **Exercise:** Go back to the first example and check the code line defining 
#' the model to a new output:
#' 
#' `y3 <-  70 * x1 + ar_noise`
#' 
#' What changes do you expect in the model? Run the code and see what happens.

#' 
#' ### Third example
#' 
#' The third example will be very similar to the previous one, but our input will 
#' be a lagged version of the variable x2.
#' 
#' We obtain it using `Lag` and remove the initial NAs. Keep an eye on the 
#' resulting `Start` value of the time index:
#' 

x2lg3 <- na.omit(Lag(x2, 3))
head(x2lg3)

#' To make sure that our time series do not contain missing values we will make 
#' them share that initial value with `ts.intersect`:

fdata4 <- ts.intersect(x2, x2lg3, ar_noise)

#' We save the column names to restore them after we add the output column y4
nc4 <- ncol(fdata4)
cnames4 <- colnames(fdata4)
head(fdata4)

#' And now we are ready to add the output.
fdata4 <- cbind(fdata4, y4 = 0.7 * fdata4[,"x2lg3"] + fdata4[,"ar_noise"])
head(fdata4)
#' and restore the variable names for the initial columns:
colnames(fdata4)[1:nc4] <- cnames4
head(fdata4)

#' The preceding definition of y4 implies that the model we are using to 
#' generate this example is:
#' $$y_4[t] = \dfrac{\omega(B)}{\delta(B)} x_2[t - 3] + v(t)$$
#' where $v(t)$ is the same ARMA(1, 0) noise we have used in previous examples. 
#' 
#' Keep in mind that in a real problem we would get the output $y_4(t)$ and the 
#' input $x_2(t)$, **not its lagged version**. And initially we would not know what
#' particular lag b (such as 3 here) we should consider for `x2[t - b]`.
#' 
#' Note also that for these examples we are keeping the functions $\omega(B)$ and
#' $\delta(B)$ extremely simple. In later examples we will see how to deal with
#' the general case.
#'  
#' In order to find what lag b we should use we can try several things. First, to 
#' simplify the code we will get shorted versions of the columns of the 
#' (multi)time series:

y4 <- fdata4[,"y4"]
x2 <- fdata4[,"x2"]
x2lg3 <- fdata4[,"x2lg3"]

#' Now, one of the first ways to look for b that you may consider is to look at 
#' the ccf between input and output:

#+ fig.width=12, fig.height=4
ccf(y = y4, x = x2, lwd=2)
abline(v = -5:5, col="orange", lty = 2)

#' In this very simple example we can still notice that b = 3 is *special* in a
#' way. But in general, with more complicated $\omega$ and $\delta$ in the model, 
#' this plot will not be easy to interpret. 
#' 
#' An alternative and much better way to look for b is to consider a preliminary 
#' and very simple transfer function model that contains a high value of s (the 
#' degree of the transfer function numerator) and a low autoregressive order 
#' for the noise. 

TF.fit <- TSA::arima(y4,
                order=c(1, 0, 0),
                #seasonal = list(order=c(1,0,0),period=24),
                xtransf = x2,
                transfer = list(c(0, 9)), #List with (r,s) orders
                include.mean = TRUE,
                method="ML")

summary(TF.fit)

#' Now when we look at the statistical significance of the estimated 
#' coefficients of the model, the one corresponding to b stands out:

coeftest(TF.fit) 

#' In this example the analysis is easy because $\omega(B)$ and
#' $\delta(B)$ are so simple. Later we will see how to use this model to find b 
#' even when the transfer function is more complicated. We will learn how to use
#'  an *identification plot* such as this:

#+ fig.width=12, fig.height=4
TF.Identification.plot(x2, TF.fit)

#' Even without understanding it completely, we can see that the value 3 stands 
#' out in this plot.

#' To check if differentiation is needed and to propose an ARMA structure for the 
#' noise term we will use a regression error plot like this one:

#+ fig.width=12, fig.height=4
TF.RegressionError.plot(y4, x2,TF.fit,lag.max = 100)

#' We see a hint of a trend in this plot, so we will apply a regular difference 
#' (d = 1) when fitting the model. Also from the ACF and PACF we propose a 
#' (p, q) = (1, 0) ARMA structure.
#' 
#' **NOTE:** If this regression error is not stationary in variance,boxcox should be applied to input and output series.

#' After seeing this plot we are ready to fit a Transfer function model as in the 
#' previous examples (remember $\omega, \delta$ are very simple and so r = s = 0). 
#' First we use the lagged version with b= 3 of x2 as input.

xlag = Lag(x2, 3)   # b
xlag[is.na(xlag)]=0

#' And now we fit the model:

arima.fit <- arima(y4,
                   order=c(1, 1, 0), # ARMA model for the noise
                   #seasonal = list(order=c(0,0,0),period=24),
                   xtransf = xlag,
                   transfer = list(c(0, 0)), #List with (r,s) orders
                   include.mean = FALSE,
                   method="ML")

summary(arima.fit)

#' The diagnosis begins by checking the statistical significance of the 
#' estimated coefficients (compare them with the values used to define y4)

coeftest(arima.fit) 

#' Next we check if the residuals qualify as white noise:

CheckResiduals.ICAI(arima.fit, lag=25)

#' And finally we check that there is no cross correlation between the residuals 
#' and the input variable

#+ fig.width=12, fig.height=4
res <- residuals(arima.fit)
res[is.na(res)] <- 0
ccf(y = res, x = x2)

#' #### Prewhitening
#' 
#' We said before that directly examining the ccf of input vs output is not easy 
#' when the input is autocrrelated. Prewhitening  is a technique that helps in 
#' such cases. where you want to examine the relationship between two time 
#' series without the distorting effect of autocorrelation. To perform 
#' prewhitening:
#' 
#' 1. Fit an ARIMA model to the input to remove its autocorrelations.
#' 2. Obtain the residuals (should behave like white noise).
#' 3. Apply the same model to the output series to get a *transformed output*.
#' 	
#' Finally, we can compute the ccf of the transformed output and the residuals.
#' 	
#' Fortunately there is a function in TSA that performs all this process for 
#' us and directly outputs the final ccf:

#+ fig.width=12, fig.height=4
prewhiten(y4, x2, main="Prewhitening of y4 & x2")
abline(v = -5:5, col="orange", lty = 2)

#' The result is very clear in this example. You should compare this to the 
#' direct ccf that we already obtained before for this example:

#+ fig.width=12, fig.height=4
ccf(y4, x2)
abline(v = -5:5, col="orange", lty = 2)

