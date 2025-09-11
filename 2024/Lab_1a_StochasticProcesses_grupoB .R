#' ---
#' title: "Lab. 1a. Introduction to Stochastic Processes. "
#' author: "Análisis y Predicción de Series Temporales, 3º B IMAT"
#' date: "2024-09-06"
#' output: 
#'  html_document:
#'    toc: true
#' bibliography: "../../forecasting.bib"
#' ---


#' 
#' #  Lab Practice 1a: Quick Introduction to Stochastic Processes.
#' 
#' 


#' # Preliminaries
#' 
#' ## Load libraries
#' 
#' If you get an error go to the packages panel, click install and 
#' type the name of the library (you only need to do this once).

#+ warning=FALSE, message=FALSE
library(fpp2) 
library(tidyverse)
library(TSstudio)

#' ## Set working directory 

#+ eval = FALSE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#' # Stochastic Processes
#' 
#' ## A simple example
#'
#' $$X_t = A(\omega) \sin\left(\frac{2  \pi t}{500}\right) $$ 
#' 
#' where $A(\omega)$ is uniformly distributed in $[0, 3]$

#' We create T samples (time index 1, 2, ..., T)

T <- 500

#' and we generate k different realizations of the stochastic process.

k <- 5

#' We set the random seed to ensure reproducibility

set.seed(2024)

#' Now we generate a sample of k values of $A(\omega)$, one for each realization. 

A <- runif(k, min = 0, max = 3)
A

#' **Note:** the value of $\omega$ is *invisible!* What we get is 
#' $$A(\omega_1), A(\omega_2), \ldots, A(\omega_k)$$
#' And we use them to create those realizations.

#' First we create a matrix such that each column will be a realization.

X = matrix(nrow = T, ncol = k)

#' The time index 
idx <-  1:T

#' is used in a for loop to fill up the matrix
for(i in 1:k){
 X[, i] = A[i] * sin(2 * pi * idx/500) 
}

#' The filled matrix has dimension looks like this

dim(X)

#' and the first rows look like this

head(X)

#' Let us convert the matrix to a (multi) time series and plot it:
X <- ts(X)

#+ fig.width=12, fig.height=6
ts_plot(X)

#' A fixed row of the matrix is a sample of the $X_t(\omega)$ random variable. 
#' For example with $t  = 300$ we are looking at the 300th row, What is the 
#' distribution of this random variable?  (Change k to 100 to create many more
#' realizations and uncomment the code below)

#+ fig.width=12, fig.height=6
# boxplot(X[300, ], horizontal = TRUE)
# stripchart(X[300, ], add = TRUE, method = "jitter", 
#            jitter = 0.1, col="red", pch = "*")


#' # White Noise


#' We will create a gaussian white noise time series. In order to do that we 
#' get a sample of $n$ random values from a standard normal  $Z \sim N(0, 1)$. 

n <- 150
z <- rnorm(n, mean = 0, sd = 1) 
# 0 and 1 are the default values for rnorm so this is equivalent to rnorm(n)

head(z, 30)

#' Let us now use this to define a `ts` time series object. Note that now we are
#' **not** providing the frequency, start, etc. In this case, the `ts` function will 
#' create a time index using the natural numbers $t = 1, 2, 3, \ldots$.

w <- ts(z)
head(w, 25)

#' And if we make a time plot we can see that the result is really *noisy*:

#+ fig.width=12, fig.height=6
autoplot(w) +
  ggtitle("White noise") + 
  geom_point(aes(x = 1:n, y = z), size=1.5, col="blue")

#' White noise series are our basic example of *unpredictable*. But they are very important, 
#' in particular for these two reasons:
#' 
#'  + They are often the starting point or basic building block from which we create 
#'  more complex time series.
#'  + In time series modeling one of our goals is to obtain a decomposition of
#'  $$\text{data = signal (model) + noise}$$ 
#'  In particular we need a way to identify noise when we see it. 


#' 
#' # Random walks with or without drift 
#' 

#' A **random walk** is an stochastic process usually defined by the recursive equation:
#' $$y_t = k + y_{t-1} + w_t$$
#' where $w_t$ is white noise (the random walk is gaussian if $w_t$ is gaussian). 
#' The value $k$ is the **drift constant** and a random walk with $k \neq 0$ is a **random walk with drift**.

#' In the above expression computing $y_1$ requires knowledge of $y_0$, so 
#' we set $y_0 = 0$ (equivalently $y_1 = k + w_1$).
#' 
#' With this assumption, an equivalent definition of random walk is:
#' 
#' the sum of a linear trend term and the cumulative sum of the white noise process:  
#' $$y_t = k\cdot t + \displaystyle\sum_{i = 1}^t w_i $$
#' 
#' Let us simulate n values of a random walk with drift:

n = 1000
set.seed(2024)

#' Let k be the drift constant
k = 0.1

#' First we create the white noise time series values:

w = 10 * rnorm(n)

#' and then

rw_ts = ts(k * (1:n)  + cumsum(w))

#' Now if we look at the time plot, it does not look like noise any more!

#+ fig.width=12, fig.height=6
autoplot(rw_ts) +
  ggtitle("Random walk with drift") 


#' **Note:** compare this to the  Google’s daily closing stock price series in 2015 (Figure 5.8 in 
#' [Section 5.2 of Hyndman, FPP3](https://otexts.com/fpp3/simple-methods.html#example-googles-daily-closing-stock-price))
#' 
#' The reason behind this is that the recursive relation $y_t = k + y_{t-1} + w_t$ creates 
#' a **correlation structure** between the values of the time series. The random variables
#' $y_t$ and $y_t-1$ are no longer uncorrelated.
#'
#' If we plot the original series and one of the first lags this correlation is apparent 
#' (to enhance the visualization we only plot the first 100 values):

rw_ts_lag <- stats::lag(rw_ts, k=-5)

#' Note the `Start` value of the lagged series. 
head(rw_ts_lag, 20)

#+ fig.width=12, fig.height=6
autoplot(head(rw_ts, 100), color="blue", alpha = 0.5) + 
  autolayer(head(rw_ts_lag, 100), color="red", alpha = 0.5)

#'
#' # Autocorrelation
#'

#' Recall that the **autocorrelation function** of $y_t$ is
#' $$
#' \rho(s, t) = \dfrac{\gamma(s, t)}{\sqrt{\gamma(s, s)\,\gamma(t, t)}},
#' $$
#' where $\gamma(s, t) = \operatorname{cov}(x_s, x_t)$ is the **autocovariance function.**


#' We can visualize the correlations between $y_t$ and the first lags $y_{t - k}$ 
#' for $k = 1, 2, 3,\ldots$ using **lag plots**:

#+ fig.width=8, fig.height=8
gglagplot(rw_ts, do.lines = FALSE, continuous = FALSE)

#' ## ACF function (AutoCorrelation function) 
#' 
#' Let us plot the ACF for the random walk with drift:
#' 
#+ fig.width=8, fig.height=4
ggAcf(rw_ts)

#' In the ACF plot the vertical bar at Lag $=k$ represents the correlation of $y_t$ 
#' with the $k$-lagged time series$y_{t - k}$. To get the numerical values we can use:
#' 

acf(rw_ts, lag.max = 10, plot = FALSE)

#' The dotted horizontal blue lines in the ACF are a visual help about a hypothesis 
#' test about the significance of the autocorrelation values. If the autocorrelation 
#' extends beyond the blue line, it is considered significant. In this case the 
#' pattern of very slow decay in the ACF is a clear indicator of the **non-stationary**
#' character of this process. In fact, even the mean depends on $t$:
#' $$\mu_{rw} = k\cdot t$$
#' and even if the drift is 0, the autocovariance $\gamma(s, t)$  always depends on $(s, t)$.
#' 
#' The situation for the random walk is in sharp contrast with the ACF of the white noise:
#' 

#+ fig.width=12, fig.height=4
ggAcf(w)
acf(w, lag.max = 10, plot = FALSE)

#' 
#' And for the autocovariance of a white noise process $w_t$ we get
#' $$\gamma_{w}(s, t) = 
#' \begin{cases}
#' \sigma_{w}^2& s = t\\[3mm]
#' 0 & s \neq t
#' \end{cases},
#' $$
#' where $\sigma_{w}$ is the standard deviation of $w_t$. The autocorrelation is 
#' therefore 1 for $s = t$ and 0 otherwise.


#' For a more mathematically inclined (and R based) introduction to these topics 
#' we recommend Chapters 1 and 2 of @shumwaystoffer2017

#' 
#' # References
#' 
