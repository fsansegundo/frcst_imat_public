#' ---
#' title: "Lab. 1 Decomposition, additional notes."
#' author: "Análisis y Predicción de Series Temporales, 3º IMAT"
#' date: "2025-09-03"
#' output: 
#'  html_document:
#'    toc: true
#' ---


#' # Preliminaries
#' 
#' ## Load libraries
#' 
#' If you get an error go to the packages panel, click install and 
#' type the name of the library (you only need to do this once).

#+ warning=FALSE, message=FALSE
library(fpp2) 
library(tidyverse)

#' ## Set working directory 

#+ eval = FALSE
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#' # Reading time series data from csv files

#' ## Load dataset 

fdata <- read_csv("Unemployment.dat")

head(fdata)

glimpse(fdata)


#' #### QUESTION: 
#' What is the frequency of observation for this data.
#' Is this data yearly? (or quarterly, monthly, weekly, daily,...)
#' <hr>

#' 
#' # Working with dates in R

#' ## Convert character columns to R date type

fdata$DATE <- as.Date(fdata$DATE, format = "%d/%m/%Y")
head(fdata)

#' Order the table by date (using the pipe operator)

fdata <- fdata %>% arrange(DATE)

# same as: fdata <- arrange(fdata,DATE)

#' ## Check for missing dates (time gaps)
#' 
#' How do we know if there are time gaps in the data?

range(fdata$DATE)
min(fdata$DATE)
max(fdata$DATE)

#' Therefore we can create a complete sequence of months with the same range and
#' compare it to the dates in our data.

date_range <- seq.Date(min(fdata$DATE), max(fdata$DATE), by = "months")

head(date_range)
tail(date_range)

#' Now we do the comparison

date_range[!date_range %in% fdata$DATE] 

#' To practice this, let us create a new dataset missing some dates.
#' 

fdata2 <- fdata[c(1:4, 8:nrow(fdata)), ]
head(fdata2)

#' Now if you repeat the comparison 
date_range[!(date_range %in% fdata2$DATE)] 

#' If you wish to add the missing dates as new rows (with missing values) do

missing_dates_df <- data.frame(DATE = date_range[!(date_range %in% fdata2$DATE)],
                            TOTAL = NA) 

fdata2 <- rbind(fdata2, missing_dates_df)
fdata2 <- fdata2[order(fdata2$DATE), ]
head(fdata2, 12)

#' ## Check for missing data (NA) in time series value columns
#' 

sum(is.na(fdata2$TOTAL))


#' ## Imputing the missing data (NEW)
#' 

library(imputeTS)
fdata2_ts <- ts(fdata2$TOTAL, start = c(2010, 1), frequency = 12)
ggplot_na_distribution(fdata2_ts)


fdata2_ts_imp_seas <- na_seasplit(fdata2_ts)
ggplot_na_imputations(fdata2_ts, fdata2_ts_imp_seas)


#' # The `ts` object for time series 
#' 
#' We will now convert the table to a time series object. We need to provide the 
#' column (or columns) containing the values and we also need to describe the time
#' indexing. To do this we provide the starting date and the number of observations
#' per unit of time. The starting date is provided as a pair (year, month), or (year, quarter), 
#' 
#' **The single most important question is: what is the frequency of your data?**
#' see [Hyndman FPP, *The seasonal period* in Section 2.1 ](https://otexts.com/fpp3/tsibbles.html#the-seasonal-period)

# start -> year and month
# frequency = 12 -> monthly data
# frequency = 4 -> quarterly data

#' Now we create the ts object and display the first and last values
y <- ts(fdata$TOTAL, start = c(2010, 1), frequency = 12)
head(y, 30)
tail(y, 12)


#' # Time Plots 
#' 

#' The most basic and useful plot for time series is the time plot:  

#+ fig.width=12, fig.height=4
autoplot(y) +
  ggtitle("Unemployment in Spain") +
  xlab("Year") + ylab("Number unemployed")

#' or using the R base plotting system:

#+ fig.width=12, fig.height=4
plot.ts(y, 
        main="Unemployment in Spain",
        xlab="Year",
        ylab="Number unemployed")

#' # Subsetting time series with `window`

# Select time series time frame
y2 <- y
y <- window(y, start = c(2010,1), end = c(2019,12))

# or directly from the original data:
y <- ts(fdata$TOTAL, start = c(2010,1), end = c(2019,12), frequency = 12)

#' Now we plot the time series 

#+ fig.width=12, fig.height=4
autoplot(y2, color = "blue") +
  ggtitle("Unemployment in Spain") +
  xlab("Year") + ylab("Number unemployed") + 
  autolayer(y, color = "red")


#' # Decomposition methods

#' ## Classical additive decomposition
#' 

y_dec_add <- decompose(y, type="additive")

#' The decomposition object is a list that contains (among other things) the components.
#' To get e.g. the seasonal component:

head(y_dec_add$seasonal, 20)

#' Let us visualize the decomposition

#+ fig.width=12, fig.height=8
autoplot(y_dec_add) + xlab("Year") +
  ggtitle("Classical additive decomposition")


#' ## Classical Multiplicative decomposition
#' 
#' Similarly
#' 
y_dec_mult <- decompose(y, type="multiplicative")

#+ fig.width=12, fig.height=8
autoplot(y_dec_mult) + xlab("Year") +
  ggtitle("Classical multiplicative decomposition")


#' ## SEATS decomposition method
#' 
#' 

library(seasonal)

#' We obtain the decomposition as follows:
y_dec_seas <- seas(y)

#' In this case the components are stored in a mts (multi time series) object
head(y_dec_seas$data)

#' But we can use `seasonal()`, `trendcycle()` and `remainder()` functions to extract 
#' the individual components. With `seasadj()` we can compute the seasonally adjusted
#'  time series.


seasonal(y_dec_seas)

#+ fig.width=12, fig.height=8
autoplot(y_dec_seas) + xlab("Year") +
  ggtitle("SEATS decomposition")


#' ### Compare the seasonal components for different decomposition methods

#+ fig.width=12, fig.height=3
autoplot(seasonal(y_dec_mult), series = "Multiplicative") +
  forecast::autolayer(seasonal(y_dec_seas), series = "SEATS")


#' ### Compare seasonal adjustment components (i.e. subtracting the seasonal component from the raw series)

#+ fig.width=12, fig.height=3
autoplot(seasadj(y_dec_add), series = "Additive") +
  forecast::autolayer(seasadj(y_dec_mult), series = "Multiplicative") +
  forecast::autolayer(seasadj(y_dec_seas),series = "SEATS")

autoplot(seasadj(y_dec_seas), series = "SEATS")


#' ## Seasonal subseries plots
#' 
#' Can be obtained from the decompositions as follows:
#' 

#+ fig.width=12, fig.height=3
ggsubseriesplot(seasonal(y_dec_add)) 

#+ fig.width=12, fig.height=3
ggsubseriesplot(seasonal(y_dec_mult)) 

#+ fig.width=12, fig.height=3
ggsubseriesplot(seasonal(y_dec_seas)) 

#' ### Seasonal plot with `ggseasoonplot`

#+ fig.width=12, fig.height=6
ggseasonplot(y, year.labels=TRUE,continuous=TRUE)

#' # Additional examples and other time series and libraries

library(TSstudio)

#' Let us load this dataset (run `?US_indicators` after loading the data)

data(US_indicators)

#' Examine the data

glimpse(US_indicators)
str(US_indicators)

head(US_indicators)
tail(US_indicators)

#' Rename variables and order dates

US_indicators <- US_indicators %>% 
  rename( VehicleSales = 'Vehicle Sales', UnemploymentRate= `Unemployment Rate`) %>% 
  arrange(Date)

head(US_indicators)
tail(US_indicators)

#' Check for complete dates and data
#' 
#' We will try to use the same strategy, but have you noticed any difference with the 
#' previous case? Look at the dates.

lubridate::day(US_indicators$Date) %>% head(20) 

#' This complicates the strategy of creating a complete date sequence and comparing it 
#' with the dates in our data. Since this is monthly data and month is all we care about, 
#' we replace all dates with the first day of the month (run `?lubridate::day`).

lubridate::day(US_indicators$Date) <- 1
head(US_indicators)

#' Now we proceed as before

date_range <- seq.Date(min(US_indicators$Date), max(US_indicators$Date), by = "months")
head(date_range)
tail(date_range)

date_range[!date_range %in% US_indicators$Date] 

#' Check for missing data in the value columns

sum(is.na(US_indicators$VehicleSales))
sum(is.na(US_indicators$UnemploymentRate))

#' Alternatively

all(complete.cases(US_indicators))

#' ## One dimensional time series, time plot with TSstudio

tvs <- US_indicators[, c("Date", "VehicleSales")]
str(tvs)

#' We create the ts object as we did before:

library(lubridate)
start_point <- c(year(min(tvs$Date)), month(min(tvs$Date)))
start_point
tvs_ts <- ts(data = tvs$'VehicleSales',
             start = start_point,
             frequency = 12)

#' and we do a basic time plot

#+ fig.width=12, fig.height=3
plot.ts(tvs_ts,
        main = "US Monthly Total Vehicle Sales",
        ylab = "Thousands of Vehicle",
        xlab = "Time"
)


#' ##  Multiple time series object (mts)
#'
#' We create it in a similar way:

US_indicators_ts <- ts(data = US_indicators[, c("VehicleSales",
                                                "UnemploymentRate")],
                       start = c(year(min(tvs$Date)),
                                 month(min(tvs$Date))),
                       frequency = 12)
str(US_indicators_ts)

#' Change plot type to `single` and see what happens. When would you use one 
#' or the other?

#+ fig.width=12, fig.height=6
plot.ts(US_indicators_ts,
        plot.type = "multiple",
        main = "US Monthly Vehicle Sales vs. Unemployment Rate",
        xlab = "Time")


#' ## Plots with library TSstudio

#+ fig.width=10, fig.height=4
ts_plot(tvs_ts,
        title = "US Monthly Total Vehicle Sales",
        Ytitle = "Thousands of Vehicle",
        slider = TRUE
)

#+ fig.width=10, fig.height=6
ts_plot(US_indicators_ts,
        title = "US Monthly Vehicle Sales vs. Unemployment Rate",
        type = "multiple")

#' # Moving Averages
#' 


head(US_indicators)

tvs_data <- US_indicators[,-3]

head(tvs_data)

tvs_data$MA5 <- slider::slide_dbl(tvs$VehicleSales, mean,
                             .before = 2, .after = 2, .complete = TRUE)

head(tvs_data, 15)
tail(tvs_data)

tvs_MA_ts <- ts(data = tvs_data[,-1],
              start = c(year(min(tvs$Date)),
                        month(min(tvs$Date))),
              frequency = 12)
  
autoplot(tvs_MA_ts)


tvs_data$MA12 <- slider::slide_dbl(tvs$VehicleSales, mean,
                               .before = 5, .after = 6, .complete = TRUE)

tvs_data$MA2x12 <- slider::slide_dbl(tvs_data$MA12, mean,
                                    .before = 1, .after = 0, .complete = TRUE)


head(tvs_data)

tvs_MA_ts <- ts(data = tvs_data[,-1],
                start = c(year(min(tvs$Date)),
                          month(min(tvs$Date))),
                frequency = 12)

head(tvs_MA_ts)

autoplot(tvs_MA_ts[ , 4], color = "yellow", size = 3, alpha = 0.5) + 
  autolayer(decompose(tvs_ts, type="additive")$trend)
