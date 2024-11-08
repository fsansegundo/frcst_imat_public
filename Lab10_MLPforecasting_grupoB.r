#' ---
#' title: "Lab 10: Nonlinear forecasting:  MLP for nonlinear forecasting"
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
##############      Lab 10: Nonlinear forecasting    ############################
##############      MLP for nonlinear forecasting    ############################
##############     ----------- solution ---------    ############################
#################################################################################
library(MLTools)
library(fpp2)
library(lmtest)
library(tseries) #contains adf.test function
library(TSA)
library(NeuralSens)
library(caret)
library(kernlab)
library(nnet)
library(NeuralNetTools)

## Set working directory ---------------------------------------------------------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Lectura datos -------------------------------------------------------------------------------------------------
fdata <- readxl::read_excel("DAILY_DEMAND_TR.xlsx")
fdata <- as.data.frame(fdata)


#Initialize output and input variables
fdata.Reg <- fdata[,c(2,3,4)] 



###Include lagged variables
#This can be done using the lag() function from the stats package but it works with time series objects
#The code is cleaner using the Lag() function from Hmisc package
library(Hmisc)
fdata.Reg$WD_lag1 <- Lag(fdata$WD,1)
fdata.Reg$WD_lag7 <- Lag(fdata$WD,7)
fdata.Reg$TEMP_lag1 <- Lag(fdata$TEMP,1)
fdata.Reg$TEMP_lag7 <- Lag(fdata$TEMP,7)
fdata.Reg$DEM_lag1 <- Lag(fdata$DEM,1)
fdata.Reg$DEM_lag7 <- Lag(fdata$DEM,7)


#Notice that the begining of the time series contains NA due to the new lagged series
head(fdata.Reg)

fdata.Reg.tr <- fdata.Reg
#Remove missing values
fdata.Reg.tr <- na.omit(fdata.Reg.tr)

## Initialize trainControl -----------------------------------------------------------------------
#Use resampling for measuring generalization error
#K-fold with 10 folds
ctrl_tune <- trainControl(method = "cv",                     
                          number = 10,
                          summaryFunction = defaultSummary,    #Performance summary for comparing models in hold-out samples.
                          savePredictions = TRUE)              #save predictions


#------------ Neural network --------------------------------
set.seed(150) #For replication
mlp.fit = train(form = DEM~., #Use formula method to account for categorical variables
                data = fdata.Reg.tr, 
                method = "nnet",
                linout = TRUE,
                # tuneGrid = data.frame(size =5, decay = 0),
                tuneGrid = expand.grid(size = seq(10,20,length.out =3), decay =  10^(c(-3:0))),
                maxit = 200,
                preProcess = c("center","scale"),
                trControl = ctrl_tune, 
                metric = "RMSE",
                trace=FALSE)
mlp.fit #information about the resampling settings
ggplot(mlp.fit)+scale_x_log10()
plotnet(mlp.fit$finalModel) #Plot the network
SensAnalysisMLP(mlp.fit) #Statistical sensitivity analysis


#Predict training data
mlp_pred = predict(mlp.fit,  newdata = fdata.Reg.tr)  

PlotModelDiagnosis(fdata.Reg.tr[,-1], fdata.Reg.tr[,1], mlp_pred, together = TRUE)

#Error measurements
accuracy(fdata.Reg.tr[,1],mlp_pred)

plot(fdata.Reg.tr[,1],type="l")
lines(mlp_pred,col = "red")



#################################################################################
## Forecast for new data with h = 7
#################################################################################
#------------------READ TV FILE --------------------------
fdata.Reg.tv <-  readxl::read_excel("DAILY_DEMAND_TV.xlsx")
fdata.Reg.tv <- as.data.frame(fdata.Reg.tv)

#----------------------------------------------------

fdata.Reg.new <- fdata.Reg.tv[,c(2,3)]

#create auxiliary output variable
fdata.Reg.new$DEM <- rep(NA,length(fdata.Reg.new[,1]))


#join the datasets
fdata.join <- rbind(fdata[,c(2:4)],fdata.Reg.new)

#create lagged variables
fdata.join$WD_lag1 <- Lag(fdata.join$WD,1)
fdata.join$WD_lag7 <- Lag(fdata.join$WD,7)
fdata.join$TEMP_lag1 <- Lag(fdata.join$TEMP,1)
fdata.join$TEMP_lag7 <- Lag(fdata.join$TEMP,7)
fdata.join$DEM_lag1 <- Lag(fdata.join$DEM,1)
fdata.join$DEM_lag7 <- Lag(fdata.join$DEM,7)

head(fdata.join, 12)

fdata.join <- fdata.join[-(1:nrow(fdata.Reg.tv)), ]

#loop for forecasting
#tstart = dim(fdata)[1]
tstart = nrow(fdata.join) - nrow(fdata.Reg.new)
for (i in (tstart+1):(tstart+nrow(fdata.Reg.tv))){
  #Predict and substitute in output variable so it can be used in following forecasts
  fdata.join$DEM[i] <- predict(mlp.fit,  newdata = fdata.join[i,]) 
  #Recalculate lagged variables using the estimated value
  fdata.join$DEM_lag1 <- Lag(fdata.join$DEM,1)
  fdata.join$DEM_lag7 <- Lag(fdata.join$DEM,7)
}

#forecasts
fdata.join$DEM[(tstart+1):(tstart+nrow(fdata.Reg.tv))]




