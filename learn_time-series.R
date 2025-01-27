# Script: Time series;

# Instructions:  Paste each block of code into R, one at a time, to see what it does;

# Load needed packages and data;

# Install package changepoint if it is not already installed;

if (!require(changepoint)) install.packages('changepoint'); 

# Load package changepoint into your current R session;

library(changepoint);

# Install package forecast if it is not already installed;

if (!require(forecast)) install.packages('forecast'); 

# Load package forecast into your current R session;

library(forecast);

# Install package ggplot2 if it is not already installed;

if (!require(ggplot2)) install.packages('ggplot2'); 

# Install package diffusion if it is not already installed;

if (!require(diffusion)) install.packages('diffusion'); 

# Load the AirPassengers dataset into the current R session; 

data(AirPassengers);

# View Air Passengers time series;

# Plot first 36 months of AirPassengers time series;

op <- par(cex.axis = 1.6, cex.lab = 1.6);

plot(AirPassengers[1:36], xlab = "time", ylab = "Sales");

lines(AirPassengers[1:36]);

# Create subset of first 36 months of time series and name it “Sales”;

#Create “Sales” data;

Sales <-AirPassengers[1:36];

# Changepoint analysis;

# Use a Kolmogorov-Smirnov test to test the null hypothesis that monthly sales have the same distribution before and after the end of month 16;

ks.test(Sales[1:16], Sales[17:36]);

# Use a two-sample t-test to test the null hypothesis that monthly sales have the same mean before and after the end of month 16, assuming that they are independently normally distributed;

t.test(Sales[1:16], Sales[17:36]);

# Perform automated search for changepoint assuming at most one changepoint (AMOC);

cpt.mean(Sales, method = "AMOC");

# Decompose time series and visualize components;

# Decompose time series;

tsdata <- ts(Sales, frequency = 12);

# Plot components;

plot(decompose(tsdata), xlab = "Years");

# Time series forecasting;

# Fit Holt-Winters model and name it “Holt1”;

Holt1 <- HoltWinters(AirPassengers);

# Fit ARIMA model and name it “ARIMA1”;

ARIMA1 <- auto.arima(AirPassengers);

# Show quick plot of forecast using Holt1;

autoplot(forecast(Holt1));

# Show better-looking plot;

autoplot(forecast(Holt1)) + theme(axis.title.y = element_text(size = rel(2), angle = 90)) + theme(axis.title.x = element_text(size = rel(2), angle = 0))+ theme(axis.text.x = element_text(angle = 0, hjust = 1, size=18,color="darkred")) +  theme(axis.text.y = element_text(angle = 0, hjust = 1, size=18,color="darkred"));

# Zoom in on end of time series and a 36-month forecast;

autoplot(forecast(Holt1), 36) + theme(axis.title.y = element_text(size = rel(2), angle = 90)) + theme(axis.title.x = element_text(size = rel(2), angle = 0))+ theme(axis.text.x = element_text(angle = 0, hjust = 1, size=18,color="darkred")) +  theme(axis.text.y = element_text(angle = 0, hjust = 1, size=18,color="darkred"));

# Display table of forecast values using Holt1;

forecast(Holt1);

# Bass diffusion modeling;

# Install package diffusion if it is not already installed;

if (!require(diffusion)) install.packages('diffusion'); 

# Load diffusion package into current R session;

library(diffusion);

# Fit a Bass diffusion model to IBM sales data and name it “Bass1”;

# The dataset is described here: 
https://www.rdocumentation.org/packages/diffusion/versions/0.2.7/topics/tsIbm
; 

Bass1<- diffusion(tsIbm[, 1]);

# Use model Bass1 to forecast demand for next 10 periods;

forecast10 <- predict(Bass1, 10);

# Show fit of model to data;

op <- par(cex.axis = 1.6, cex.lab = 1.6);

plot(Bass1);

# Show model fit to data and forecast values;

plot(forecast10);

# Create Bass forecast based on only 7 periods of data and compare to observations (black curve);

tsIBM7 <- tsIbm[1:7,];

Bass7<- diffusion(tsIBM7[, 1]);

forecast7<- predict(Bass7, 17);

plot(forecast7);

lines(tsIbm[,1]);

# Create Bass forecast based on 9 periods of data and compare to observations (black curve);

tsIBM9 <- tsIbm[1:9,];

Bass9<- diffusion(tsIBM9[, 1]);

forecast9<- predict(Bass9, 17);

plot(forecast9);

lines(tsIbm[,1]);

# Create Bass diffusion model for second product line in IBM dataset;

BassNew<- diffusion(tsIbm[, 2]);

# Show fit of model to data.  The systematic differences indicate model misspecification.;

op <- par(cex.axis = 1.6, cex.lab = 1.6);

plot(BassNew);
