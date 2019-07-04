"
Modified from
Ana Vicedo-Cabrera, Francesco Sera, Antonio Gasparrini, London School of Hygiene and Tropical Medicine
CAR Workshop Advanced modelling techniques for time series analysis using R. Woolcock Institute, Sydney - 05 April 2019. http://www.ag-myresearch.com/tscourse.html
Practical 1 - Time series models (./notes&Rscripts/01TS_shortterm.R)

by ivanhanigan
Modified to work with the chicagoNMMAPS example and temperature
with some additions of interest
"
## first off I don't want all these 
## install.packages(c("tsModel","Epi","splines","lubridate"))
##library(tsModel) ; library("Epi") ; library("splines") ; library("lubridate")

## just a couple, check if installed, installl if not
if(!require(dlnm)) install.packages("dlnm")
if(!require(splines)) install.packages("splines")
if(!require(lubridate)) install.packages("lubridate")

library("dlnm")
library("splines") 
library("lubridate")

## we want to use the chicago data not london
data(chicagoNMMAPS)
## data <- read.csv("data/london.csv")
data <- chicagoNMMAPS

data$date <- as.Date(data$date, format="%d/%m/%Y")

head(data, 3)

summary(data)

data$year <- year(data$date)
data$month <- month(data$date)
data$day <- day(data$date)
data$yday <- yday(data$date)
data$dow <- wday(data$date, label=T)

## I like to create the figures as PNG files, and put them into a
## folder called figs so create that
dir.create("figs")
png("figs/tsplot-deaths-temp.png", width = 1000, height = 650, res = 100)
layout(1:2)
par(mex=0.7)
plot(data$date,data$death,ylab="Deaths",xlab="Date",cex=0.6,col=2,
 main="Time series of daily deaths")
## NB I changed from CO to temp
plot(data$date,data$temp,ylab="Temperature",xlab="Date",cex=0.6,col=4,
 main="Time series of daily temp level")
par(mex=1)
layout(1)
dev.off()

options(na.action=na.exclude)

mnone <- glm(death ~ temp, data, family=poisson)

summary(mnone)
