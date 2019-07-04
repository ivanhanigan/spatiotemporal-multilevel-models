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

## we want to use the chicago heatwave data 
data(chicagoNMMAPS)
## described in the related help page
help(chicagoNMMAPS)
## and the vignette dlnmOverview
str(chicagoNMMAPS)

## data <- read.csv("data/london.csv")
data <- chicagoNMMAPS
head(data, 3)
summary(data)
data$date <- as.Date(data$date, format="%d/%m/%Y")
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

## the heatwave was in 1995
png("figs/tsplot-deaths-temp-95.png", width = 1000, height = 650, res = 100)
layout(1:2)
par(mex=0.7)
qc <- data[data$date >= as.Date("1995-01-01") & data$date < as.Date("1996-01-01") ,]
with(qc, plot(date,death,ylab="Deaths",xlab="Date",cex=0.6,col=2, main="Time series of daily deaths", type = "l"))
with(qc, plot(date,temp,ylab="Temperature",xlab="Date",cex=0.6,col=4,
 main="Time series of daily temp level", type = "l"))
par(mex=1)
layout(1)
dev.off()



## the smooth
options(na.action=na.exclude)
## permanently resetting options(na.action=na.exclude). The default is
## na.omit, which omits the observations related to missing data from
## predictions or residuals. With na.exclude, these are instead
## included as missing (easier for plotting, among other issues).

png("figs/tsplot-deaths-temp-smooth.png", width = 1000, height = 650, res = 100)
with(data, plot(temp,death,ylab="Deaths",pch = 16, cex=0.6, col='darkgrey'))
with(data, lines(lowess(death ~ temp, f = 0.2), col = 'red', lwd = 1.5))
dev.off()

## now we can follow the TSCOURSE codes, I changed co to temp
## also the original authors use Epi::ci.exp a lot, but it caused
## issues to install, so just ignore those lines

## mnone <- glm(death ~ co, data, family=poisson)
mnone <- glm(death ~ temp, data, family=poisson)

summary(mnone)
## this needs a bit of work to interpret as they are on the log scale
## the authors of TSCOURSE used the Epi package, but I couldn't
## install easily, so I use a simple function
## Instead of 
## ci.exp(mnone, subset="temp")
## ci.exp just gets the exponentiated coefficient and 95%CIs
ci.hack <- function(fit, subset = NA){
    x <- summary(fit)$coeff
    nms <- rownames(x)
    beta <- x[which(nms %in% subset),1]
    se <- x[which(nms %in% subset),2]
    est <- exp(beta)
    x2p5 <- exp(beta) - (1.96 * se)
    x97p5 <- exp(beta) + (1.96 * se)
    estat <- data.frame(est, x2p5, x97p5)
    return(estat)
}

ci.hack(mnone, subset = "temp")
## BORING, but we know other things are going on


## a linear trend
mlin <- glm(death ~ date, data, family=poisson)
## add a strata term for calendar month
mmonth <- glm(death ~ date + factor(month), data, family=poisson)
## relax assumption about regular seasonal pattern
mmonthinyear <- glm(death ~ factor(month)*factor(year), data, family=poisson)
## allow smooth temporal trends
spltime <- bs(data$date, df=7*8)
mspline <- glm(death ~ spltime, data, family=poisson)

png("figs/tsplot-deaths-temp-seas.png", width = 1000, height = 650, res = 100)
plot(data$date,data$death,ylab="Deaths",xlab="date",cex=0.6,col=grey(0.8),
 main="Modelling seasonality and trend (truncated Y)", ylim = c(80,170))
lines(data$date,predict(mlin, type="response"),col="green")
lines(data$date,predict(mmonth, type="response"),col="blue")
lines(data$date,predict(mmonthinyear, type="response"),col="orange")
lines(data$date,predict(mspline, type="response"),col="red")
legend("topright",c("Linear time","Month indicators and linear time","Month*year term",
  "Spline function of time"),col=c("green","blue","orange","red"),lty=1,cex=0.8,bty="n")
dev.off()

## Now update the previous models including temp as a linear term
mlin <- update(mlin, .~ . + temp)
mmonth <- update(mmonth, .~ . + temp)
mmonthinyear <- update(mmonthinyear, .~ . + temp)
mspline <- update(mspline, .~ . + temp)

## replace ci.exp with our own hack
estat <- data.frame(model = "simplest", ci.hack(mnone, subset="temp"))
estat <- rbind(estat, data.frame(model = "linear", ci.hack(mlin, subset="temp")))
estat <- rbind(estat, data.frame(model = "months", ci.hack(mmonth, subset="temp")))
estat <- rbind(estat, data.frame(model = "months+year", ci.hack(mmonthinyear,subset="temp")))
estat <- rbind(estat, data.frame(model = "spline", ci.hack(mspline, subset="temp")))

## if you like install knitr for the kable function
## install.packages("knitr")
## knitr::kable(estat, digits = 4)

## or perhaps a plot
png("figs/tsreg-deaths-temp-seas.png", width = 1000, height = 650, res = 100)
with(estat, plot(1:nrow(estat), est, ylim = c(0.995,1.002), xlab = "", axes = F))
with(estat, segments(1:nrow(estat), x2p5, 1:nrow(estat), x97p5))
abline(1,0)
axis(2)
box()
mtext(estat$model, 1, at = 1:nrow(estat))
dev.off()

## Model selection

## LR test
anova(mmonth, mmonthinyear, test="LRT")
## suggests that model 2 (month*year) is preferred

## Akaike Information Criterion
AIC(mnone, mlin, mmonth, mmonthinyear, mspline)
## suggests that model 2 (month*year) is preferred

## Original authors extended the CO analysis by adding Temp, we could
## add something, like PM10, but for now I just skip this all
## plot(data$tmean, data$death, ylab="Deaths", xlab="Temperature", cex=0.6,
##  col=grey(0.8), main="Temperature-mortality scatter plot") 
## lw <- loess(death ~ tmean, data)
## pred <- predict(lw, data.frame(tmean=-2:37))
## lines(-2:37, pred, col=2)

## spltmean <- ns(data$tmean, df=3)
## mtmean <- glm(death ~ temp + spltime + dow + spltmean, data, family=poisson)  

## ci.exp(mtmean, subset="temp")
## AIC(mspline, mtmean)

## drop1(mtmean, test="LRT")



overdisp <- update(mtmean, family=quasipoisson)
summary(overdisp)$dispersion
ci.exp(mtmean, subset="temp")
ci.exp(overdisp, subset="temp")

layout(1:2)
pacf(data$death, ylim=c(-0.1,1), main="Original response variable")
pacf(residuals(mtmean), ylim=c(-0.1,1), na.action=na.pass, main="Residuals from the regression model")
layout(1)

dataseas <- subset(data, month%in%6:9)

splyday <- ns(dataseas$yday, df=4)
spltimelong <- bs(dataseas$date, df=2*8)
mseas <- glm(death ~ temp + splyday + spltimelong + ns(tmean, df=3), dataseas,
  family=poisson)
ci.exp(mseas, subset="temp")

mseas2 <- glm(death ~ temp + splyday*factor(year) + ns(tmean, df=3), dataseas,
  family=poisson)
ci.exp(mseas2, subset="temp")

#### Excess Heat Factor ####

if(!require(ExcessHeatIndices)){devtools::install_github("swish-climate-impact-assessment/ExcessHeatIndices");require(ExcessHeatIndices)}
require(ExcessHeatIndices)
summary(data$date)

data$EHF <- EHF(ta = data$temp)
data$EHFduration <- EHFduration(data$EHF)
data$EHFload <- EHFload(data$EHF, data$EHFduration)

data[data$date >= as.Date("1995-07-01") &
     data$date < as.Date("1995-07-30"),]
