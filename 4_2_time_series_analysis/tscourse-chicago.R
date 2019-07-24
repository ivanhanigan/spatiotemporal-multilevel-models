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
tail(data, 6)
data[1,1:4]
data[1:25,"yday"]
data$yday[1:25]

summary(data)
data$date <- as.Date(data$date, format="%d/%m/%Y")
data$year <- year(data$date)
data$month <- month(data$date)
data$day <- day(data$date)
data$yday <- yday(data$date)
data$dow <- wday(data$date, label=T)
str(data)
summary(data$yday)

## I like to create the figures as PNG files, and put them into a
## folder called figs so create that
dir.create("figs")
png("figs/tsplot-deaths-temp.png", width = 1000, height = 650, res = 100)
layout(1:2)
par(mex=0.7)
plot(data$date,data$death,ylab="Deaths",xlab="Date",cex=0.6,col=2,
 main="Time series of daily deaths", type = "l")
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
str(mnone)
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
## NOTE that it is almost tradition to use 7df per year
ny <- length(unique(data$year))
ny
spltime <- bs(data$date, df=7*ny)
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

## as a brief diversion, this is how to fit a penalised spline in GAM
## using cross validation to determine best curve for temp
## we will fix the smooth on time
library(mgcv)
mgam <- gam(death ~ s(temp) + s(time, fx = T, k=7*ny), data, family=poisson)
summary(mgam)
png("figs/tsreg-deaths-temp-gam.png", height = 300, width = 1000, res = 100)
par(mfrow = c(1,2), mar = c(4,4,1,1))
plot(mgam, all.terms = T)
dev.off()

## back to TSCOURSE
#### Model selection ####

## LR test
anova(mmonth, mmonthinyear, test="LRT")
## suggests that model 2 (month*year) is preferred

## Akaike Information Criterion
AIC(mnone, mlin, mmonth, mmonthinyear, mspline, mgam)
## suggests that non-linear term is strongly preferred

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

#### Model diagnostics ####
## Original authors checked for overdispersion, we will skip this
## overdisp <- update(mtmean, family=quasipoisson)
## summary(overdisp)$dispersion
## ci.exp(mtmean, subset="temp")
## ci.exp(overdisp, subset="temp")

## layout(1:2)
## pacf(data$death, ylim=c(-0.1,1), main="Original response variable")
## pacf(residuals(mtmean), ylim=c(-0.1,1), na.action=na.pass, main="Residuals from the regression model")
## layout(1)

## dataseas <- subset(data, month%in%6:9)

## splyday <- ns(dataseas$yday, df=4)
## spltimelong <- bs(dataseas$date, df=2*8)
## mseas <- glm(death ~ temp + splyday + spltimelong + ns(tmean, df=3), dataseas,
##   family=poisson)
## ci.exp(mseas, subset="temp")

## mseas2 <- glm(death ~ temp + splyday*factor(year) + ns(tmean, df=3), dataseas,
##   family=poisson)
## ci.exp(mseas2, subset="temp")

#### Excess Heat Factor ####
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(ExcessHeatIndices)){devtools::install_github("swish-climate-impact-assessment/ExcessHeatIndices");require(ExcessHeatIndices)}
require(ExcessHeatIndices)
summary(data$date)
## by default the frequency distribution is taken from the full time period
data$EHF <- EHF(ta = data$temp)
data$EHFduration <- EHFduration(data$EHF)
data$EHFload <- EHFload(data$EHF, data$EHFduration)

## for a plot we need the interim steps too
data$t3 <- zoo::rollapplyr(data$temp, width = 3, FUN = mean, fill = NA)
data$t30 <- zoo::rollapplyr(c(rep(NA, 3), data$temp[1:(length(data$temp)-3)]), width = 30, FUN = mean, fill = NA)
t95 <- quantile(data$temp, 0.95, na.rm = T)
t99 <- quantile(data$temp, 0.99, na.rm = T)
data$EHIsig <- data$t3 - t95
data$EHIaccl <- data$t3 - data$t30
## EHF <- EHIsig * pmax(1, EHIaccl)


qc <- data[data$date >= as.Date("1995-06-01") & data$date < as.Date("1995-07-30"),]


png("figs/tsplot-ehf-demo.png", width = 1300, height = 690, res = 100)
par(mfrow = c(3,1), mar = c(4,4,3,1))
with(qc,
     plot(date, temp, type = 'b', lty = 2, main = "", ylim = c(10,40))
)

with(qc,
     lines(date, t3, type = 'l')
     )
with(qc,
     lines(date, t30, type = 'l', lwd = 1.5)
)
abline(t95, 0, col = 'red')
abline(t99, 0, col = 'red', lwd = 2, lty = 2)
legend("topright", legend = c("avtemp", "3movav", "30movav", "T95th"), lty = c(2,1,1,1), lwd = c(1,1,2,1), col = c(1,1,1, 'red'), pch = c(1,NA,NA,NA), bg="white", cex = .8)

with(qc,
     plot(date, EHIaccl, type = 'h',  main = "EHI acclimatisation (3movav - 30movav)", ylim = c(-12,12), lwd = 4, col = 'grey')
     )
with(qc[qc$EHIaccl > 0,],
     lines(date, EHIaccl, type = 'h', lwd = 4, col = 'black')
     )
abline(0,0)

with(qc,
     plot(date, EHIsig,type = 'h', main = "EHI significant (3movav - T95th)", ylim = c(-12,12), lwd = 4, col = 'grey')
     )
with(qc[qc$EHIsig > 0,],
     lines(date, EHIsig, type = 'h', lwd = 4, col = 'black')
     )
abline(0,0)
dev.off()

png("figs/tsplot-ehf-demo-duration.png", width = 1300, height = 690, res = 100)
par(mfrow = c(3,1), mar = c(4,4,3,1))
with(qc,
     plot(date, EHF,type = 'l', main = "EHF (EHIsig x max(1,EHIaccl))", ylim = c(-30,60), lwd = 2, col = 'grey')
     )
with(qc[qc$EHF > 0,],
     points(date, EHF, pch = 16, lwd = 2, col = 'black')
     )
abline(0,0)

with(qc,
     plot(date, EHFduration,type = 'l', main = "EHF duration (consecutive days with EHF > 0)")#, ylim = c(10,50))
)

with(qc,
     plot(date, EHFload,type = 'l', main = "EHF load (cumulative sum during periods with EHF > 0)")#, ylim = c(10,50))
)

dev.off()

#### EHF indicator variable ####
data$heatwave <- as.factor(ifelse(data$EHFduration > 0, 1, 0))
mgamehf <- glm(death ~ heatwave + ns(temp, df=4) + ns(time, df=7*ny), data, family=poisson)
summary(mgamehf)
estat_hwave <- ci.hack(mgamehf, subset = "heatwave1")
knitr::kable(estat_hwave, digits = 3)

png("figs/tsreg-deaths-temp-glm-heatwave.png", height = 400, width = 1000, res = 100)
par(mfrow = c(2,2), mar = c(4,4,1,1))
termplot(mgamehf, se = T, terms = 1, col.term = "black", col.se = "black")
termplot(mgamehf, se = T, terms = 2, col.term = "black", col.se = "black")
termplot(mgamehf, se = T, terms = 3, col.term = "black", col.se = "black")
dev.off()

### 3 days or more abnove 95 
data$heatwave2 <- as.factor(ifelse(data$EHFduration >= 3, 1, 0))
mgamehf <- glm(death ~ heatwave2 + ns(temp, df=4) + ns(time, df=7*ny), data, family=poisson)
summary(mgamehf)
#estat_hwave <- ci.hack(mgamehf, subset = "heatwave1")
#knitr::kable(estat_hwave, digits = 3)

# png("figs/tsreg-deaths-temp-glm-heatwave.png", height = 400, width = 1000, res = 100)
# par(mfrow = c(2,2), mar = c(4,4,1,1))
termplot(mgamehf, se = T, terms = 1, col.term = "black", col.se = "black")
termplot(mgamehf, se = T, terms = 2, col.term = "black", col.se = "black")
# termplot(mgamehf, se = T, terms = 3, col.term = "black", col.se = "black")
# dev.off()

### duration as a term
mgamehf <- glm(death ~ EHFduration + ns(temp, df=4) + ns(time, df=7*ny), data, family=poisson)
summary(mgamehf)
termplot(mgamehf, se = T, terms = 1, col.term = "black", col.se = "black")
termplot(mgamehf, se = T, terms = 2, col.term = "black", col.se = "black")

mgamehf <- gam(death ~ s(EHFload, fx = T, k = 6) + ns(temp, df=4) + ns(time, df=7*ny), data, family=poisson)
#mgamehf <- glm(death ~ ns(EHFload, df = 9) + ns(temp, df=4) + ns(time, df=7*ny), data, family=poisson)
summary(mgamehf)
plot(mgamehf, se = T, terms = 1, col.term = "black", col.se = "black")
##termplot(mgamehf, se = T, terms = 1, col.term = "black", col.se = "black")
termplot(mgamehf, se = T, terms = 2, col.term = "black", col.se = "black")

