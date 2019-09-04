---
layout: default
title: Introduction to R stats modelling
---


Download the script developed by ivan [here](./1_intro_to_R/intro_spatiotemporal_multilevel_models.R)


{% highlight r %}
## Intro to R stats modelling ##
# by ivanhanigan
# recall objects
# e.g. numbers
a <- 1
a
# or characters
a <- "abc"
a
# and vectors
x <- 1:100
x
# math functions and simulating from a range of distributions
y <- x^2 + sample(rnorm(10000, 0, 1000), 100)
# and plotting
plot(x, y)

# stats models also create objects

# use the mgcv package shipped with the base installation
library(mgcv)
# with statistical methods such as generalised additive models and
# penalised splines
fit <- gam(y ~ s(x))
# created an object that holds the results of the stats model 
summary(fit)
plot(fit)

# there are many other packages on CRAN we can use tests to see if we
# need to install packages on this computer
if(!require(season)){
  install.packages("season")
}; library(season)
# sometimes with well documented help files
??season

# these can also contain data
data(CVDdaily)
?CVDdaily
# str means structure
str(CVDdaily)
summary(CVDdaily)

# sometimes you specify different plot types
plot(CVDdaily$date, CVDdaily$cvd, type='l')
# other times R knows based on the type of data
plot(CVDdaily$dow, CVDdaily$cvd)

# generalised linear models with non-gausian responses
fit2 <- glm(cvd ~ tmpd + dow, data = CVDdaily, family = "poisson")
summary(fit2)

# partial residual plots are fun
termplot(fit2, terms = 1, se = T)

# and so are parametric smoothing splines
library(splines)
fit3 <- glm(cvd ~ ns(tmpd, df = 4) + dow,
            data = CVDdaily, family = "poisson")
summary(fit3)

# partial residual plots are fun
termplot(fit3, terms = 1, se = T)

# and post estimation statistics 
aic_table <- data.frame(aic2 = AIC(fit2), aic3 = AIC(fit3))
aic_min <- min(aic_table)
(aic_table - aic_min)
# model fit3 is substantially better than fit2

# and diagnostics like cooks distance and leverage
par(mfrow = c(2,2))
plot(fit3)

## let's look at a monthly analysis, and data aggregations
if(!require(sqldf)){install.packages("sqldf")}
library(sqldf)

data(CVDdaily)
?CVDdaily
str(CVDdaily)
summary(CVDdaily)
plot(CVDdaily$date, CVDdaily$cvd, type='l')
CVDdaily$yy <- as.numeric(substr(as.character(CVDdaily$date), 1,4))
head(CVDdaily)
?aggregate
dat <- sqldf("select yy, month, sum(cvd) as cvd, avg(o3mean) as o3
 from CVDdaily
group by yy, month
order by yy, month")
dat$time  <- 1:nrow(dat)
head(dat)
hist(dat$cvd)
fit <- gam(cvd ~ o3 + s(month, bs = 'cc', k = 4, fx=T) + s(time, k = 6, fx=T), data = dat,
           family = poisson(link=log))
summary(fit)
# edf reports k minus one for s(), looks like k - 2 for cyclic basis
# bs='cc'
png("~/season03.png")
par(mfrow=c(2,2))
plot(fit, all.terms = T)
dev.off()

{% endhighlight %}
