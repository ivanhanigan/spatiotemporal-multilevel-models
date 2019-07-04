---
layout: default
title: Introduction to R
---

<iframe style="border: none;" height="400" width="600" src="./images/20190312_data_inventory.mp4"></iframe>


Download this script [here](./intro-to-r.R)


{% highlight r %}
## Intro to R ##
# objects
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

# some packages are shipped with the base installation
library(mgcv)
# with statistical methods such as generalised additive models and
# penalised splines
fit <- gam(y ~ s(x))
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

# and R can read most file formats
library(foreign)
# or even stata13/14 files 
library(readstata13)
infile <- dir(file.path(system.file(package = "readstata13"), "extdata"), full.names = T, pattern = ".dta")
infile
dat <- read.dta13(infile)
str(dat)

{% endhighlight %}
