#### name: multilevel model with varying intercepts and varying slopes ####
library(lme4)
dat <- read.csv("data/simulated_data.csv")
# step one: exploratory work
str(dat)
summary(dat)
plot(dat$x, dat$y)
plot(dat$z, dat$y)
plot(density(dat$y))

fit0 <- lm(y ~ x, data = dat)
summary(fit0)
fit0cf <- coefficients(fit0)
with(dat, plot(x, y, col = z))

abline(fit0cf[1], fit0cf[2])

# step 2: we suspect a group level effect! begin to build multilevel model(s)
# first lets set up a nice plot
my_plot <- function(dat){

  par(mar = c(3,3,2,1))
  plot(1, type="n", xlim=c(-0.3,6), ylim=c(-1,0.5), xlab="", ylab="", axes = F)
  axis(1, labels = F); axis(2, labels = F)
  mtext("Y", 2, 1 , at = 0, las = 2)
  mtext("X", 1, 1 , at = 3)
  
  with(subset(dat, z == "group1"), points(x, y,  pch = 1, cex = .5))
  with(subset(dat, z == "group2"), points(x, y, pch = 3, cex = .5))
  with(subset(dat, z == "group3"), points(x, y, pch = 4, cex = 1.5, col = 'darkgreen'))

}

my_plot(dat)
#dev.off()

#  model 1: OLS with unvarying intercept and slope (fixed) 
fit <- lm(y~x, data = dat)
summary(fit)
abline(fit, lwd = 2, lty = 2)

# model 2: varying intercept (random) and unvarying slope (fixed)
my_plot(dat)
fit2  <- lm(y ~ x + z, data = dat)
summary(fit2)
# we know this is put together in a linear equation thus:
title(expression(paste("Regression model Y = ", beta[0] + beta[1],"X + ", beta[2], "Z + ", epsilon )))

# so create a coeffs object
fit2cf <- coefficients(fit2)

# now we can extract the betas
b0   <- fit2cf[1]
b1   <- fit2cf[2]
b2.1 <- fit2cf[3]
b2.2 <- fit2cf[4]

# and show the linear fits
abline(b0, b1)
abline(b0+b2.1, b1)
abline(b0+b2.2, b1)

# TODO maybe stop to talk about the tests of 'if group makes a difference?' here, which justify going the next

#######################################################################################
#### model 3: LM approach to varying intercept (random) and varying slope (random) ####
#######################################################################################
# clear out the plot
my_plot(dat)
fit3  <- lm(y ~ x * z, data = dat)
summary(fit3)

# this is a different equation
title(expression(paste("Regression model Y = ", beta[0] + beta[1],"X + ", beta[2], "Z + ", beta[3], "XZ + ", epsilon )))

# there is a trick to make this less work with getting se around multiplicative interaction terms... for lesson 3!
# x1 <- ifelse(dat$z == 'group1', x, 0)
# x2 <- ifelse(dat$z == 'group2', x, 0)
# x3 <- ifelse(dat$z == 'group3', x, 0)
# 
# fit3 <- lm(y ~ z + x1 + x2 + x3, data = dat)
# summary(fit3)
fit3cf <- coefficients(fit3)
b0   <- fit3cf[1]
b1   <- fit3cf[2]
b2.1 <- fit3cf[3]
b2.2 <- fit3cf[4]
b3.1 <- fit3cf[5]
b3.2 <- fit3cf[6]

abline(b0, b1)
abline(b0+b2.1, b1+b3.1)
abline(b0+b2.2, b1+b3.2, col = 'darkgreen')

# at this point may expect a question about change in X = estimated change in Y for each

######################################################################################################
#### model 5 is linear mixed effects regression (lmer) with varying intercept and unvarying slope ####
######################################################################################################
my_plot(dat)
# all the tutes I read said to start with a 'null' random intercepts model
fitnull <- lmer(y ~ (1|z), data = dat)
summary(fitnull)
random <- ranef(fitnull)
fixed <- fixef(fitnull)
gamma1 <- fixed[1] + random[[1]][1,1]  # this is beta0 + mu0 where Z = group1
gamma2 <- fixed[1] + random[[1]][2,1]  # this is beta0 + mu0 where Z = group2
gamma3 <- fixed[1] + random[[1]][3,1]  # this is beta0 + mu0 where Z = group3

abline(gamma1, 0, col = 'red', lty = 'dashed')
abline(gamma2, 0, col = 'red', lty = 'dashed')
abline(gamma3, 0, col = 'red', lty = 'dashed')

# versus the LM version
fitnull1 <- lm(y ~ 1 + z, data = dat)
summary(fitnull1)


# but we know we have slopes so lets get going
my_plot(dat)
fit5 <- lmer(y ~ x + (1|z), data = dat)
summary(fit5)

title(expression(paste("Regression model Y = ", beta[0] + beta[1],"X + (", mu[0], "Z + ", epsilon, ")")))

# we can extract the components thus
random <- ranef(fit5)
random
fixed <- fixef(fit5)
fixed

gamma1 <- fixed[1] + random[[1]][1,1]  # this is beta0 + mu0 where Z = group1
gamma2 <- fixed[1] + random[[1]][2,1]  # this is beta0 + mu0 where Z = group2
gamma3 <- fixed[1] + random[[1]][3,1]  # this is beta0 + mu0 where Z = group3
beta_x <- unlist(fixed)[2]

abline(gamma1, beta_x, col = 'red', lty = 'dashed')
abline(gamma2, beta_x, col = 'red', lty = 'dashed')
abline(gamma3, beta_x, col = 'red', lty = 'dashed')

# model 6  is lmer with varying intercept and varying slope
my_plot(dat)
fit6 <- lmer(y ~ x + (1 + x |z), data = dat)
summary(fit6)

title(expression(paste("Regression model Y = ", beta[0] + beta[1],"X + (", mu[0], "Z + ", mu[1], "X + ", epsilon, ")")))

random <- ranef(fit6)
random
fixed <- fixef(fit6)
fixed

gamma1 <- fixed[1] + random[[1]][1,1]
gamma2 <- fixed[1] + random[[1]][2,1]
gamma3 <- fixed[1] + random[[1]][3,1]
beta_x1 <- fixed[2] + random[[1]][1,2]
beta_x2 <- fixed[2] + random[[1]][2,2]
beta_x3 <- fixed[2] + random[[1]][2,2]

abline(gamma1, beta_x1, col = 'red', lty = 'dashed')
abline(gamma2, beta_x2, col = 'red', lty = 'dashed')
abline(gamma3, beta_x3, col = 'red', lty = 'dashed')



