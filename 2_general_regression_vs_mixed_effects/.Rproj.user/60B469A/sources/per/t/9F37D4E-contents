#### name: basic regression model with varying intercepts and varying slopes ####
# load some data
dat <- read.csv("data/simulated_data.csv")

# exploratory work
str(dat)
summary(dat)
plot(dat$x, dat$y)
plot(dat$z, dat$y)
plot(density(dat$y))
with(dat, plot(x, y, col = z))
legend("topleft", legend = 1:3, fill = 1:3)

# subset
dat <- dat[dat$z != "group3",]
str(dat)
# z still has 3 factor levels
table(dat$z)
# we can remove the extra level
dat$z <- factor(dat$z)
table(dat$z)

# basic linear model
fit1 <- lm(y ~ x, data = dat)
summary(fit1)
fit1cf <- coefficients(fit1)
with(dat, plot(x, y, col = z))

abline(fit1cf[1], fit1cf[2])

# we suspect a group level effect! begin to build multilevel model(s)

par(mar = c(3,3,2,1))
plot(1, type="n", xlim=c(-0.3,6), ylim=c(-1,0.5), xlab="", ylab="", axes = F)
axis(1, labels = F); axis(2, labels = F)
mtext("Y", 2, 1 , at = 0, las = 2)
mtext("X", 1, 1 , at = 3)
  
with(subset(dat, z == "group1"), points(x, y,  pch = 1, cex = .5))
with(subset(dat, z == "group2"), points(x, y, pch = 3, cex = .5))

# a multiplicative interaction term
fit2  <- lm(y ~ x * z, data = dat)
summary(fit2)

# this is a different equation
title(expression(paste("Regression model Y = ", beta[0] + beta[1],"X + ", beta[2], "Z + ", beta[3], "XZ + ", epsilon )))

fit2cf <- coefficients(fit2)
b0 <- fit2cf[1]
b1 <- fit2cf[2]
b2 <- fit2cf[3]
b3 <- fit2cf[4]


abline(b0, b1)
abline(b0+b2, b1+b3)


text(x = 2.8, y = 0.2, expression(paste("Y = (", beta[0] + beta[2],") + (", beta[1] + beta [3],")X when Z = 1")))
text(x = 1.9, y = 0.1, expression(paste("Slope = ", beta[1] + beta[3])))

text(x = -0.3, y = -.75, expression(beta[0]))
segments(0, b0, 0, b0+b2, lty = 1, col = 'grey', lwd = 6)

text(x = -0.3, y = -0.4, expression(beta[2]))
segments(-.9, b0+b2, 0, b0+b2, lty = 3)

text(x = 3.57, y = -0.7,  expression(paste("Y = ", beta[0] + beta[1] ,"X when Z = 0")))
text(x = 3, y = -0.8, expression(paste("Slope = ", beta[1])))

segments(-.9, b0, 0, b0, lty = 3)
segments(0, b0+b2, 0, -1, lty = 3)
