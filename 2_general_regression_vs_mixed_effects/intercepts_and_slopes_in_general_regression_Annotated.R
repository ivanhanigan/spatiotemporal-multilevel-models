#### name: general regression model with varying intercepts and varying slopes ####
## this example comes from https://github.com/ivanhanigan/spatiotemporal-multilevel-models
## 2_general_regression_vs_mixed_effects/intercepts_and_slopes_in_general_regression_Annotated.R

#### load and clean in data ####

dat <- read.csv("data/simulated_data.csv")
dat$z <- factor(dat$z)
## subset the data to just excluding group 3 
## this was only dropped to make it simpler

dat <- dat[dat$z != "group3",]
str(dat)

## remove factor levels that don't contain any data 

table(dat$z)
dat$z <- factor(dat$z)
table(dat$z)

#### do exploratory work ####

str(dat)
summary(dat)

#### do plot for exploration ####

plot(dat$x, dat$y)
plot(dat$z, dat$y)
par(mfrow = c(2,1), mar = c(2,2,1,1))
plot(dat$x, dat$y)
plot(dat$z, dat$y)
dev.off()

plot(density(dat$y))

par(mfrow = c(2,1), mar = c(2,2,1,1))
plot(dat$z, dat$y)
with(dat, plot(x, y, col = z))
legend("topleft", legend = c("group 1", "group 2"), fill = 1:2)

dev.off()


#### do a linear model ####
fit1 <- lm(y ~ x, data = dat)
summary(fit1)

library(stargazer)
stargazer(fit1, type = "text")
## the results show that both the intercept and x are significant, and the R2 is 0.12

#### do plotting linear model ####
#### view the parameter estimates
#### intercept and slope (coefficient is slope)
fit1cf <- coefficients(fit1)
##fit1cf
with(dat, plot(x, y, col = z))
abline(fit1cf[1], fit1cf[2])

## this is a wrong model! Because it's getting the middle and not capturing the difference in groups 

#### do build a plot manually ####

par(mar = c(3,3,2,1))
plot(1, type="n", xlim=c(-0.3,6), ylim=c(-1,0.5), xlab="", ylab="", axes = F)
axis(1, labels = F); axis(2, labels = F)
mtext("Y", 2, 1 , at = 0, las = 2)
mtext("X", 1, 1 , at = 3)
  
with(subset(dat, z == "group1"), points(x, y,  pch = 1, cex = .5))
with(subset(dat, z == "group2"), points(x, y, pch = 3, cex = .5))

# this plot can be bult into something awesome 

# the results suggest a a group level effect! So we can begin to build stratified model. 
# the intercept and slope can vary by group ie age / gender etc. 
# the effect of x on y is confounded by x. 


#### do interaction term (stratified model) ####

fit2  <- lm(y ~ (x * z), data = dat)

summary(fit2)
stargazer(fit2, type = "text")
## what does the results mean? 

## x:zgroup2 - b3 - the term
# the difference of the slope between the two categories is significantly different. 
## following plot helps 

# so this model is a linear model however is times by the two groups (in an example case that could be agegroup or gender)
# results are much better - significant for all factors and R2 of 0.97

#### do plot the new model results ####

title(expression(paste("Regression model Y = ", beta[0] + beta[1],"X + ", beta[2], "Z + ", beta[3], "XZ + ", epsilon )))

# y = a+b(x)+error  - slope of a line 


fit2cf <- coefficients(fit2)
b0 <- fit2cf[1]
b1 <- fit2cf[2]
b2 <- fit2cf[3]
b3 <- fit2cf[4]

fit2cf

## the interaction term allows you to ask if the impact of 'group' (ie gender) if it's modified / mediated by this group. 
#  different ways to test that - can compare the AIC or the BICs of the two models. (one with and one without the groups)

abline(b0, b1)
# intecept b0 and slope b1
# this is the bottom flat line 

abline(b0+b2, b1+b3)
# this is the effect of going from b0 to 1
# thie second line (top) has an intecept that is greater than b0. it's the effectof adding b2 to b0. 


text(x = 2.8, y = 0.2, expression(paste("Y = (", beta[0] + beta[2],") + (", beta[1] + beta [3],")X when Z = group2")))
text(x = 1.9, y = 0.1, expression(paste("Slope = ", beta[1] + beta[3])))

text(x = -0.3, y = -.75, expression(beta[0]))
segments(0, b0, 0, b0+b2, lty = 1, col = 'grey', lwd = 6)

# b0 is the difference between b0 and b1. 

text(x = -0.3, y = -0.4, expression(beta[2]))
segments(-.9, b0+b2, 0, b0+b2, lty = 3)

text(x = 3.57, y = -0.7,  expression(paste("Y = ", beta[0] + beta[1] ,"X when Z = group1")))
text(x = 3, y = -0.8, expression(paste("Slope = ", beta[1])))

segments(-.9, b0, 0, b0, lty = 3)
segments(0, b0+b2, 0, -1, lty = 3)

#### Problems ####
## next things to look at is standard error. standard error of b1
## (ie bottom slope)



