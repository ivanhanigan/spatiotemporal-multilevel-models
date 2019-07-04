#### name: basic regression model with varying intercepts and varying slopes ####

#### 1.1 load in data

dat <- read.csv("C:/Users/hs419/OneDrive - Macquarie University/data/practice/2_general_regression_vs_mixed_effects/data/simulated_data.csv")


## which data to use in this model? 
# single incident data  - 
# daily count 

# if it's just age / gender then go with possion daily count....

## note that this model isn't telling you if gender etc is significant, but rather how different they are from each other. 


#### 1.2 exploatory work 

str(dat)
summary(dat)

#### 1.2.1 plot for exploration 

plot(dat$x, dat$y)
plot(dat$z, dat$y)
plot(density(dat$y))
with(dat, plot(x, y, col = z))
legend("topleft", legend = 1:3, fill = 1:3)


#### 1.3 subset the data to just including group 3 
# this was only dropped to make it simplier

dat <- dat[dat$z != "group3",]
str(dat)

# 1.3.1 remove features that don't contain any data 

table(dat$z)
dat$z <- factor(dat$z)
table(dat$z)

#### 1.4 running a basic linear model ####


fit1 <- lm(y ~ x, data = dat)

summary(fit1)

fit1cf <- coefficients(fit1)
fit1cf
# view the matrix - intercept and slope (coefficient is slope)

## the results show that both the intecept and x are significant, and the R2 is 0.12

#### 1.4.1 plotting basic linear model 

with(dat, plot(x, y, col = z))

abline(fit1cf[1], fit1cf[2])

## this is a wrong model! Because it's getting the middle and not capturing the difference in groups 

# 1.4.2 build a plot manually 

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


#### 1.5 a multiplicative interaction term (stratified model) ####

fit2  <- lm(y ~ (x * z), data = dat)

summary(fit2)

## what does the results mean? 

## x:zgroup2 - b3 - the term
# the difference of the slope between the two categories is significantly different. 
## following plot helps 

# so this model is a linear model however is times by the two groups (in my case that could be gender)
# results are much better - significant for all factors and R2 of 0.97

#### 1.5.1 plot the new model results ####
# wow very nice code to build this plot.  What is it telling me? 

title(expression(paste("Regression model Y = ", beta[0] + beta[1],"X + ", beta[2], "Z + ", beta[3], "XZ + ", epsilon )))

# y = a+b(x)+error  - slope of a line 


fit2cf <- coefficients(fit2)
b0 <- fit2cf[1]
b1 <- fit2cf[2]
b2 <- fit2cf[3]
b3 <- fit2cf[4]

fit2cf

# Call:
#   lm(formula = y ~ (x * z), data = dat)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.132759 -0.031398 -0.001198  0.030781  0.130142 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
#  B0 - (Intercept) -0.520880   0.008633 -60.334  < 2e-16 ***
#   B1-  x            0.007245   0.002487   2.913  0.00392 ** 

# this is the estimate of the slope, with p value of standard error. 

#   B2-  zgroup2      0.187348   0.012209  15.345  < 2e-16 ***
#   B3 -  x:zgroup2    0.094204   0.003517  26.784  < 2e-16 ***

# 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04778 on 238 degrees of freedom
# Multiple R-squared:  0.9693,	Adjusted R-squared:  0.969 
# F-statistic:  2509 on 3 and 238 DF,  p-value: < 2.2e-16

## 

## the interaction term allows you to ask if the impact of 'group' (ie gender) if it's modified / mediated by this group. 
#  different ways to test that - can compare the AIC or the BICs of the two models. (one with and one without the groups)

abline(b0, b1)
# intecept b0 and slope b1
# this is the bottom flat line 

abline(b0+b2, b1+b3)
# this is the effect of going from b0 to 1
# thie second line (top) has an intecept that is greater than b0. it's the effectof adding b2 to b0. 


text(x = 2.8, y = 0.2, expression(paste("Y = (", beta[0] + beta[2],") + (", beta[1] + beta [3],")X when Z = 1")))
text(x = 1.9, y = 0.1, expression(paste("Slope = ", beta[1] + beta[3])))

text(x = -0.3, y = -.75, expression(beta[0]))
segments(0, b0, 0, b0+b2, lty = 1, col = 'grey', lwd = 6)

# b0 is the difference between b0 and b1. 

text(x = -0.3, y = -0.4, expression(beta[2]))
segments(-.9, b0+b2, 0, b0+b2, lty = 3)

text(x = 3.57, y = -0.7,  expression(paste("Y = ", beta[0] + beta[1] ,"X when Z = 0")))
text(x = 3, y = -0.8, expression(paste("Slope = ", beta[1])))

segments(-.9, b0, 0, b0, lty = 3)
segments(0, b0+b2, 0, -1, lty = 3)


## next things to look at is standard error. standard error of b1
 #(ie bottom slope)



#### MY TURNbasic regression model with varying intercepts and varying slopes ####

#### 1.1 load in data

## make a dataset that counts by age / gender per day per suburb 
# age groups (5)
# gender 
# event date
# met conditions
# region / spatial 
# lat long 
# daily count of this dataset. 
# group by.... SA3? , count of age brackets, count of gender, and mean max_ave and mean EHF_load 

dat1 <- read.csv("C:/Users/hs419/OneDrive - Macquarie University/data/modelling/csv/incident_with_weather_poi_vic.csv")

str(dat1)

# 1.1. cut it down to just gender and temperature?

dat2 <- dat1 %>% 
  subset(select = c("offender_gender", 
                    "max_ave", "event_date" ))

# make just for sydeny so it's a smaller sample

assaults_daily <- subset(assaults_daily, assaults_daily$SA4_CODE_2011== "101") 

dat2 <- dat2 %>% 
  subset( dat1$incident_lga == "Sydney")


#### 1.2 exploatory work 
str(dat2)
summary(dat2)

#### 1.2.1 plot for exploration 

dat2$event_date<- as.Date(dat2$event_date)

plot(dat2$event_date,dat2$max_ave)

plot(dat2$max_ave, dat2$offender_gender)

# why is this flat lined? 

plot(density(dat2$offender_gender))

with(dat2, plot(event_date, max_ave, col = offender_gender))
legend("topleft", legend = 1:3, fill = 1:3)


#### 1.4 running a basic linear model ####

fit1 <- lm(y ~ x, data = dat)

summary(fit1)

fit1cf <- coefficients(fit1)

## the results show that both the intecept and x are significant, and the R2 is 0.12

#### 1.4.1 plotting basic linear model 

with(dat, plot(x, y, col = z))

abline(fit1cf[1], fit1cf[2])

# 1.4.2 build a plot manually 

par(mar = c(3,3,2,1))
plot(1, type="n", xlim=c(-0.3,6), ylim=c(-1,0.5), xlab="", ylab="", axes = F)
axis(1, labels = F); axis(2, labels = F)
mtext("Y", 2, 1 , at = 0, las = 2)
mtext("X", 1, 1 , at = 3)

with(subset(dat, z == "group1"), points(x, y,  pch = 1, cex = .5))
with(subset(dat, z == "group2"), points(x, y, pch = 3, cex = .5))

# the results suggest a a group level effect! So we can begin to build multilevel model(s)

#### 1.5 a multiplicative interaction term ####

fit2  <- lm(y ~ x * z, data = dat)
summary(fit2)

# so this model is a linear model however is times by the two groups (in my case that could be gender)
# results are much better - significant for all factors and R2 of 0.97

#### 1.5.1 plot the new model results ####
# wow very nice code to build this plot.  What is it telling me? 

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

