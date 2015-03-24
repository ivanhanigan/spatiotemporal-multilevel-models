
# func
require(splines)

# load
# assumes the prior code chunks have been run

# do
fit <- glm(cvd ~ ns(tmax, df = 3) + ns(dptp, df = 3) +
           NeighboursYij + 
           city + agecat +
           ns(time, df = 7*numYears) +
           offset(log(pop)),
           data = analyte, family = poisson
           )

# plot of response functions
png("images/nmmaps-eg-sp-lag.png", width = 1000, height = 750, res = 150)
par(mfrow=c(2,3))
termplot(fit, terms = attr(terms(fit),'term.labels') [c(1:2,6)], se = TRUE, ylim =c(-.2,.2), col.term = 'black', col.se = 'black')
termplot(fit, terms = attr(terms(fit),'term.labels') [c(4,5)], se = TRUE, ylim =c(-3,3), col.term = 'black', col.se = 'black')
termplot(fit, terms = attr(terms(fit),'term.labels') [3], se = TRUE, ylim =c(-1,1), col.term = 'black', col.se = 'black')
dev.off()
