
################################################################
# func
setwd("~/projects/spatiotemporal-regression-models/NMMAPS-example")
require(plyr)

# load
analyte <- read.csv("analyte.csv")

# clean
head(analyte)
analyte$yy  <- substr(analyte$date, 1, 4)
  
# do
## first we want to see if the age specific rates vary across the
## study sites
disease_colname <- "cvd"
pop_colname <- "pop"
by_cols  <- c("city", "agecat", "yy")

stdysites  <- ddply(analyte, by_cols,
                    function(df) return(
                      c(observed = sum(df[,disease_colname]),
                        pop = mean(df[,pop_colname]),
                        crude.rate = sum(df[,disease_colname])/mean(df[,pop_colname])
                        )
                      )
                    )
## check
head(stdysites)
subset(stdysites, yy == 1987 & city == "batr")

## define the subset we will use just the deaths and pop in 2000
stdy <- subset(stdysites, yy == 2000)
stdy

## now define the standard population as the entire country
standard  <- ddply(stdy, c("agecat"),
                     function(df) return(
                       c(observed = sum(df[,"observed"]),
                         pop = sum(df[,"pop"]),
                         crude.rate = sum(df[,"observed"])/
                           sum(df[,"pop"])
                         )
                       )
                    )
standard

## Merge the studysites and the standard
stdyByStnd  <- merge(stdy, standard, by = "agecat")
stdyByStnd

## plot the rate ratios
png("images/ratio-stdy-by-stnd.png")
mp <- barplot(stdyByStnd$crude.rate.x/stdyByStnd$crude.rate.y)
text(mp, par("usr")[3],
     labels = paste(stdyByStnd$agecat,stdyByStnd$city),
     srt = 45, adj = c(1.1,1.1), xpd = TRUE
     )
abline(1,0)
dev.off()

## we can see that LA has an issue with the 65to74 agecat

## Second we will check if ratio of the proportions in each population
## agecat vary between study sites and the standard
totals <- ddply(stdyByStnd, c("city"), summarise,
                sum(pop.x)
                )
totals <- merge(stdyByStnd[,c("city","agecat","pop.x")], totals)
totals$pop.wt  <- totals[,3] / totals[,4]
totals <- arrange(totals, city, agecat)
totals

totalsStnd <- ddply(stdyByStnd, c("agecat"), summarise,
                sum(pop.x)
                )
totalsStnd$totalPop <- sum(totalsStnd[,2])
totalsStnd$pop.wt.total <- totalsStnd[,2]/totalsStnd[,3]
totalsStnd

## merge these so we can look at the ratios
totals2 <- merge(totals, totalsStnd, by = "agecat")
totals2 <- arrange(totals2, agecat, city)

## now plot the ratios
png("images/ratio-stdy-by-stnd-pops.png")
mp <- barplot(totals2$pop.wt/totals2$pop.wt.total)
text(mp, par("usr")[3],
     labels = paste(totals2$agecat,totals2$city),
     srt = 45, adj = c(1.1,1.1), xpd = TRUE
     )
abline(1,0)
dev.off()

## and we can see that there are far fewer 65to74 aged persons in LA
## than expected.
