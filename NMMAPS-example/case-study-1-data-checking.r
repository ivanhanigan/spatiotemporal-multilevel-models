
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
stdysites  <- ddply(analyte, c("city", "agecat", "yy"),
                     function(df) return(
                       c(observed = sum(df[,"cvd"]),
                         pop = mean(df[,"pop"]),
                         crude.rate = sum(df[,"cvd"])/mean(df[,"pop"])
                         )
                       )
                    )
subset(stdysites, yy == 1987 & city == "batr")
stdy <- subset(stdysites, yy == 2000)

## standard
standard  <- ddply(stdysites, c("agecat", "yy"),
                     function(df) return(
                       c(observed = sum(df[,"observed"]),
                         pop = sum(df[,"pop"]),
                         crude.rate = sum(df[,"observed"])/
                           sum(df[,"pop"])
                         )
                       )
                    )
stnd <- subset(standard, yy == 2000)

stdyByStnd  <- merge(stdy, stnd, by = "agecat")
stdyByStnd

## ratio
png("images/ratio-stdy-by-stnd.png")
mp <- barplot(stdyByStnd$crude.rate.x/stdyByStnd$crude.rate.y)
text(mp, par("usr")[3],
     labels = paste(stdyByStnd$agecat,stdyByStnd$city),
     srt = 45, adj = c(1.1,1.1), xpd = TRUE
     )
abline(1,0)
dev.off()

## Second we will check if populations vary
totals <- ddply(stdyByStnd, c("city"), summarise,
                sum(pop.x)
                )
totals <- merge(stdyByStnd[,c("city","agecat","pop.x")], totals)
totals$pop.wt  <- totals[,3] / totals[,4]
totals <- arrange(totals, city, agecat)

totalsStnd <- ddply(stdyByStnd, c("agecat"), summarise,
                sum(pop.x)
                )
totalsStnd$totalPop <- sum(totalsStnd[,2])
totalsStnd$pop.wt.total <- totalsStnd[,2]/totalsStnd[,3]
totalsStnd
totals
totals2 <- merge(totals, totalsStnd, by = "agecat")
totals2 <- arrange(totals2, agecat, city)
png("images/ratio-stdy-by-stnd-pops.png")
mp <- barplot(totals2$pop.wt/totals2$pop.wt.total)
text(mp, par("usr")[3],
     labels = paste(totals2$agecat,totals2$city),
     srt = 45, adj = c(1.1,1.1), xpd = TRUE
     )
abline(1,0)
dev.off()
