
# Checking the proportional hazards assumption for Indirect Age Standardisation

# Background
## Indirect SMRs from different index/study populations are not strictly comparable
## because they are calculated using different weighting schemes that
## depend upon the age structures of the index/study populations
## (see http://www.statsdirect.com/webhelp/#rates/smr.htm).

## Indirect SMRs can be compared if you make the assumption that the
## ratio of rates between index and reference populations is constant;
## this is similar to the assumption of proportional hazards in Cox
## regression (Armitage and Berry, 1994).

## So we need to check if the rate ratio of the study population(s)
## compared to the standard population varies substantially with age.
## If not the proportional hazards assumption holds for the standard
## rates compared with the observed rates and the Indirect SMRs are
## comparable.  To do this check we calculate the Annualised Age Specific
## Rates for our study areas and for our standard for several years at
## periodic timepoints across the study period, and then calculate the
## ratio of these at each timepoint we could reassure our selves that
## this assumption holds.

## An additional issue arises when there are non-negligible differences
## in the age distributions of the study population(s) and the standard
## population. In this situation, indirect standardisation produces
## biased results due to residual confounding by age

## Also see Australian Institute of Health and Welfare. (2011). Principles on
## the use of direct age-standardisation in administrative data
## collections For measuring the gap between Indigenous and
## non-Indigenous Australians. Data linkage series. Cat. no. CSI 12.
## Canberra: AIHW. Retrieved from
## http://www.aihw.gov.au/publication-detail/?id=10737420133

################################################################
# func
setwd("~/projects/spatiotemporal-regression-models/NMMAPS-example")
require(plyr)

# load
analyte <- read.csv("analyte.csv")
View(analyte)

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
plot(mp)

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
totals2
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
