
################################################################
# name:spatwat
# func
setwd("~/projects/spatiotemporal-regression-models/NMMAPS-example")
require(gisviz)

# load
dir()
shp <- readOGR("cities.shp", "cities")

# clean
head(shp@data)

# do
## I will use nearest neighbour within a distance threshold, but
## usually polygon datasets would use poly2nb
nb <- dnearneigh(shp, d1 = 1, d2 = 1000)                      
head(nb)
shp[[1]][1]
shp[[1]][nb[[1]]]
# map
nudge <- 10
png("images/nmmaps-eg-neighbourhood.png")
plotMyMap(
    shp@coords, xl = c(min(shp@coords[,1])-nudge, max(shp@coords[,1])+nudge),
    yl = c(min(shp@coords[,2])-nudge, max(shp@coords[,2])+nudge)
    )
plot(nb, shp@coords, add=TRUE)
text(shp@data$long, shp@data$lat, shp@data$address, pos=3)
points(shp@coords[nb[[1]],], col = 'green', pch = 16)
points(shp@data[1,c("long","lat")],col = 'blue', pch = 16)
dev.off()

#### Now do the Neighbourhood autoregressive lagged variable  ####
# func
setwd("~/projects/spatiotemporal-regression-models/NMMAPS-example")
require(gisviz)
require(plyr)

# load
analyte <- read.csv("analyte.csv")
shp <- readOGR("cities.shp", "cities")

# clean
analyte$yy <- substr(analyte$date,1,4)
numYears<-length(names(table(analyte$yy)))
analyte$date <- as.Date(analyte$date)
analyte$time <- as.numeric(analyte$date)
analyte$agecat <- factor(analyte$agecat,
                          levels = c("under65",
                              "65to74", "75p"),
                          ordered = TRUE
                          )
 
names(analyte)
analyte[1,]
table(analyte$city)
# do
study <- analyte[, c("city","date", "agecat", "cvd", "pop")]
subset(study, city == "tucs" & date == as.Date("1991-01-01"))

nb <- dnearneigh(shp, d1 = 1, d2 = 1000)                      
head(nb)
head(shp@data)
adj <- adjacency_df(NB = nb, shp = shp, zone_id = 'city')
subset(adj, V1 == "tucs")

neighbours <- merge(study, adj, by.x = "city", by.y = "V1")
subset(neighbours, city == "tucs" & date == as.Date("1991-01-01"))

xvars <- c("V2", "date","agecat")
yvars <- c("city", "date", "agecat")
neighbours <- merge(neighbours[,c(xvars, "city")],
                    analyte[,c(yvars, "cvd", "pop")],
                    by.x = xvars,
                    by.y = yvars)
names(neighbours)
subset(neighbours, city == "tucs" & date == as.Date("1991-01-01"))

neighbours$asr  <- (neighbours$cvd / neighbours$pop) * 1000


neighbours2  <- ddply(neighbours, c("city", "date", "agecat"), summarise,
                     NeighboursYij  = mean(asr)
                     )
subset(neighbours2, city == "tucs" & date == as.Date("1991-01-01"))
table(neighbours2$city)
head(analyte)

analyte  <- merge(analyte, neighbours2, by = c("city", "agecat", "date"))
analyte <- arrange(analyte, city, date, agecat)
head(analyte)
