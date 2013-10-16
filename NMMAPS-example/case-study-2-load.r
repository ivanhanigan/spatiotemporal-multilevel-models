
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
