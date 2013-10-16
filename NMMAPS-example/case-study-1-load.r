
################################################################
# name:Pooled Dataset
setwd("~/projects/spatiotemporal-regression-models/NMMAPS-example")

flist <- dir("data", full.names=T)

flist <-    flist[which(basename(flist) %in%
                        c("Baton Rouge.csv",
                          "Los Angeles.csv",
                          "Tucson.csv",
                          "Denver.csv")
                        )
                  ]
flist
for(f_i in 1:length(flist))
    {
        #f_i <- 2
        fi <- flist[f_i]
        df <- read.csv(fi)
        df <- df[,c("city","date", "agecat",
                    "cvd", "resp", "tmax",
                    "tmin", "dptp")]
        # str(df)
        write.table(df,
                    "outcome.csv", sep = ",",
                    row.names = F, append = f_i > 1,
                    col.names = f_i ==1
                    )
    }

################################################################
# name:zones
# func
setwd("~/projects/spatiotemporal-regression-models/NMMAPS-example")
#require(devtools)
#install_github("gisviz", "ivanhanigan")
require(gisviz)

# load
flist <- dir("data")

# do    
## geocode
flist <- gsub(".csv", "", flist)    
flist_geo <- gGeoCode2(flist)
flist_geo

## plot
png("images/nmmaps-eg-cities.png")
plotMyMap(
    flist_geo[,c("long","lat")],
    xl = c(-130,-60), yl = c(25,50)
    )
text(flist_geo$long, flist_geo$lat, flist_geo$address, pos=3)
dev.off()
   
# load city names
flist2 <- dir("data")
flist2
city_codes <- matrix(NA, nrow = 0, ncol = 2)
for(fi in 1:length(flist2))
    {
        # fi <- 1
        fname <- flist2[fi]
        print(fi); print(fname);
        df <- read.csv(
            file.path("data", fname),
            stringsAsFactors = F, nrow = 1)
        city_codes <- rbind(city_codes,
            c(gsub(".csv","",fname), df$city)
            )
    }
city_codes <- as.data.frame(city_codes)
names(city_codes) <- c("address","city")
city_codes 
flist_geo2 <- merge(flist_geo, city_codes, by = )
flist_geo2

## make shapefile
epsg <- make_EPSG()
prj_code <- epsg[grep("WGS 84$", epsg$note),]
prj_code
shp <- SpatialPointsDataFrame(cbind(flist_geo2$long,flist_geo2$lat),flist_geo2,
                              proj4string = CRS(
                                  epsg$prj4[which(epsg$code ==prj_code$code)]
                                  )
                              )
writeOGR(shp, 'cities.shp', 'cities', driver='ESRI Shapefile')

################################################################
# name:population-data
# first city, remove population data.frame
rm(population)

# func
setwd("~/projects/spatiotemporal-regression-models")
require(gisviz)

# load
## citycodes
shp <- readOGR("cities.shp", "cities")
shp@data
cityname <- "Baton Rouge"

# do
# save url
# http://www.city-data.com/us-cities/The-South/Baton-Rouge-Population-Profile.html
population_input <- read.table(textConnection(
"age: population
Population Under 5 years: 15502
Population 5 to 9 years: 15609
Population 10 to 14 years: 15248
Population 15 to 19 years: 21954
Population 20 to 24 years: 27230
Population 25 to 34 years: 31719
Population 35 to 44 years: 30343
Population 45 to 54 years: 27166
Population 55 to 59 years: 9495
Population 60 to 64 years: 7490
Population 65 to 74 years: 13312
Population 75 to 84 years: 9611
Population 85 years and over: 3139
"), sep = ":", header = TRUE)

## agecats
## 65to74     75p under65  
population_input$agecat <- c(rep("under65", 10),
                             rep("65to74"),
                             rep("75p",2)
                             )

citycode <- subset(shp@data, address == cityname,
                   select = city)

population_input$city <- rep(
    as.character(citycode[1,1])        
    , 13
    )

population_input
if(exists("population"))
    {
        population <- rbind(population,
                            population_input
                            )        
    } else {
        population <- population_input
    }

################################################################
# name:population-data

# func
#setwd("~/projects/spatiotemporal-regression-models")
#require(gisviz)

# load
## citycodes
shp <- readOGR("cities.shp", "cities")
shp@data
cityname <- "Los Angeles"

# do
# save url
# http://www.city-data.com/us-cities/The-West/Los-Angeles-Population-Profile.html
population_input <- read.table(textConnection(
"age: population
Population under 5 years old: 285976
Population 5 to 9 years old: 297837
Population 10 to 14 years old: 255604
Population 15 to 19 years old: 251632
Population 20 to 24 years old: 299906
Population 25 to 34 years old: 674098
Population 35 to 44 years old: 584036
Population 45 to 54 years old: 428974
Population 55 to 59 years old: 143965
Population 60 to 64 years old: 115663
Population 65 to 74 years old: 18711
Population 75 to 84 years old: 125829
Population 85 years and over: 44189
"), sep = ":", header = TRUE)

## agecats
## 65to74     75p under65  
population_input$agecat <- c(rep("under65", 10),
                             rep("65to74"),
                             rep("75p",2)
                             )

citycode <- subset(shp@data, address == cityname,
                   select = city)

population_input$city <- rep(
    as.character(citycode[1,1])        
    , 13
    )

population_input
if(exists("population"))
    {
        population <- rbind(population,
                            population_input
                            )        
    } else {
        population <- population_input
    }

################################################################
# name:population-data

# func
#setwd("~/projects/spatiotemporal-regression-models")
#require(gisviz)

# load
## citycodes
shp <- readOGR("cities.shp", "cities")
shp@data
cityname <- "Tucson"

# do
# save url
# http://www.city-data.com/us-cities/The-West/Tucson-Population-Profile.html
population_input <- read.table(textConnection(
"age: population
Population under 5 years old: 35201
Population 5 to 9 years old: 34189
Population 10 to 14 years old: 31939
Population 15 to 19 years old: 38170
Population 20 to 24 years old: 47428
Population 25 to 34 years old: 76394
Population 35 to 44 years old: 72289
Population 45 to 54 years old: 57608
Population 55 to 59 years old: 19597
Population 60 to 64 years old: 16056
Population 65 to 74 years old: 29117
Population 75 to 84 years old: 21394
Population 85 years and older: 7317
"), sep = ":", header = TRUE)

## agecats
## 65to74     75p under65  
population_input$agecat <- c(rep("under65", 10),
                             rep("65to74"),
                             rep("75p",2)
                             )

citycode <- subset(shp@data, address == cityname,
                   select = city)

population_input$city <- rep(
    as.character(citycode[1,1])        
    , 13
    )

population_input
if(exists("population"))
    {
        population <- rbind(population,
                            population_input
                            )        
    } else {
        population <- population_input
    }

################################################################
# name:population-data

# func
#setwd("~/projects/spatiotemporal-regression-models")
#require(gisviz)

# load
## citycodes
shp <- readOGR("cities.shp", "cities")
shp@data
cityname <- "Denver"

# do
# save url
# http://www.city-data.com/us-cities/The-West/Denver-Population-Profile.html

population_input <- read.table(textConnection(
"age: population
Population under 5 years old: 37769
Population 5 to 9 years old: 34473
Population 10 to 14 years old: 31315
Population 15 to 19 years old: 32259
Population 20 to 24 years old: 45534
Population 25 to 34 years old: 113676
Population 35 to 44 years old: 86420
Population 45 to 54 years old: 71000
Population 55 to 59 years old: 22573
Population 60 to 64 years old: 17191
Population 65 to 74 years old: 30643
Population 75 to 84 years old: 23369
Population 85 years and over: 8414
"), sep = ":", header = TRUE)

## agecats
## 65to74     75p under65  
population_input$agecat <- c(rep("under65", 10),
                             rep("65to74"),
                             rep("75p",2)
                             )

citycode <- subset(shp@data, address == cityname,
                   select = city)

population_input$city <- rep(
    as.character(citycode[1,1])        
    , 13
    )

population_input
if(exists("population"))
    {
        population <- rbind(population,
                            population_input
                            )        
    } else {
        population <- population_input
    }

################################################################
# name:population-data

# func
setwd("~/projects/spatiotemporal-regression-models")
require(gisviz)

# load
## citycodes
shp <- readOGR("cities.shp", "cities")
shp@data

cityname <- "Louisville"

# do
# save url
#http://www.city-data.com/us-cities/The-South/Louisville-Population-Profile.html

## NB error in table, 75-84 was missing.  used same from baton rouge
population_input <- read.table(textConnection(
  "age: population
  Population under 5 years old: 16926
  Population 5 to 9 years old: 17359
  Population 10 to 14 years old: 16627
  Population 15 to 19 years old: 17362
  Population 20 to 24 years old: 18923
  Population 25 to 34 years old: 37541
  Population 35 to 44 years old: 40354
  Population 45 to 54 years old: 33755
  Population 55 to 59 years old: 10716
  Population 60 to 64 years old: 9211
  Population 65 to 74 years old: 18577
  Population 75 to 84 years: 9611
  Population 85 years and older: 5075
  "), sep = ":", header = TRUE)

## agecats
## 65to74     75p under65  
population_input$agecat <- c(rep("under65", 10),
                             rep("65to74"),
                             rep("75p",2)
                             )

citycode <- subset(shp@data, address == cityname,
                   select = city)

population_input$city <- rep(
    as.character(citycode[1,1])        
    , 13
    )

population_input
if(exists("population"))
    {
        population <- rbind(population,
                            population_input
                            )        
    } else {
        population <- population_input
    }

################################################################
# name:save-population
write.csv(population, "population.csv",
          row.names = FALSE
          )

################################################################
# name:Population Summary
# func
require(plyr)

# load
population <- read.csv("population.csv")
str(population)
population <- population[,c("city", "agecat", "population")]
# do
population_summary <- ddply(population,
                           .variables = c("city",
                               "agecat"),
                           .fun = summarise,
                           sum(population)
                           )

names(population_summary) <- c("city","agecat","pop")

write.csv(population_summary,
          "population_summary.csv",
          row.names = FALSE
          )

################################################################
# name:merge
# func
setwd("~/projects/spatiotemporal-regression-models/NMMAPS-example")

# load
outcome <- read.csv("outcome.csv")
str(outcome)
outcome$date <- as.Date(outcome$date)

population <- read.csv("population_summary.csv")
str(population)
population

# do
analyte <- merge(outcome, population)
write.csv(analyte, "analyte.csv", row.names = FALSE)

################################################################
# name:eda-tsplots
# func
setwd("~/projects/spatiotemporal-regression-models/NMMAPS-example")
  
# load
flist
fname <- flist[7]; print(fname)
df <- read.csv(file.path("data", fname))

# clean
str(df)
summary(df$tmax); summary(df$dptp)

# do
## we will only consider cities with long periods temp and humidity observed
png("images/nmmaps-eg-dateranges.png", width = 1000, height=500, res = 150)
par(mfrow=c(6,5), mar=c(0,3,3,0), cex=.25)
for(fi in 1:length(flist))
    {
        #fi <- 4
        fname <- flist[fi]
        df <- read.csv(file.path("data", fname))
        print(fi); print(fname);
        with(df, plot(as.Date(date), tmax, type = "l"))
        title(paste(fname, "tmax"))
        with(df, plot(as.Date(date), dptp, type = "l"))
        title(paste(fname, "dptp"))
    }
dev.off()
