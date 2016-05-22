
"
http://cran.r-project.org/web/packages/NMMAPSlite/index.html
Package ‘NMMAPSlite’ was removed from the CRAN repository.
Formerly available versions can be obtained from the archive.
Archived on 2013-05-11 at the request of the maintainer.

The Archived versions do not seem to work either.

That is such a shame, lucky I saved some of the data using the following code:
"

#### Code: get nmmaps data
# func
if(!require(NMMAPSlite)) install.packages('NMMAPSlite');require(NMMAPSlite)

######################################################
# load  
setwd('data')
initDB('data/NMMAPS') # this requires that we connect to the web,
                      # so lets get local copies
setwd('..')
cities <- getMetaData('cities')
head(cities)
citieslist <- cities$cityname
# write out a few cities for access later
for(city_i in citieslist[sample(1:nrow(cities), 9)])
{
 city <- subset(cities, cityname == city_i)$city
 data <- readCity(city)
 write.table(data, file.path('data', paste(city_i, '.csv',sep='')),
 row.names = F, sep = ',')
}
# these are all tiny, go some big ones
for(city_i in c('New York', 'Los Angeles', 'Madison', 'Boston'))
{
 city <- subset(cities, cityname == city_i)$city
 data <- readCity(city)
 write.table(data, file.path('data', paste(city_i, '.csv',sep='')),
 row.names = F, sep = ',')
}
