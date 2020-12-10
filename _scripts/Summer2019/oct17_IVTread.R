## goal: read IVT file from SIO-R1 catalog
## http://cw3e.ucsd.edu/Publications/SIO-R1-Catalog/

## helpful info: http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html
require(ncdf4)
require(lubridate)

AR <- nc_open('C:/Users/cbowers/Downloads/IVT_2006.nc')
AR

lat <- ncvar_get(AR, 'lat')
lon <- ncvar_get(AR, 'lon') - 360

timevar <- ncvar_get(AR, 'time')
ncatt_get(AR, 'time', 'units')
timevar <- ymd('1800-01-01') + hours(timevar)

IVT <- ncvar_get(AR, 'IVT')
IVT[,,1]


require(rgdal)
require(ggplot2)
require(ggmap)
require(rgeos)
require(maptools)

latlon <- get_map(location = c(lon = mean(lon), lat = mean(lat)))
map <- ggmap(latlon) 

IVT_fortify <- fortify(data.frame(IVT[,,1]))

# map + ggplot(data = IVT_fortify)

require(raster)

IVT_raster <- raster(IVT[,,1], xmn = min(lon), xmx = max(lon), ymn = min(lat), ymx = max(lat))

plot(IVT_raster)

library(rworldmap)
newmap <- getMap(resolution = "low")

plot(newmap)
plot(newmap, xlim = range(lon))
plot(newmap, ylim = c(-4, 4), asp = 1)

plot(newmap, xlim = range(lon), ylim = range(lat), colourPalette = c('red', 'blue'))





CBG_geo <- readOGR(dsn="C:/Users/Corinne/OneDrive/classes/big data for cities/unit 2/module 07/Block Groups 2015 BARI", layer="Census Block Groups")
CBG_fortify <- fortify(CBG_geo, region = "BG_ID_10")
CBG_geo <- merge(CBG_fortify, CBG_geo, by.x='id', by.y='BG_ID_10', all.x=TRUE)
CBG_geo <- CBG_geo[order(CBG_geo$order),]
CBG_geo <- merge(CBG_geo, indicators, by.x='id', by.y='BG_ID_10', all.x=TRUE)
CBG_geo <- CBG_geo[order(CBG_geo$order),] 

Boston <- get_map(location=c(left = -71.193799, bottom = 42.15, right =
                                 -70.985746, top = 42.5))
Bostonmap <- ggmap(Boston) 

