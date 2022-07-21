library(sp)

file <- "Data/t-lat-long-id.csv"
#file <- "Data/Condor-movebank-noutm.csv"
gpsdata <- read.csv(file, header=TRUE)
names(gpsdata)[names(gpsdata) == 'location_long'] <- 'long'
utm <- 11


gpsdata.latlong <- gpsdata[c("lat", "long")]
sp::coordinates(gpsdata.latlong) <- ~long+lat
sp::proj4string(gpsdata.latlong) <- CRS("+proj=longlat +ellps=WGS84")
crs.str <- paste("+proj=utm +zone=", utm, " +datum=WGS84", sep="")
gpsdata.latlong <- sp::spTransform(gpsdata.latlong, CRSobj=crs.str)
gpsdata$easting <- gpsdata.latlong$long
gpsdata$northing <- gpsdata.latlong$lat
rm(gpsdata.latlong)
