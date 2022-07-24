preprocessDataframe <- function(gpsdata) {

# Pre-processes a data frame containing telemetry data.

# (1) Rename columns to use a canonical set of names - x, y, zdata,
# lat, long, time, id utm.zone, utm.easting, utm.northing. Note that
# not all data sets will have all columns, but they do need to contain
# time and some specification of spatial coordinate (x-y, lat-long
# and/or UTM)

# (2) If time is in POSIX timestamp format, convert to minutes from
# start of epoch (1/1/1970)

# (3) Add an 'id' column if the data set does not have one. This
# allows the processing routines to loop over animals even if the data
# set only contains a single animal

# (4) If necessary, generate UTM coordinates from latitudes and
# longitudes

# (5) If we have UTM data, then rename utm.[easting|northing] to
# [x|y]data. Otherwise rename the x and y columns to [x|y]data

      library(sp)

      #### Rename column names to conform to canonical names

      # Horizontal coordinates (canonical name: x and y)
      names(gpsdata)[names(gpsdata) == 'X'] <- 'x'
      names(gpsdata)[names(gpsdata) == 'Y'] <- 'y'

      # Horizontal UTM coordinates (canonical name: utm.[easting|northing])
      names(gpsdata)[names(gpsdata) == 'utm-easting'] <- 'utm.easting'
      names(gpsdata)[names(gpsdata) == 'utm_easting'] <- 'utm.easting'
      names(gpsdata)[names(gpsdata) == 'utm-northing'] <- 'utm.northing'
      names(gpsdata)[names(gpsdata) == 'utm_northing'] <- 'utm.northing'

      # UTM zone (canonical name: utm.zone)
      options <- c('Z', 'utm-zone', 'utm_zone', 'utmzone', 'zone')
      for (opt in options) {
      	  names(gpsdata)[names(gpsdata) == opt] <- 'utm.zone'
      }

      # Vertical location (canonical name: zdata)
      options <- c('Z', 'height-raw', 'height_raw', 'height.raw')
      for (opt in options) {
      	  names(gpsdata)[names(gpsdata) == opt] <- 'z'
      }

      # Latitude (canonical name: lat)
      options <- c('Lat', 'LAT', 'location_lat', 'Location_lat', 'LOCATION_LAT',
      	      	   'location-lat', 'Location-lat', 'LOCATION-LAT', 'location.lat',
		   'Location.lat', 'LOCATION.LAT', 'latitute', 'Latitute', 'LATITUDE')
      for (opt in options) {
      	  names(gpsdata)[names(gpsdata) == opt] <- 'lat'
      }

      # Longitude (canonical name: long)
      options <- c('Long', 'LONG', 'location_long', 'Location_long',
                  'LOCATION_LONG', 'location-long', 'Location-long', 'LOCATION-LONG',
	   	  'location.long', 'Location.long', 'LOCATION.LONG', 'longitude',
	    	  'Longitude', 'LONGITUDE')
      for (opt in options) {
      	  names(gpsdata)[names(gpsdata) == opt] <- 'long'
      }

      # Time (canonical name: time)
      options <- c('t', 'T', 'Time', 'TIME', 'timestamp', 'Timestamp',
                   'TIMESTAMP', 'date', 'Date', 'DATE')
      for (opt in options) {
      	  names(gpsdata)[names(gpsdata) == opt] <- 'time'
      }

      # animal identifier (canonical name: id)
      options <- c('Id', 'ID',
      	           'identifier', 'Identifier', 'IDENTIFIER',
		   'individual-local-identifier', 'individual_local_identifier', 'individual.local.identifier',
		   'local-identifier', 'local_identifier', 'local.identifier')
      for (opt in options) {
      	  names(gpsdata)[names(gpsdata) == opt] <- 'id'
      }

      #### Test that data set has the required columns - time plus complete spatial coordinates
      
      if ("time" %in% colnames(gpsdata) &&
      	  ( ("x" %in% colnames(gpsdata) && "y" %in% colnames(gpsdata)) ||
      	    ("utm.easting" %in% colnames(gpsdata) && "utm.northing" %in% colnames(gpsdata) && "utm.zone" %in% colnames(gpsdata)) ||
      	    ("lat" %in% colnames(gpsdata) && "long" %in% colnames(gpsdata)) )) {
	 # All is good
      } else {
      	 print("Missing required columns; must have time and lat-long, x-y or UTM")
	 # Implement error handling
      }

      #### If necessary convert POSIX time to epoch time (minutes since 1/1/1970)

      if (!is.numeric(gpsdata$time)) {
      	 gpsdata$time <- as.numeric(as.POSIXct(gpsdata$time)) / 60
      }

      #### Add an 'id' column if it doesn't already exist

      if (!"id" %in% colnames(gpsdata)) {
      	 gpsdata['id'] <- 1
      }

      #### Generate UTM coordinates from lat-long

      # If we have lat-long data without UTM data, do conversion to
      # UTM using sp package. Note that call to proj4string() gives
      # warning message: In showSRID(uprojargs, format = "PROJ",
      # multiline = "NO", prefer_proj = prefer_proj) : Discarded datum
      # Unknown based on WGS84 ellipsoid in Proj4 definition

      if(("lat" %in% colnames(gpsdata) && "long" %in% colnames(gpsdata)) &&
      	 (!"utm.easting" %in% colnames(gpsdata) && !"utm.northing" %in% colnames(gpsdata) && !"utm.zone" %in% colnames(gpsdata))) {

	 # Determine the UTM zone
	 # See https://apollomapping.com/blog/gtm-finding-a-utm-zone-number-easily
	 utm <- ceiling((180 + min(gpsdata$long))/6)

	 # Conversion using sp package
	 gpsdata.latlong <- gpsdata[c("lat", "long")]
	 sp::coordinates(gpsdata.latlong) <- ~long+lat
	 sp::proj4string(gpsdata.latlong) <- CRS("+proj=longlat +ellps=WGS84")
	 crs.str <- paste("+proj=utm +zone=", utm, " +datum=WGS84", sep="")
	 gpsdata.latlong <- sp::spTransform(gpsdata.latlong, CRSobj=crs.str)
	 gpsdata$utm.easting <- gpsdata.latlong$long
	 gpsdata$utm.northing <- gpsdata.latlong$lat
	 rm(gpsdata.latlong)

	 # Add N/S to UTM zone. This has to be done after conversion
         # since sp package expects an integer value for zone
	 
	 if (mean(gpsdata$lat) > 0.0) {
	    utm <- paste(utm, "N", sep="")
	 } else {
	    utm <- paste(utm, "S", sep="")
	 }	 
	 gpsdata$utm.zone <- utm
      }

      #### Delete the column names that we don't need

      # This isn't strictly necessary and we may decide to revisit
      # later, but it can eliminate some confusion over which columns
      # to subsequently use.

      canonical <- c('x', 'y', 'xdata', 'ydata', 'zdata', 'id', 'time', 'lat', 'long',
                      'utm.easting', 'utm.northing', 'utm.zone')
      for (name in colnames(gpsdata)) {
      	  if (!name %in% canonical) {
	     gpsdata[name] <- NULL
     	  }
      }

      #### Rename UTM or x-y data to xdata and ydata

      # Having columns named [xy]data simplifies the rest of the data
      # processing since the we won't need to do tests to see which
      # columns are available (e.g., do we have UTM or xy?). Use UTM
      # data if we have it, otherwise use xy

      if ("utm.easting" %in% colnames(gpsdata) && "utm.northing" %in% colnames(gpsdata)) {
      	 names(gpsdata)[names(gpsdata) == 'utm.easting'] <- 'xdata'
      	 names(gpsdata)[names(gpsdata) == 'utm.northing'] <- 'ydata'
      } else {
      	 names(gpsdata)[names(gpsdata) == 'x'] <- 'xdata'
      	 names(gpsdata)[names(gpsdata) == 'y'] <- 'ydata'
      }

      return(gpsdata)
}
