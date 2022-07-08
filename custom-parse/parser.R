parseGpsText <- function(file) {

      # Parse a plain text (whitespace separate values) or csv file
      # containing animal GPS tracking data. Also renames columns to
      # use a standard set of column names - x, y, z, lat, long, time,
      # id. If time is in timestamp format, convert to minutes from
      # first reading.
      #
      # Returns a data frame with appropriately renamed columns.

      library(tools)
      library(move)

      # Determine file type and then load data
      
      ext <- file_ext(file)
      if (ext == "csv") { 
      	 gpsdata <- read.csv(file, header=TRUE)
      } else if (ext == "txt") {
      	 gpsdata <- read.table(file, header=TRUE)
      } else { 
      	print("Unknown file extension")
      	# implement error handling
      }

      # Rename column names to conform to x, y, z, lat, long, t, id
      
      names(gpsdata)[names(gpsdata) == 'X'] <- 'x'
      names(gpsdata)[names(gpsdata) == 'Y'] <- 'y'

      names(gpsdata)[names(gpsdata) == 'Z'] <- 'z'
      names(gpsdata)[names(gpsdata) == 'height-raw'] <- 'z'
      names(gpsdata)[names(gpsdata) == 'height_raw'] <- 'z'
      names(gpsdata)[names(gpsdata) == 'height.raw'] <- 'z'

      names(gpsdata)[names(gpsdata) == 'Lat'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'LAT'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'location_lat'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'Location_lat'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'LOCATION_LAT'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'location-lat'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'Location-lat'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'LOCATION-LAT'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'location.lat'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'Location.lat'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'LOCATION.LAT'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'latitute'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'Latitute'] <- 'lat'
      names(gpsdata)[names(gpsdata) == 'LATITUDE'] <- 'lat'

      names(gpsdata)[names(gpsdata) == 'Long'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'LONG'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'location_long'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'Location_long'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'LOCATION_LONG'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'location-long'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'Location-long'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'LOCATION-LONG'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'location.long'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'Location.long'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'LOCATION.LONG'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'longitude'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'Longitude'] <- 'long'
      names(gpsdata)[names(gpsdata) == 'LONGITUDE'] <- 'long'

      names(gpsdata)[names(gpsdata) == 't'] <- 'time'
      names(gpsdata)[names(gpsdata) == 'T'] <- 'time'
      names(gpsdata)[names(gpsdata) == 'Time'] <- 'time'
      names(gpsdata)[names(gpsdata) == 'TIME'] <- 'time'
      names(gpsdata)[names(gpsdata) == 'timestamp'] <- 'time'
      names(gpsdata)[names(gpsdata) == 'Timestamp'] <- 'time'
      names(gpsdata)[names(gpsdata) == 'TIMESTAMP'] <- 'time'
      names(gpsdata)[names(gpsdata) == 'date'] <- 'time'
      names(gpsdata)[names(gpsdata) == 'Date'] <- 'time'
      names(gpsdata)[names(gpsdata) == 'DATE'] <- 'time'

      names(gpsdata)[names(gpsdata) == 'Id'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'ID'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'individual_id'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'Individual_id'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'INDIVIDUAL_ID'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'individual-id'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'Individual-id'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'INDIVIDUAL-ID'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'individual.id'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'Individual.id'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'INDIVIDUAL.ID'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'identifier'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'Identifier'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'IDENTIFIER'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'individual-local-identifier'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'individual_local_identifier'] <- 'id'
      names(gpsdata)[names(gpsdata) == 'individual.local.identifier'] <- 'id'

      names(gpsdata)[names(gpsdata) == 'easting'] <- 'utm-easting'
      names(gpsdata)[names(gpsdata) == 'easting'] <- 'utm_easting'
      names(gpsdata)[names(gpsdata) == 'easting'] <- 'utm.easting'

      names(gpsdata)[names(gpsdata) == 'northing'] <- 'utm-northing'
      names(gpsdata)[names(gpsdata) == 'northing'] <- 'utm_northing'
      names(gpsdata)[names(gpsdata) == 'northing'] <- 'utm.northing'


      # Make sure that we have the required columns
      
      if ("time" %in% colnames(gpsdata) &&
      	  ( ("x"   %in% colnames(gpsdata)   && "y"    %in% colnames(gpsdata)) ||
      	    ("lat" %in% colnames(gpsdata) && "long" %in% colnames(gpsdata)) )) {
	 # All is good
      } else {
      	 print("Missing required columns; must have time and lat-long or x-y")
	 # Implement error handling
      }
	  
      # If we have lat-long data without x-y data, do conversion to
      # aeqd (Azimuthal Equidistance) projection. This step is a
      # little convoluted since we need to convert to a "move" object
      # to use the spTransform() function, convert back to data frame
      # and then extract converted columns. Note the the conversion
      # function puts the new x data into column coords.x2 and new y
      # data into coords.x1. In the future, should explore how this
      # can be done more simply
      
      if(("lat" %in% colnames(gpsdata) && "long" %in% colnames(gpsdata)) &&
      	 (!"x" %in% colnames(gpsdata)  && !"y"   %in% colnames(gpsdata))) {
	 if (class(gpsdata$time) == "character") {
	     gpsmb <- move(x=gpsdata$long, y=gpsdata$lat,
	                   time=as.POSIXct(gpsdata$time, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
	                   proj=CRS("+proj=longlat +ellps=WGS84"))
             gpsmb <- spTransform(gpsmb, center=TRUE)
	 } else {
	     gpsmb <- move(x=gpsdata$long, y=gpsdata$lat,
	                   time=as.POSIXct(gpsdata$time, origin = "1970-01-01"),
	                   proj=CRS("+proj=longlat +ellps=WGS84"))
             gpsmb <- spTransform(gpsmb, center=TRUE)

         }
	 tempdf <- as.data.frame(gpsmb)
	 gpsdata$x <- tempdf$coords.x2
	 gpsdata$y <- tempdf$coords.x1
	 rm(gpsmb)
	 rm(tempdf)
      }

      # Convert timestamp to epoch time (minutes since 1/1/1970)

      if (class(gpsdata$time) == "character") {
      	 gpsdata$time <- as.numeric(as.POSIXct(gpsdata$time)) / 60
      }

      # Return a data frame containing x, y and t (minutes), plus
      # optionally id and z. This format can then easily be used

      return(gpsdata)
}
