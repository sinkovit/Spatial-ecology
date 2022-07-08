parseGpsText <- function(file) {

# Parse a plain text (whitespace separate values) or csv file
# containing animal GPS tracking data and rename columns to use a
# canonical set of names - x, y, z, lat, long, time, id, utm.zone,
# utm.easting, utm.northing

# Note that not all data sets will have all columns, but they do need
# to contain time and some specification of spatial coordinate (x-y,
# lat-long and/or UTM)

# If time is in POXSIX timestamp format, convert to minutes from start
# of epoch (1/1/1970)
#
# Add an 'id' column if the data set does not have one - this allows
# the processing routines to loop over animals without having to check
# if the column exists

# Parse a plain text (whitespace separate values) or csv file
# containing animal GPS tracking data and rename columns to use a
# canonical set of names - x, y, z, lat, long, time, id utm.zone,
# utm.easting, utm.northing Note that not all data sets will have all
# columns, but they do need to contain time and some specification of
# spatial coordinate (x-y, lat-long and/or UTM) If time is in
# timestamp format, convert to minutes from first reading Add an 'id'
# column if the data set does not have one - this allows the
# processing routines to loop over animals without having to check if
# the column exists

# Function returns a data frame with appropriately renamed columns

      library(tools)
      library(move)

      #### Determine file type and then load data
      
      ext <- file_ext(file)
      if (ext == "csv") { 
      	 gpsdata <- read.csv(file, header=TRUE)
      } else if (ext == "txt") {
      	 gpsdata <- read.table(file, header=TRUE)
      } else { 
      	print("Unknown file extension")
      	# implement error handling
      }

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

      # Vertical location (canonical name: z)
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
      	 print("Missing required columns; must have time and lat-long or x-y")
	 # Implement error handling
      }

      #### Convert time to epoch time (minutes since 1/1/1970)

      if (class(gpsdata$time) == "character") {
      	 gpsdata$time <- as.numeric(as.POSIXct(gpsdata$time)) / 60
      }

      #### Add an 'id' column if it doesn't already exist

      if (!"id" %in% colnames(gpsdata)) {
      	 gpsdata['id'] <- 1
      }

      #### Choose/set horizonal spatial coordinates

      # The data set may contain redundant spatial data in multiple
      # formats. For example, csv files downloaded from Movebank will
      # contain lat-long plus optionally UTM coordinates. Here we
      # impose the following precedence rules
      #
      # (1) UTM coordinates
      # (2) x-y coordinates
      # (3) lat-long, converted to UTM

      # If we have lat-long data without x-y or UTM data, do
      # conversion to UTM. This step is a little convoluted since we
      # need to convert to a "move" object to use the spTransform()
      # function, convert back to data frame and then extract
      # UTM data

      if(("lat" %in% colnames(gpsdata) && "long" %in% colnames(gpsdata)) &&
      	 (!"utm.easting" %in% colnames(gpsdata) && !"utm.northing" %in% colnames(gpsdata) && !"utm.zone" %in% colnames(gpsdata)) &&
      	 (!"x" %in% colnames(gpsdata)  && !"y" %in% colnames(gpsdata))) {

	 # Determine the UTM zone
	 # See https://apollomapping.com/blog/gtm-finding-a-utm-zone-number-easily
	 utm <- ceiling((180 + min(gpsdata$long))/6)

	 # Create a move object from data frame
	 gpsmb <- move(x=gpsdata$long, y=gpsdata$lat,
	               time=as.POSIXct(gpsdata$time, origin = "1970-01-01"),
	 	       proj=CRS("+proj=longlat +ellps=WGS84"))

         # Transform lat-long to UTM easting and northing
	 # spTransform puts easting and northing into new columns coords.x1 and coords.x2
         crs.str <- paste("+proj=utm +zone=", utm, " +datum=WGS84", sep="")
         data.utm <- spTransform(gpsmb, CRSobj=crs.str)

	 # Extract the UTM data and add to gpsdata data frame
	 tempdf <- as.data.frame(data.utm)
	 gpsdata$utm.easting <- tempdf$coords.x1
	 gpsdata$utm.northing <- tempdf$coords.x2
	 if (mean(gpsdata$lat) > 0.0) {
	    utm <- paste(utm, "N", sep="")
	 } else {
	    utm <- paste(utm, "S", sep="")
	 }	 
	 gpsdata$utm.zone <- utm
	 rm(gpsmb)
	 rm(tempdf)
      }

      #### Delete the column names that we don't need

      # This isn't strictly necessary and we may decide to revisit
      # later, but it can eliminate some confusion over which columns
      # to subsequently use.

      canonical <- c('x', 'y', 'z', 'id', 'time', 'lat', 'long', 'utm.easting', 'utm.northing', 'utm.zone')
      for (name in colnames(gpsdata)) {
      	  if (!name %in% canonical) {
	     gpsdata[name] <- NULL
     	  }
      }

      return(gpsdata)
}
