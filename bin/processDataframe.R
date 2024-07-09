# This software is Copyright 2022 The Regents of the University of California.
# All Rights Reserved. Permission to copy, modify, and distribute this software
# and its documentation for educational, research and non-profit purposes,
# without fee, and without a written agreement is hereby granted, provided that
# this entire copyright appear in all copies. Permission to make commercial use
# of this software may be obtained by contacting:
# 
# Office of Innovation and Commercialization
# 9500 Gilman Drive, Mail Code 0910
# University of California
# La Jolla, CA 92093-0910
# (858) 534-5815
# invent@ucsd.edu
#
# This software program and documentation are copyrighted by The Regents of the
# University of California. The software program and documentation are supplied
# “as is”, without any accompanying services from The Regents. The Regents does
# not warrant that the operation of the program will be uninterrupted or
# error-free. The end-user understands that the program was developed for
# research purposes and is advised not to rely exclusively on the program for
# any reason.
# 
# IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
# DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING
# LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION,
# EVEN IF THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE. THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE PROVIDED
# HEREUNDER IS ON AN “AS IS” BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO
# OBLIGATIONS TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.

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

# Returns a list where first item is the data and second item is the error
# message; if successful, there error message = NULL and data will be
# populated; if there is an error message, then data = NULL

preprocessDataframe <- function(gpsdata) {

  #### Rename column names to conform to canonical names
  
  # Horizontal coordinates (canonical name: x and y)
  names(gpsdata)[names(gpsdata) == 'X'] <- 'x'
  names(gpsdata)[names(gpsdata) == 'Y'] <- 'y'

  # Horizontal UTM coordinates (canonical name: utm.[easting|northing])
  names(gpsdata)[names(gpsdata) == 'utm-easting'] <- 'utm.easting'
  names(gpsdata)[names(gpsdata) == 'utm_easting'] <- 'utm.easting'
  names(gpsdata)[names(gpsdata) == 'easting'] <- 'utm.easting'
  names(gpsdata)[names(gpsdata) == 'Easting'] <- 'utm.easting'
  names(gpsdata)[names(gpsdata) == 'utm-northing'] <- 'utm.northing'
  names(gpsdata)[names(gpsdata) == 'utm_northing'] <- 'utm.northing'
  names(gpsdata)[names(gpsdata) == 'northing'] <- 'utm.northing'
  names(gpsdata)[names(gpsdata) == 'Northing'] <- 'utm.northing'
  
  # UTM zone (canonical name: utm.zone)
  options <- c('utm-zone', 'utm_zone', 'utmzone', 'zone')
  for (opt in options) {
    names(gpsdata)[names(gpsdata) == opt] <- 'utm.zone'
  }
  
  # Vertical location (canonical name: zdata)
  options <- c('Zdata', 'ZDATA', 'z', 'Z', 'height-raw', 'height_raw', 'height.raw')
  for (opt in options) {
    names(gpsdata)[names(gpsdata) == opt] <- 'zdata'
  }
  
  # Latitude (canonical name: lat)
  options <- c('Lat', 'LAT', 'location_lat', 'Location_lat', 'LOCATION_LAT',
               'location-lat', 'Location-lat', 'LOCATION-LAT', 'location.lat',
               'Location.lat', 'LOCATION.LAT', 'latitude', 'Latitude', 'LATITUDE')
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
  options <- c('t', 'T',
               'Time', 'TIME',
               'timestamp', 'Timestamp', 'TIMESTAMP',
               'date', 'Date', 'DATE',
               'datetime', 'DateTime', 'DATETIME',
               'date_time', 'Date_Time', 'DATE_TIME')
  for (opt in options) {
    names(gpsdata)[names(gpsdata) == opt] <- 'time'
  }
  
  # Cummulative time (canonical name: cummultime)
  options <- c('CummulTime', 'CUMMULTIME')
  for (opt in options) {
    names(gpsdata)[names(gpsdata) == opt] <- 'cummultime'
  }
  
  # animal identifier (canonical name: id)
  options <- c('Id', 'ID',
               'identifier', 'Identifier', 'IDENTIFIER',
               'individual-local-identifier', 'individual_local_identifier', 'individual.local.identifier',
               'local-identifier', 'local_identifier', 'local.identifier')
  for (opt in options) {
    names(gpsdata)[names(gpsdata) == opt] <- 'id'
  }
  
  #### If cummultime and time are both present, delete time and rename cummultime to time
  #### If cummultime is present and time absent, just rename cummultime to time
  
  if ("cummultime" %in% colnames(gpsdata)) {
    if ("time" %in% colnames(gpsdata)) {
      # Not sure why "gpsdata['time'] <- NULL" doesn't work here
      # Therefore have to use following bit of code
      gpsdata <- gpsdata[ , !(names(gpsdata) %in% c("time"))]
    }
    names(gpsdata)[names(gpsdata) == 'cummultime'] <- 'time'
  }

  #### Test that data set has the required columns - time plus complete spatial coordinates
  #### No longer testing for UTM zone since it's entered from GUI
  
  if ("time" %in% colnames(gpsdata) &&
      ( ("x" %in% colnames(gpsdata) && "y" %in% colnames(gpsdata)) ||
        ("xdata" %in% colnames(gpsdata) && "ydata" %in% colnames(gpsdata)) ||
        ("utm.easting" %in% colnames(gpsdata) && "utm.northing" %in% colnames(gpsdata)) ||
        ("lat" %in% colnames(gpsdata) && "long" %in% colnames(gpsdata)) )) {
    # All is good
  } else {
    return(list(NULL,
                "Missing required columns; must have time and lat-long, x-y or UTM",
                NULL))
  }

  #### If necessary convert POSIX time to epoch time (minutes since 1/1/1970)
  #### Set timezone to UTC to avoid daylight savings time ambiguities
  
  if (!is.numeric(gpsdata$time)) {
    gpsdata$time <- unclass(as.POSIXct(gpsdata$time,
                                       tryFormats = c("%m/%e/%y %H:%M", "%Y-%m-%d %H:%M:%S"),
                                       tz="UTC")) / 60
  }
  
  #### Add an 'id' column if it doesn't already exist and convert to string
  #### Handing as string adds flexibility since not all data sets will use
  #### integers to label animals
  
  if (!"id" %in% colnames(gpsdata)) {
    gpsdata['id'] <- 1
  }
  gpsdata$id <- as.character(gpsdata$id)
  
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
    
    # Extracting latitude and longitude data
    gpsdata.latlong <- gpsdata[c("lat", "long")]
    gpsdata.sf <- sf::st_as_sf(gpsdata.latlong, coords = c("long", "lat"), crs = 4326)
    
    # Transforming coordinates to UTM
    crs.str <- paste("+proj=utm +zone=", utm, " +datum=WGS84", sep="")
    gpsdata.latlong <- sf::st_transform(gpsdata.sf, crs = crs.str)
    
    if ('long' %in% colnames(sf::st_coordinates(gpsdata.latlong))) {
      gpsdata$utm.easting <- sf::st_coordinates(gpsdata.latlong)[,1]
    } else {
      gpsdata$utm.easting <- sf::st_coordinates(gpsdata.latlong)[,1]
    }
    
    if ('lat' %in% colnames(sf::st_coordinates(gpsdata.latlong))) {
      gpsdata$utm.northing <- sf::st_coordinates(gpsdata.latlong)[,2]
    } else {
      gpsdata$utm.northing <- sf::st_coordinates(gpsdata.latlong)[,2]
    }
    
    rm(gpsdata.latlong)
  
    utm <- ifelse(base::mean(gpsdata$lat) > 0.0, paste(utm, "N", sep=""), paste(utm, "S", sep=""))
    gpsdata$utm.zone <- utm
    
  }

  #### Generate list of UTM zones found in data file
  found.zones <- base::unique(gpsdata$utm.zone)
  
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
  
  #print(summary(gpsdata))
  return(list(gpsdata, NULL, found.zones))
}
