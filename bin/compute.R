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

# From Bob's code
# @ https://github.com/sinkovit/Spatial-ecology/blob/Bob/code-snippets/animal_stats.R
# The following code snippet shows how to list the stats for each
# animal and for the data set as a whole. It also provides advice on
# choosing pixel size.

# data_df is the data frame containg data that has already been
# transformed so that location_[lat|long].1 contain the azimuthal
# equidistance projection and t is time in minutes relative to the
# first observation in the entire data set

# Note 1 - I'm using the R range() function for both performance and
# to maintain more compact code. It returns a vector of min and max
# values

# Note 2 - code is written assuming that data has already been
# loaded. The animalAttributes() function is first defined and call to
# function is made at bottom of file
# --------------------------------------------------------------------

animalAttributes <- function(data_df, areaUnits) {
  # print("entered animalAttributes()")
  # print(paste("data_df =", summary(data_df)))
  # print(paste("areadUnits =", areaUnits))
  printf <- function(...) cat(sprintf(...))
  
  gpsdata <- data_df[, c("xdata", "ydata")]
  # print(paste("gpsdata =", summary(gpsdata)))
  
  area.convexhull <- function(points) {
    
    if(base::nrow(points) < 3) {  
      
      return(0)
      
    }
    
    points <- rbind(points, points[1, ])  

    points <- apply(points, 2, as.numeric)
    shoelace.sum <- sum(points[1:(base::nrow(points)-1), 1] * points[2:(base::nrow(points)), 2] - 
                          points[2:(base::nrow(points)), 1] * points[1:(base::nrow(points)-1), 2])
    return(abs(shoelace.sum) / 2)
  }
  
  convertArea <- function(area, units) {
    switch(units,
           "km2" = area / (10^6),    
           "m" = area,              
           "ha" = area / (10^4),    
           area                     
    )
  }

  animals <- base::as.list(sort(base::unique(data_df$id)))
  max_pixels <- c(30, 60, 100, 300)
  areaString <- paste("MCP Area (", areaUnits, ")", sep="")

  result <- data.frame(id = numeric())
  result[, 'Easting (min)'] <- numeric()
  result[, 'Easting (max)'] <- numeric()
  result[, 'Northing (min)'] <- character()
  result[, 'Northing (max)'] <- character()
  result[, areaString] <- numeric()
  row.index <- 1

  tryCatch({
    for (local_id in animals) {
      # print(paste("local_id =", local_id))

      animal.data <- gpsdata[which(data_df$id == local_id), ]
      # print(paste("animal.data length =", length(animal.data)))
      # print(paste("animal.data class =", class(animal.data)))
      # print(paste("head 1 =", head(animal.data[,1])))
      # print(paste("head 2 =", head(animal.data[,2])))
      
      hull.indices <- chull(animal.data[, 1], animal.data[, 2])
      hull.points <- animal.data[hull.indices, ]

      x_minmax <- range(animal.data[, 1])
      y_minmax <- range(animal.data[, 2])
      t_minmax <- range(data_df[which(data_df$id == local_id), "time"])
      x_range <- x_minmax[2] - x_minmax[1]
      y_range <- y_minmax[2] - y_minmax[1]
      t_range <- t_minmax[2] - t_minmax[1]

      area <- area.convexhull(hull.points)

      if (is.na(area)) {
        
        area <- 0  
        
      }
      
      area <- convertArea(area, areaUnits)
      area.str <- as.character(area)

      row <- c(local_id, round(x_minmax[1]), round(x_minmax[2]), round(y_minmax[1]), round(y_minmax[2]), area.str)
      row.tail = c()
      
      # Loop over pixel sizes
      for (max_pix in max_pixels) {
        
        nx <- as.integer(x_range/max_pix)
        ny <- as.integer(y_range/max_pix)
        dims <- sprintf("%dx%d", nx, ny)
        
        if(row.index == 1) {
          
          label <- sprintf("Grid \n (%dm)", max_pix)
          result[, label] <- character()
          
        }
        
        value <- paste(dims)
        row.tail <- append(row.tail, c(value))
        
      }
      
      row <- append(row,row.tail)
      result[row.index, ] <- row
      row.index <- row.index + 1
      
    }
  },
  error = function(error_message) {
    
    # base::print(paste("error_message =", error_message))
    return(NULL)
    
  })
  
  #base::print("leaving animalAttributes()")
  return(result)
}

# Calculate rasters for each indivdual in a dataframe using mkde
# package and return the results as a list of rasters
#
# gpsdata - data frame containing animal movement data
# sig2obs - location error variance
# t.max - maximum time between locations
# cell.sz - cell size
# xmin, xmax - min/max x coordinate
# ymin, ymax - min/max y coordinate
#
# returns string if there is an error or a list of rasters if successful
calculateRaster2D <- function (gpsdata, id, sig2obs, t.max, cell.sz, buffer) {

  x <- gpsdata[which(gpsdata$id == id), "xdata"]
  y <- gpsdata[which(gpsdata$id == id), "ydata"]
  t <- gpsdata[which(gpsdata$id == id), "time"]
  
  # determine if max time is big enough...
  min_maxt <- find_minimum_maxt(t)
  if (min_maxt > t.max)
    return(paste("Max time must be >=", min_maxt, "for animal id", id))

  xmin = min(x) - buffer
  xmax = max(x) + buffer 
  ymin = min(y) - buffer
  ymax = max(y) + buffer

  xrange <- xmax - xmin
  yrange <- ymax - ymin

  nx <- as.integer(xrange/cell.sz)
  ny <- as.integer(yrange/cell.sz)
  
  # base::print(paste("identifier =", id))
  # base::print("Home range dimensions (pixels/voxels)")
  # base::print(paste("nx =", nx, "ny =", ny))
  # base::print("Home range dimensions (meters)")
  # base::print(paste("xrange =", xrange, "yrange =", yrange))
  # base::print(paste("cell size =", cell.sz))
  # base::print("---------------------------------------")
  
  mv.dat <- initializeMovementData(t, x, y, sig2obs = sig2obs, t.max = t.max)
  mkde.obj <- initializeMKDE2D(xmin, cell.sz, nx, ymin, cell.sz, ny)
  dens.res <- initializeDensity(mkde.obj, mv.dat)
  mkde.obj <- dens.res$mkde.obj
  
  return (mkde.obj)
  
}

# mkdeToTerra converts an mkde object to a SpatRaster
mkdeToTerra <- function(mkde.obj) {
  
  sx <- mkde.obj$x[2] - mkde.obj$x[1]
  sy <- mkde.obj$y[2] - mkde.obj$y[1]
  
  if (mkde.obj$dimension == 2 | mkde.obj$dimension == 2.5) {
    
    # Calculate the extent using the calculated values
    extent <- c(min(mkde.obj$x) - 0.5 * sx, max(mkde.obj$x) + 0.5 * sx,
                min(mkde.obj$y) - 0.5 * sy, max(mkde.obj$y) + 0.5 * sy)
    
    rst <- terra::rast(nrow = mkde.obj$ny, ncol = mkde.obj$nx, xmin = extent[1], xmax = extent[2],
                ymin = extent[3], ymax = extent[4], crs = st_crs("+proj=longlat"))
    
    # Transpose and then flip vertically by reversing rows
    d_matrix <- base::t(matrix(mkde.obj$d[,,1], nrow = mkde.obj$nx, ncol = mkde.obj$ny))
    d_flipped <- d_matrix[base::nrow(d_matrix):1, ]
    
    # Assign the flipped matrix to the raster
    terra::values(rst) <- d_flipped
    
    # print(paste("str =", str(rst)))
    # print(paste("typeof =", typeof(rst)))
    # print(paste("attributes =", attributes(rst)))
    return(rst)
    
  } 
  
  else {
    
    # Not able to test this
    stop("Only 2D and 2.5D MKDE objects are supported")
    
  }
  
}

# terraToContour converts a SpatRaster to lat long
terraToContour <- function(terra.obj, levels, crsstr) {
  
  # Check if terra.obj is NULL or empty
  if (is.null(terra.obj)|| sum(!is.na(values(terra.obj))) == 0) {
    
    stop("SpatRaster object is empty or NULL.")
    
  }
  
  if (is.null(levels) || is.null(crsstr))
  {
    
    stop("Proper input must be provided.")
    
  }
  
  # Create contours from mkde_terra and set its CRS
  terra_contour <- terra::as.contour(terra.obj, levels = levels)
  terra::crs(terra_contour) <- crsstr
  
  # Transform it to an sf object and set its CRS to longlat
  sf_contour <- sf::st_as_sf(terra_contour)
  sf_contour <- sf::st_transform(sf_contour, crs="+proj=longlat")
  
  return(sf_contour)
  
}

# Input
# mkde2d.obj: MKDE object
# probs: list of probabilities for image contours
# basename: base file name for raster and shape files
#           basename_[all|outer]contour.[asc|cpg|dbf|prj|shp|shx]
# all: if true, use all probability contours, if false use only outer contour
#
# Output
# Completion code (0 for success)
#
# Usage examples
# createContours(rasters[[1]], probs, "condor", 11, "WGS84", all=TRUE)
# createContours(mkde2d.obj, probs, "tejon-pig", 11, "NAD83", all=FALSE)
# createContour(rasters[[1]], c(0.99, 0.9, 0.8), "condor", 11, "WGS84", all=TRUE)
#
# Note: see https://mhallwor.github.io/_pages/basics_SpatialPolygons
createContour <- function(mkde2d.obj, probs, utm.zone, datum, buffer, all = TRUE) {

  # Setting bounds for map and zoom level
  xmin <- min(mkde2d.obj$x)
  ymin <- min(mkde2d.obj$y)
  xmax <- max(mkde2d.obj$x)
  ymax <- max(mkde2d.obj$y)
  area = (xmax-xmin)*(ymax-ymin) / 1000000000.0
  
  # Create terra raster from MKDE object
  dimension <- c(mkde2d.obj$nx, mkde2d.obj$ny, mkde2d.obj$nz)
  extent <- c(xmin-buffer, xmax+buffer, ymin-buffer, ymax+buffer)
  
  # Setting the default CRS
  crsstr <- paste("+proj=utm +zone=", utm.zone, " +datum=", datum, " +units=m +no_defs", sep="")
  
  # Calling mkdeToTerra to convert MKDE object to SpatRaster
  mkde_terra <- mkdeToTerra(mkde2d.obj)
  
  # Sort prob contours into ascending order and remove values <= 0
  #probs <- sort(probs[probs > 0])
  #probs_max <- raster::tail(probs, n=1)
  
  # Set filenames and contour probabilites based on whether all
  # contours or outer contour are used
  #if (all) {
    # filename <- paste(basename, "_allcontour", sep="")
  #  contour_probs <- probs
  #} else {
    # filename <- paste(basename, "_outercontour", sep="")
  #  contour_probs <- raster::tail(probs, n=1)
  #}
  # Sort prob contours into ascending order and remove values <= 0
  probs <- sort(probs[probs > 0])
  
  # Function to get the last element of a vector
  my_tail <- function(x, n = 1) {
    x[length(x) - (0:(n-1))]
  }
  
  # Get the last element of the sorted vector
  probs_max <- my_tail(probs, n = 1)
  
  # Set filenames and contour probabilities based on whether all contours or outer contour are used
  if (all) {
    # filename <- paste(basename, "_allcontour", sep="")
    contour_probs <- probs
  } else {
    # filename <- paste(basename, "_outercontour", sep="")
    contour_probs <- my_tail(probs, n = 1)
  }
  
  # Calculate contours
  cont <- computeContourValues(mkde2d.obj, prob = contour_probs)
  terra.cont <- terra::crop(mkde_terra, extent)
  
  #RSSRSS Start code to choose between original display and mkde on map
  fits <- TRUE
  
  if (area < 1.0) {
    zoom = 10
  } else if (area < 20.0) {
    zoom = 9
  } else {
    zoom = 8
  }
  
  gpsdata.sf <- data.frame(label=character(), x=double(), y=double())
  gpsdata.sf[1,] = list("dummy", xmin, ymin)
  gpsdata.sf[2,] = list("dummy", xmin, ymax)
  gpsdata.sf[3,] = list("dummy", xmax, ymin)
  gpsdata.sf[4,] = list("dummy", xmax, ymax)
  
  # Call terraToContour to convert SpatRaster to lat long
  sf_contour <- terraToContour(mkde_terra, cont$threshold, crsstr)

  # St_Cast is called to avoid using sfc_GEOMETRY
  gpsdata.sfgeo <- st_cast(st_geometry(sf_contour))
  
  # Convert the coordinate data to a dataframe
  coords <- as.data.frame(sf::st_coordinates(gpsdata.sfgeo))
  
  # Get the base map
  mean_lat <- base::mean(c(min(coords$Y), max(coords$Y)))
  
  # Adjust the bounding box by the buffer
  left <- adjustLongByMeters(min(coords$X), mean_lat, -buffer)
  right <- adjustLongByMeters(max(coords$X), mean_lat, buffer)
  bottom <- adjustLatByMeters(min(coords$Y), -buffer)
  top <- adjustLatByMeters(max(coords$Y), buffer)
  
  # Use the adjusted coordinates to get the base map with an appropriate buffer
  mybasemap <- get_stadiamap(bbox = c(left = left, bottom = bottom, right = right, top = top), zoom = zoom)
  
  # Add the MKDE results to the map
  mymap <- ggmap(mybasemap) +
    geom_polygon(aes(x = X, y = Y, group = paste(L2, L1, sep = "_")), data = coords,
                 alpha = 0.25, linewidth = 0.1, color = "black", fill = "blue")
  
  tolerance = 0.1
  
  # Check to see if contour fits on map
  for (gname in unique(coords$L1)) {
    x1 <-  coords$Y[which(coords$L1 == gname)]
    y1 <-  coords$X[which(coords$L1 == gname)]
    if (abs(x1[1] - x1[length(x1)]) > tolerance || abs(y1[1] - y1[length(y1)]) > tolerance) {
      # print(abs(x1[1] - x1[length(x1)]))
      # print(abs(y1[1] - y1[length(y1)]))
      fits <- FALSE # Contour does not fit on map
    }
  }
  
  results <- list(raster = mkde_terra, contour = cont, cut = terra.cont, map = mymap,
                  probabilities = contour_probs, fits = fits)
  
  return(list(results, fits))

}

adjustLatByMeters <- function(lat, buffer_meters) {

  radius_earth_meters <- 6378137
  buffer_deg_lat <- (buffer_meters / radius_earth_meters) * (180 / pi)
  return(lat + buffer_deg_lat)
  
}

adjustLongByMeters <- function(lon, lat, buffer_meters) {

  radius_earth_meters <- 6378137
  width_deg_long <- cos(lat * pi / 180) * 2 * pi * radius_earth_meters / 360
  buffer_deg_long <- buffer_meters / width_deg_long
  return(lon + buffer_deg_long)
  
}

# Function to check if there is a run of at least min_rl less than the specified value
has_consecutive_run <- function(dt, specified_value) {
  logical_vector <- dt < specified_value
  rle_result <- rle(logical_vector)
  min_rl <- 5
  any(rle_result$values & rle_result$lengths >= min_rl)
}

# Binary search to find the minimum specified value
find_minimum_maxt <- function(t) {
  dt <- diff(t)
  low <- min(dt)
  high <- max(dt)
  
  while (low+1 < high) {
    mid <- (low + high) / 2
    if (has_consecutive_run(dt, mid)) {
      high <- mid
    } else {
      low <- mid + .Machine$double.eps
    }
  }
  
  return(ceiling(low))
}

# Input
# gpsdata:     data frame containing animal GPS data
# utm.zone:    zone for UTM coordinates (with or without N/S designation)
# datum:       data projection (e.g. WGS84 or NAD83)
# buffer:      buffer space around the plot (m)
# ids:         identifiers of animals to be plotted
# include_mcp: if true, show min convex polygon; otherwise only show data on map
#
# Output
# mymap:       animal data, with or without min convex polygon, superimposed on map
#
# Usage example
# minConvexPolygon(gpsdata, 11, "WGS84", c(269, 284), TRUE)
# minConvexPolygon(gpsdata, 12, "NAD83, c(123, 234, 456), FALSE)
#
# Note - to get MCP/map to display in shiny app, call function without saving results to variable
minConvexPolygon <- function(gpsdata, utm.zone, datum, buffer, ids, include_mcp) {
  
  # convert animal id to string as needed by geom_polygon
  gpsdata$id <- as.character(gpsdata$id)
  
  # Generate the basemap using all data
  gpsdata.sf <- gpsdata[gpsdata$id %in% ids, c("id", "xdata", "ydata")]

  # Add rows with buffering
  xmin <- min(gpsdata.sf$xdata) - buffer
  xmax <- max(gpsdata.sf$xdata) + buffer
  ymin <- min(gpsdata.sf$ydata) - buffer
  ymax <- max(gpsdata.sf$ydata) + buffer
  # replacing following area calculation line with separate calculations and if
  # test to avoid
  # "NAs produced by integer overflow" error due to (xmax - xmin) * (ymax - ymin)
  # which allows for larger areas to be calculated
  # area = (xmax-xmin)*(ymax-ymin) / 1000000000.0
  x = xmax - xmin
  y = ymax - ymin
  if (x > y)
    area = x / 1000000000.0 * y
  else
    area = y / 1000000000.0 * x

  # Choose the map zoom based on area
  if (area < 1.0) {
    zoom = 10
  } else if (area < 20.0) {
    zoom = 9
  } else {
    zoom = 8
  }

  # printf(paste("x:", xmin, ",", xmax, "\n"))
  # printf(paste("y:", ymin, ",", ymax, "\n"))
  # printf(paste("area:", area, "\n"))
  # printf(paste("zoom:", zoom, "\n"))
  gpsdata.sf[base::nrow(gpsdata.sf)+1,] = list("dummy", xmin, ymin)
  gpsdata.sf[base::nrow(gpsdata.sf)+1,] = list("dummy", xmin, ymax)
  gpsdata.sf[base::nrow(gpsdata.sf)+1,] = list("dummy", xmax, ymin)
  gpsdata.sf[base::nrow(gpsdata.sf)+1,] = list("dummy", xmax, ymax)
  
  # Set the coordinates in the sf object
  crsstr <- paste("+proj=utm +zone=", utm.zone, " +datum=", datum, " +units=m +no_defs", sep="")
  gpsdata.sf <- st_as_sf(gpsdata.sf, coords = c("xdata", "ydata"), crs = crsstr)
  
  # Transform the coordinate reference system (CRS)
  gpsdata.sfgeo <- st_transform(gpsdata.sf, crs = "+proj=longlat") 
  
  # base::print(min(st_coordinates(gpsdata.sfgeo)[, 1]))
  # base::print(min(st_coordinates(gpsdata.sfgeo)[, 2]))
  # base::print(max(st_coordinates(gpsdata.sfgeo)[, 1]))
  # base::print(max(st_coordinates(gpsdata.sfgeo)[, 2]))
  
  # Get the base map
  mybasemap <- get_stadiamap(bbox = c(left = min(st_coordinates(gpsdata.sfgeo)[, 1]),
                                      bottom = min(st_coordinates(gpsdata.sfgeo)[, 2]),
                                      right = max(st_coordinates(gpsdata.sfgeo)[, 1]),
                                      top = max(st_coordinates(gpsdata.sfgeo)[, 2])),
                             zoom = zoom)
  
  
  # Filter data based on selected animal IDs
  ids <- as.character(ids)
  gpsdata.sf <- gpsdata.sf[gpsdata.sf$id %in% ids, ]
  
  # Prepare data and minimum convex polygon
  gpsdata.mcp <- st_buffer(gpsdata.sf, dist = 0) 
  gpsdata.sfgeo <- st_transform(gpsdata.sf, crs = "+proj=longlat") 
  gpsdata.geo <- cbind(st_coordinates(gpsdata.sfgeo), id = gpsdata.sfgeo$id) 
  
  # Extract coordinates and attributes from gpsdata.sf and combine into a dataframe
  coords <- st_coordinates(gpsdata.sfgeo)
  attributes <- st_drop_geometry(gpsdata.sfgeo)
  gpsdata_df <- cbind(attributes, coords)
  
  # Plot data points on the map
  mymap <- ggmap(mybasemap) +
    geom_point(data = gpsdata_df, aes(x = X, y = Y, color = factor(id)), size = 1.5) + 
    theme(legend.position = c(-0.2, 0.90)) +
    labs(x = "Longitude", y = "Latitude") 
  
  # Add a polygon
  if (include_mcp) {
    
    # Initialize a list to store convex hull polygons for each ID
    hull_list <- list()
    
    # Loop over unique IDs and compute convex hull for each
    for (unique_id in base::unique(gpsdata_df$id)) {
      subset_data <- gpsdata_df[gpsdata_df$id == unique_id, ]
      hull_points <- subset_data[chull(subset_data$X, subset_data$Y), ]
      hull_list[[unique_id]] <- hull_points
    }
    
    # Combine all hull polygons into a single data frame
    all_hulls <- do.call(rbind, hull_list)
    
    # Add the combined convex hull polygons to the map
    mymap <- mymap +
      geom_polygon(data = all_hulls, aes(x = X, y = Y, fill = factor(id), colour = factor(id)), alpha = 0.5)
  }
  
  return(mymap)

}

