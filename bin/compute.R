# This software is Copyright © 2022 The Regents of the University of California.
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


library(sp)
library(adehabitatHR)
library(scales)
library(ggmap)
library(fs)


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
  printf <- function(...) cat(sprintf(...))

  gpsdata.sp <- data_df[, c("id", "xdata", "ydata")]
  coordinates(gpsdata.sp) <- c("xdata", "ydata")
  area_mcp <- mcp.area(gpsdata.sp, unin="m", unout=areaUnits, percent=100)

  animals <- as.list(sort(unique(data_df$id)))
  max_pixels <- c(30, 60, 100, 300)
  areaString <- paste("Area (", areaUnits, ")", sep="")

  result <- data.frame(id = numeric())
  result[ , 'Easting (min)'] <- numeric()
  result[ , 'Easting (max)'] <- numeric()
  result[ , 'Northing (min)'] <- character()
  result[ , 'Northing (max)'] <- character()
  result[ , areaString] <- character()
  row.index <- 1
  
  tryCatch({
    for (local_id in animals) {
      x_minmax = range(data_df[which(data_df$id == local_id), "xdata"])
      y_minmax = range(data_df[which(data_df$id == local_id), "ydata"])
      t_minmax = range(data_df[which(data_df$id == local_id), "time"])
      x_range <- x_minmax[2] - x_minmax[1]
      y_range <- y_minmax[2] - y_minmax[1]
      
      area_raw <- area_mcp[1, as.character(local_id)]
      if (area_raw < 100) {
        area <- sprintf("%.3f", area_raw)
      } else if (area_raw < 1000000) {
        area <- as.integer(area_raw)
      } else {
        area <- sprintf("%.3e", area_raw)      	
      }
      
      row <- c(local_id, round(x_minmax[1]), round(x_minmax[2]), round(y_minmax[1]), round(y_minmax[2]), area)
      row.tail = c()
      
      # Loop over pixel sizes
      for (max_pix in max_pixels) {
        nx <- as.integer(x_range/max_pix)
        ny <- as.integer(y_range/max_pix)
        dims <- sprintf("%dx%d", nx, ny)
        if(row.index == 1) {
          label <- sprintf("Grid \n (%dm)", max_pix)
          result[ , label] <- character()
        }
        value <- paste(dims)
        row.tail <- append(row.tail, c(value))
      }
      
      row <- append(row,row.tail)
      result[row.index, ] <- row
      row.index <- row.index + 1
      
    }
    return(result)
  },
  error = function(error_message) {
    return(NULL)
  })
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
calculateRaster2D <- function (gpsdata, id, sig2obs, t.max, cell.sz, xmin, xmax,
                               ymin, ymax) {

  x <- gpsdata[which(gpsdata$id == id), "xdata"]
  y <- gpsdata[which(gpsdata$id == id), "ydata"]
  t <- gpsdata[which(gpsdata$id == id), "time"]
  
  xrange <- xmax - xmin
  yrange <- ymax - ymin
  
  nx <- as.integer(xrange/cell.sz)
  ny <- as.integer(yrange/cell.sz)
  
  print(paste("identifier =", id))
  print("Home range dimensions (pixels/voxels)")
  print(paste("nx =", nx, "ny =", ny))
  print("Home range dimensions (meters)")
  print(paste("xrange =", xrange, "yrange =", yrange))
  print(paste("cell size =", cell.sz))
  print("---------------------------------------")
  
  mv.dat <- initializeMovementData(t, x, y, sig2obs = sig2obs, t.max = t.max)
  mkde.obj <- initializeMKDE2D(xmin, cell.sz, nx, ymin, cell.sz, ny)
  dens.res <- initializeDensity(mkde.obj, mv.dat)
  mkde.obj <- dens.res$mkde.obj
  
  return (mkde.obj)
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
createContour <- function(mkde2d.obj, probs, utm.zone, datum, raster = FALSE,
                          shape = FALSE, basename = NULL, all = TRUE) {

  # Create raster from MKDE object
  rst.mkde = mkdeToRaster(mkde2d.obj)

  # Sort prob contours into ascending order and remove values <= 0
  probs <- sort(probs[probs > 0])
  probs_max <- tail(probs, n=1)

  # Set filenames and contour probabilites based on whether all
  # contours or outer contour are used
  if (all) {
    filename <- paste(basename, "_allcontour", sep="")
    contour_probs <- probs
  } else {
    filename <- paste(basename, "_outercontour", sep="")
    contour_probs <- tail(probs, n=1)
  }

  # Create CRS string
  crsstr <- paste("+proj=utm +zone=", utm.zone, " +datum=", datum, " +units=m +no_defs", sep="")
  
  # Plot contours
  cont <- computeContourValues(mkde2d.obj, prob = contour_probs)
  rst.cont = cut(rst.mkde, breaks = c(cont$threshold, max(values(rst.mkde), na.rm = T)))
  plot(rst.cont)
  contour_display <- contour(rst.mkde, add = T, levels = cont$threshold, lwd = 1.0, drawlabels = FALSE)

  if((raster == TRUE || shape == TRUE) && !is.null(basename)) {
    output_file <- paste(path_home(), "/", basename, sep = "")
    print(paste("output_file =", output_file))
    raster.contour <- rasterToContour(rst.mkde, levels = cont$threshold)
    proj4string(raster.contour) = CRS(crsstr)
    
    # Create raster of contours
    if(raster == TRUE) {
      printf("Writing raster to file %s.asc...", output_file)
      writeRaster(rst.cont, output_file, format = "ascii", overwrite = T)
      printf("done\n")
    }

    # Create shapefiles of contours
    if(shape == TRUE && !is.null(basename)) {
      raster.contour = spChFIDs(raster.contour, paste(contour_probs, "% Contour Line", sep=""))
      proj4string(raster.contour) = CRS(crsstr)
      printf("Writing shape to 5 files %s.*...", output_file)
      shapefile(x = raster.contour, file = output_file, overwrite = T)
      printf("done\n")
    }
  }

  # return(0)
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
  
  # library(sp)
  # library(adehabitatHR)
  # library(scales)
  # library(ggmap) 
  
  # convert animal id to string as needed by geom_polygon
  gpsdata$id <- as.character(gpsdata$id)
  
  # Generate the basemap using all data
  gpsdata.sp <- gpsdata[, c("id", "xdata", "ydata")]
  
  # Add rows with buffering
  xmin <- min(gpsdata.sp$xdata) - buffer
  xmax <- max(gpsdata.sp$xdata) + buffer
  ymin <- min(gpsdata.sp$ydata) - buffer
  ymax <- max(gpsdata.sp$ydata) + buffer
  printf(paste("x:", xmin, ",", xmax, "\n"))
  printf(paste("y:", ymin, ",", ymax, "\n"))
  gpsdata.sp[nrow(gpsdata.sp)+1,] = list("dummy", xmin, ymin)
  gpsdata.sp[nrow(gpsdata.sp)+1,] = list("dummy", xmin, ymax)
  gpsdata.sp[nrow(gpsdata.sp)+1,] = list("dummy", xmax, ymin)
  gpsdata.sp[nrow(gpsdata.sp)+1,] = list("dummy", xmax, ymax)
  
  coordinates(gpsdata.sp) <- c("xdata", "ydata")
  crsstr <- paste("+proj=utm +zone=", utm.zone, " +datum=", datum, " +units=m +no_defs", sep="")
  proj4string(gpsdata.sp) <- CRS(crsstr)
  gpsdata.spgeo <- spTransform(gpsdata.sp, CRS("+proj=longlat"))
  mybasemap <- get_stamenmap(bbox = c(left = min(gpsdata.spgeo@coords[,1]),
                                      bottom = min(gpsdata.spgeo@coords[,2]),
                                      right = max(gpsdata.spgeo@coords[,1]),
                                      top = max(gpsdata.spgeo@coords[,2])),
                             zoom = 8)
  
  # Filter data based on selected animal IDs
  ids <- as.character(ids)
  gpsdata.sp <- gpsdata.sp[gpsdata.sp$id %in% ids, ]
  
  # Prepare data and minimum convex polygon
  gpsdata.mcp <- mcp(gpsdata.sp, percent = 100)
  gpsdata.spgeo <- spTransform(gpsdata.sp, CRS("+proj=longlat"))
  gpsdata.mcpgeo <- spTransform(gpsdata.mcp, CRS("+proj=longlat"))
  gpsdata.geo <- data.frame(gpsdata.spgeo@coords, id = gpsdata.spgeo@data$id)
  
  # Plot data points on basemap
  # Note that kludge below needed so that code works everywhere

  if ('xdata' %in% colnames(gpsdata.geo) && 'ydata' %in% colnames(gpsdata.geo)) {
     mymap <- ggmap(mybasemap) +
              geom_point(data = gpsdata.geo, 
              aes(x = xdata, y = ydata, colour = id), size = 0.8, alpha = 0.5)  +
              theme(legend.position = c(-0.2, 0.90)) +
              labs(x = "Longitude", y = "Latitude")
  } else {
     mymap <- ggmap(mybasemap) +
              geom_point(data = gpsdata.geo, 
              aes(x = coords.x1, y = coords.x2, colour = id), size = 0.8, alpha = 0.5)  +
              theme(legend.position = c(-0.2, 0.90)) +
              labs(x = "Longitude", y = "Latitude")
  }


  
  # Add minimum convex polygon
  if (include_mcp) {
    mymap <- mymap +
      geom_polygon(data = fortify(gpsdata.mcpgeo),
                   aes(long, lat, colour = id, fill = id),
                   alpha = 0.3) # alpha sets the transparency
  }
  
  return(mymap)
}

