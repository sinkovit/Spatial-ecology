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

  library(sp)
  library(adehabitatHR)
  library(scales)

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
    #print(paste("error_message 2 =", error_message))
    return(NULL)
  })
}


# Create home range using mkde
getMKDEData <- function(data_df, index, sig2obs, tmax, cell.size) {
  ids <- unique(data_df$local_identifier)
  id <- ids[index]

  x <- data_df[which(data_df$local_identifier == id), "location_long.1"]
  y <- data_df[which(data_df$local_identifier == id), "location_lat.1"]
  t <- data_df[which(data_df$local_identifier == id), "time"]
  
  # Get data range; set grid size and cell size
  xmin <- min(x)
  ymin <- min(y)
  xmax <- max(x)
  ymax <- max(y)
  xrange <- xmax-xmin
  yrange <- ymax-ymin

  if (xrange >= yrange) {
    nx <- 50
    ny <- as.integer(nx * (yrange/xrange))
  } else {
    ny <- 50
    nx <- as.integer(ny * (xrange/yrange))
  }  

  mv.dat <- initializeMovementData(t, x, y, sig2obs = sig2obs, t.max = tmax)
  mkde.obj <- initializeMKDE2D(xmin, cell.size, nx, ymin, cell.size, ny)
  #dens.res <- initializeDensity(mkde.obj, mv.dat)
  mkde.obj <- dens.res$mkde.obj
  mv.dat <- dens.res$move.dat
  return(mkde.obj)
}

# Returns TRUE of the parameter is NULL, a string with no non-space characters,
# or length < 1; otherwise returns FALSE
isEmpty <- function(x) {
  if (is.null(x))
    return(TRUE)
  if (length(x) < 1)
    return(TRUE)
  if (is.character(x) && nchar(str_replace_all(x, " ", "")) < 1)
    return(TRUE)
  return(FALSE)
}
