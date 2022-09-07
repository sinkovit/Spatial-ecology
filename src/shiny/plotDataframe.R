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

calculateRaster2D <- function(gpsdata, sig2obs, t.max, cell.sz,
                              xmin, xmax, ymin, ymax) {

# Calculate rasters for each indivdual in a dataframe using mkde
# package and return the results as a list of rasters
#
# gpsdata - data frame containing animal movement data
# sig2obs - location error variance
# t.max - maximum time between locations
# cell.sz - cell size
# xmin, xmax - min/max x coordinate
# ymin, ymax - min/max y coordinate

  library(mkde)
  library(raster)

  # Initialize list of rasters
  rasters <- list()

  for (id in unique(gpsdata$id)) {
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

      Sys.sleep(3)

      mv.dat <- initializeMovementData(t, x, y, sig2obs=25.0, t.max=185.0)
      mkde.obj <- initializeMKDE2D(xmin, cell.sz, nx, ymin, cell.sz, ny)
      dens.res <- initializeDensity(mkde.obj, mv.dat)
      mkde.obj <- dens.res$mkde.obj
      rasters <- append(rasters, list(mkde.obj))
   }
   return(rasters)
}
