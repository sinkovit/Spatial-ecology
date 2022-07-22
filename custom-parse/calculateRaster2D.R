calculateRaster2D <- function(gpsdata, sig2obs, t.max, cell.sz,
                              xmin, xmax, ymin, ymax) {

# gpsdata - data frame containing animal movement data
# sig2obs - location error variance
# t.max - maximum time between locations
# cell.sz - cell size
# xmin, xmax - min/max x coordinate
# ymin, ymax - min/max y coordinate

  library(mkde)
  library(raster)

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
      mv.dat <- dens.res$move.dat
      plotMKDE(mkde.obj)
   }
}
