createContour <- function(mkde2d.obj, probs, basename, utm.zone, datum, all=TRUE) {

   # Input
   # mkde2d.obj: MKDE object
   # probs: list of probabilities for image contours
   # basename: base file name for raster and shape files
   #           basename_[all|outer]contour.[asc|dbf|shp|shx]
   # all: if true, use all probability contours, if false use only outer contour
   #
   # Output
   # Completion code (0 for success)
   #
   # Usage examples
   # createContours(rasters[[1]], probs, "condor", 11, "WGS84", all=TRUE)
   # createContours(mkde2d.obj, probs, "tejon-pig", 11, "NAD83", all=FALSE)
   #
   # Note: see https://mhallwor.github.io/_pages/basics_SpatialPolygons

   library(raster)
   library(mkde)
   library(sp)

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
   contour_display <- contour(rst.mkde, add = T, levels = cont$threshold, lwd = 2.0)

   # Create raster of contours
   raster.contour <- rasterToContour(rst.mkde, levels = cont$threshold)
   proj4string(raster.contour) = CRS(crsstr)
   writeRaster(rst.cont, filename, format = "ascii", overwrite = T)

   # Create shapefiles of contours
   raster.contour = spChFIDs(raster.contour, paste(contour_probs, "% Contour Line", sep=""))
   proj4string(raster.contour) = CRS(crsstr)
   shapefile(x = raster.contour, file = filename, overwrite = T)

   return(0)
}
