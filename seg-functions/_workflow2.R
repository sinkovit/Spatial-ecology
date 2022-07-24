# Script to demonstrate how building blocks of workflow fit
# together, with data pulled from Movebank

source("loadDataframeFromMB.R")
source("preprocessDataframe.R")
source("calculateRaster2D.R")

# Set parameters (these will come from the Shiny user interface)
study <- 408181528      # California condor (have permission)
#study <- 1109284853     # Andean condor (do not have permission)
#study <- 1010101010101  # Nonexistent Movebank study
username <- "RSinkovits"
password <- "aBBa&0805&mb"
sig2obs <- 25.0
t.max <- 185
cell.sz <- 3000

# Read from Movebank
results <- loadDataframeFromMB(study, username, password)
gpsdata <- results[[1]]
errmsg <- results[[2]]
rm(results)

# Check for errors
if (errmsg != "") {
   print(errmsg)
   print("Bye!")
}

if (errmsg == "") {
  # Pre-process gpsdata
  gpsdata <- preprocessDataframe(gpsdata)

  # Spatial extent can be calculated in different ways, for example from
  # data set itself, from digital elevation model or manually set. For
  # now, just using min/max values for the GPS readings.
  xmin <- min(gpsdata$xdata)
  xmax <- max(gpsdata$xdata)
  ymin <- min(gpsdata$ydata)
  ymax <- max(gpsdata$ydata)

  # Generate a list of rasters
  rasters <- calculateRaster2D(gpsdata, sig2obs, t.max, cell.sz, xmin, xmax, ymin, ymax)

  # Plot the rasters for the two condors
  print("First raster")
  plotMKDE(rasters[[1]])
  Sys.sleep(3)
  print("Second raster")
  plotMKDE(rasters[[2]])
}
