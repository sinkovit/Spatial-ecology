# Script to demonstrate how building blocks of workflow fit
# together, with data read from file

source("loadDataframeFromFile.R")
source("preprocessDataframe.R")
source("calculateRaster2D.R")

# Set parameters (these will come from the Shiny user interface)
file <- "Data/CondorFull.csv"
#file <- "Data/pandabob.txt"
sig2obs <- 25.0
t.max <- 185
cell.sz <- 6000

# Read and pre-process data
gpsdata <- loadDataframeFromFile(file)
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
