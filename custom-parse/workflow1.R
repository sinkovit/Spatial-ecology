source("loadDataframeFromFile.R")
source("preprocessDataframe.R")
source("calculateRaster2D.R")

# Set parameters

file <- "Data/CondorFull.csv"
sig2obs <- 25.0
t.max <- 185
cell.sz <- 4000

# Read and pre-process data

gpsdata <- loadDataframeFromFile(file)
gpsdata <- preprocessDataframe(gpsdata)

# Extent can be calculated in different ways, for example from data
# set itself, from digital elevation model or manually set. For now,
# just using min/max values for the GPS readings.

xmin <- min(gpsdata$xdata)
xmax <- max(gpsdata$xdata)
ymin <- min(gpsdata$ydata)
ymax <- max(gpsdata$ydata)

calculateRaster2D(gpsdata, sig2obs, t.max, cell.sz, xmin, xmax, ymin, ymax)
