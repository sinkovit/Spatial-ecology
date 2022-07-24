library(mkde)
library(raster)

# Read GPS movement data from file
# Note to Mona - the file that you read from the Shiny app will replace "pandabob.txt"
gpsdata <- read.table("pandabob.txt", header=TRUE)

xmin <- min(gpsdata$x)
ymin <- min(gpsdata$y)
xmax <- max(gpsdata$x)
ymax <- max(gpsdata$y)
xrange <- xmax-xmin
yrange <- ymax-ymin
cell.sz <- 10
nx = xrange/cell.sz
ny = yrange/cell.sz

# Note to Mona - the value that you read from the Shiny app will replace
# Note to Mona - the hardcoded values (25.0 and 185.0) assigned to the
# Note to Mona - variables sig2obs and t.max
mv.dat <- initializeMovementData(gpsdata$time, gpsdata$x, gpsdata$y, sig2obs=25.0, t.max=185.0)
mkde.obj <- initializeMKDE2D(xmin, cell.sz, nx, ymin, cell.sz, ny)
dens.res <- initializeDensity(mkde.obj, mv.dat)

mkde.obj <- dens.res$mkde.obj
mv.dat <- dens.res$move.dat

# Plot results
plotMKDE(mkde.obj)
