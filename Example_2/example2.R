library(mkde)
library(raster)

# Read GPS movement data from file
# Note to Mona - the file that you read from the Shiny app will replace "Condor-movebank-trunc.csv"
gpsdata <- read.csv("Condor-movebank-trunc.csv", header=TRUE)

# Convert date/time (yyyy-mm-dd hh:mm:ss.sss) to epoch minutes
gpsdata$timestamp = as.numeric(as.POSIXct(gpsdata$timestamp)) / 60

xmin <- min(gpsdata$utm.easting)
ymin <- min(gpsdata$utm.northing)
xmax <- max(gpsdata$utm.easting)
ymax <- max(gpsdata$utm.northing)
xrange <- xmax-xmin
yrange <- ymax-ymin
cell.sz <- 10
nx = xrange/cell.sz
ny = yrange/cell.sz

# Note to Mona - the value that	you read from the Shiny	app will replace
# Note to Mona - the hardcoded values (25.0 and 185.0) assigned	to the	
# Note to Mona - variables sig2obs and t.max
mv.dat <- initializeMovementData(gpsdata$timestamp, gpsdata$utm.easting, gpsdata$utm.northing,
       sig2obs=25.0, t.max=185.0)
mkde.obj <- initializeMKDE2D(xmin, cell.sz, nx, ymin, cell.sz, ny)
dens.res <- initializeDensity(mkde.obj, mv.dat)

mkde.obj <- dens.res$mkde.obj
mv.dat <- dens.res$move.dat

# Plot results
plotMKDE(mkde.obj)
