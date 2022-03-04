library(mkde)
library(raster)
library(move)

# Note to Mona - The following information will need to be collected by the RShiny app
# (1) sig2obs
# (2) t.max
# (3) dimensionality (2D or 3D)
# (4) study (Movebank study ID)
# (5) local_data (Boolean for choosing between local data and pull from Movebank)
# (6) username (Movebank user name if pulling from MB)
# (7) password (Movebank password if pulling from MB)

sig2obs <- 25.0
t.max <- 185.0
dimensionality <- 2 # Not currently used in script
study <- 408181528  # This is the California Condor study in Movebank
local_data <- TRUE
username <- "yourusername"
password <- "yourpassword"

# Generate file name where Movebank data is or will be stored
outfile <- paste(getwd(), "/Study-", toString(study), ".RData", sep="")

# Load local data or pull from Movebank and save a local copy
if (local_data) {
   print("Loading local copy of Movebank study")
   load(outfile)
} else {
   print("Loading study from Movebank")
   login <- movebankLogin(username=username, password=password)
   data <- getMovebankData(study=study, login=login)
   save(data, file=outfile)
}

# Data preparation
# (1) Convert lat/long to aeqd (Azimuthal Equidistance) projection
# (2) Convert MoveStack to data frame
# (3) Convert timestamps to epoch minutes
data <- spTransform(data, center=TRUE)
data_df <- as.data.frame(data)
data_df$time = as.numeric(as.POSIXct(data_df$timestamp)) / 60

# Pare down data for test purposes
x <- data_df$location_long.1[1:100]
y <- data_df$location_lat.1[1:100]
t <- data_df$time[1:150]

# Get data characteristics and prepare problem
xmin <- min(x)
ymin <- min(y)
xmax <- max(x)
ymax <- max(y)
xrange <- xmax-xmin
yrange <- ymax-ymin
cell.sz <- 10
nx = as.integer(xrange/cell.sz)
ny = as.integer(yrange/cell.sz)
print("Home range dimensions (pixels/voxels)")
print(paste("nx =", nx, "ny =", ny))
print("Home range dimensions (meters)")
print(paste("xrange =", xrange, "yrange =", yrange))

# Note to Mona - the value that	you read from the Shiny	app will replace
# Note to Mona - the hardcoded values (25.0 and 185.0) assigned	to the	
# Note to Mona - variables sig2obs and t.max
# Note to Mona - this is exactly what you had done in previous examples

# Create home range using mkde
mv.dat <- initializeMovementData(t, x, y, sig2obs=25.0, t.max=185.0)
mkde.obj <- initializeMKDE2D(xmin, cell.sz, nx, ymin, cell.sz, ny)
dens.res <- initializeDensity(mkde.obj, mv.dat)
mkde.obj <- dens.res$mkde.obj
mv.dat <- dens.res$move.dat

# Plot results
plotMKDE(mkde.obj)
