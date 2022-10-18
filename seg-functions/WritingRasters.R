# To save raster, first convert from an mkde object to a raster object
# and then write using the raster package

# Typical generation of mkde object

mv.dat <- initializeMovementData(t, x, y, sig2obs=25.0, t.max=185.0)
mkde.obj <- initializeMKDE2D(xmin, cell.sz, nx, ymin, cell.sz, ny)
dens.res <- initializeDensity(mkde.obj, mv.dat)
mkde.obj <- dens.res$mkde.obj

# Conversion to raster object and writing using ascii format. Note
# that format is inferred from the filename extension

mkde.rst <- mkdeToRaster(mkde.obj)
r <- writeRaster(mkde.rst, filename="bobraster.asc")