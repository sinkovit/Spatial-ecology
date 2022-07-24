r <- raster(nrows=39, ncols=50)
r <- setValues(r, apply(mkde.obj$d,1,c))
plot(r)
