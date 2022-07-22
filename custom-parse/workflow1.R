source("loadDataframeFromFile.R")
source("preprocessDataframe.R")
source("calculateRaster2D.R")


file <- "Data/CondorFull.csv"
sig2obs <- 25.0
t.max <- 185
cell.sz <- 4000

gpsdata <- loadDataframeFromFile(file)
gpsdata <- preprocessDataframe(gpsdata)
calculateRaster2D(gpsdata, sig2obs, t.max, cell.sz)