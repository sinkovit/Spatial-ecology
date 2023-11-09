library(sp)
library(adehabitatHR)
library(scales)
library(ggmap)
library(fs)
library(ggplot2)
library(mkde)
library(raster)
library(broom)

source("processDataframe.R")

gpsdata <- read.csv("/Users/robertsinkovits/SEGdata/269.csv", header=TRUE)
gpsdata <- preprocessDataframe(gpsdata)
gpsdata <- gpsdata[[1]]

buffer = 1000
xmin <- min(gpsdata$xdata) - buffer
ymin <- min(gpsdata$ydata) - buffer
xmax <- max(gpsdata$xdata) + buffer
ymax <- max(gpsdata$ydata) + buffer

print(xmin)
print(xmax)
print(ymin)
print(ymax)

zoom = 9
gpsdata.sp <- data.frame(label=character(), x=double(), y=double())
gpsdata.sp[1,] = list("dummy", xmin, ymin)
gpsdata.sp[2,] = list("dummy", xmin, ymax)
gpsdata.sp[3,] = list("dummy", xmax, ymin)
gpsdata.sp[4,] = list("dummy", xmax, ymax)

cell.sz = 3000
xrange <- xmax - xmin
yrange <- ymax - ymin
nx <- as.integer(xrange/cell.sz)
ny <- as.integer(yrange/cell.sz)

id = "269"
sig2obs = 25.0
t.max = 185.0
x <- gpsdata[which(gpsdata$id == id), "xdata"]
y <- gpsdata[which(gpsdata$id == id), "ydata"]
t <- gpsdata[which(gpsdata$id == id), "time"]

mv.dat <- initializeMovementData(t, x, y, sig2obs = sig2obs, t.max = t.max)
mkde.obj <- initializeMKDE2D(xmin, cell.sz, nx, ymin, cell.sz, ny)
dens.res <- initializeDensity(mkde.obj, mv.dat)
mkde.obj <- dens.res$mkde.obj

rst.mkde = mkdeToRaster(mkde.obj)

probs <- c(0.9999, 0.999, 0.99, 0.9, 0.5)
probs <- sort(probs[probs > 0])
probs_max <- tail(probs, n=1)
contour_probs <- probs

cont <- computeContourValues(mkde.obj, prob = contour_probs)

rst.cont = cut(rst.mkde, breaks = c(cont$threshold, max(values(rst.mkde),
                                                          na.rm = TRUE)))
plot(rst.cont)
contour_display <- contour(rst.mkde, add = T, levels = cont$threshold,
                             lwd = 1.0, drawlabels = FALSE)

## Start additions for plotting mkde on map
crsstr <- paste("+proj=utm +zone=", 11, " +datum=", "WGS84", " +units=m +no_defs", sep="")
results <- list(raster = rst.mkde, contour = cont, cut = rst.cont,
                probabilities = contour_probs)
raster.contour <- rasterToContour(rst.mkde, levels = cont$threshold)
raster.contour = spChFIDs(raster.contour, paste(contour_probs, "% Contour Line", sep=""))
proj4string(raster.contour) = CRS(crsstr)
raster.contour <- spTransform(raster.contour, CRS("+proj=longlat"))
tidydta2 <- tidy(raster.contour, group=group)

good <- TRUE
for (gname in unique(tidydta2$group)) {
    x1 <-  tidydta2$lat[which(tidydta2$group == gname)]
    y1 <-  tidydta2$long[which(tidydta2$group == gname)]
    if (x1[1] != x1[length(x1)] && y1[1] != y1[length(y1)]) {
       good <- FALSE
    }
}
print(good)

coordinates(gpsdata.sp) <- c("x", "y")
proj4string(gpsdata.sp) <- CRS(crsstr)
gpsdata.spgeo <- spTransform(gpsdata.sp, CRS("+proj=longlat"))

mybasemap <- get_stadiamap(bbox = c(left = min(gpsdata.spgeo@coords[,1]),
	  bottom = min(gpsdata.spgeo@coords[,2]),
	  right = max(gpsdata.spgeo@coords[,1]),
	  top = max(gpsdata.spgeo@coords[,2])),
	  zoom = zoom)

mymap <- ggmap(mybasemap) +
      geom_polygon(aes(x=long, y=lat, group=group), 
      data=tidydta2, 
      alpha=.2, linewidth=.2)

plot(mymap)