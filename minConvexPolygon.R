minConvexPolygon <- function(gpsdata, utm.zone) {

   # Input
   # gpsdata: Data frame containing animal GPS data
   # utm.zone: Zone for UTM coordinates (with or without N/S designation)
   #
   # Output
   # Minimum convex polygon data superimposed on map
   #
   # Usage example
   # minConvexPolygon(gpsdata, 11)
   # minConvexPolygon(gpsdata, "11N")
   #
   # Note - to get MCP/map to display, call function without saving results to variable

   library(sp)
   library(adehabitatHR)
   library(scales)
   library(ggmap) 

   # geom_polygon function needs the animal id to be an integer
   gpsdata$id <- as.character(gpsdata$id)

   gpsdata.sp <- gpsdata[, c("id", "xdata", "ydata")]
   coordinates(gpsdata.sp) <- c("xdata", "ydata")
   crsstr <- paste("+proj=utm +zone=", utm.zone, " +datum=WGS84 +units=m +no_defs", sep="")
   proj4string(gpsdata.sp) <- CRS(crsstr)
   gpsdata.mcp <- mcp(gpsdata.sp, percent = 100)
   #plot(gpsdata.sp, col = as.factor(gpsdata.sp@data$id), pch = 16)
   #plot(gpsdata.mcp, col = alpha(1:5, 0.5), add = TRUE)

   gpsdata.spgeo <- spTransform(gpsdata.sp, CRS("+proj=longlat"))
   gpsdata.mcpgeo <- spTransform(gpsdata.mcp, CRS("+proj=longlat"))
   mybasemap <- get_stamenmap(bbox = c(left = min(gpsdata.spgeo@coords[,1])-0.005, 
                                    bottom = min(gpsdata.spgeo@coords[,2])-0.005, 
                                    right = max(gpsdata.spgeo@coords[,1])+0.005, 
                                    top = max(gpsdata.spgeo@coords[,2])+0.005), 
                                    zoom = 8)

   gpsdata.geo <- data.frame(gpsdata.spgeo@coords, id = gpsdata.spgeo@data$id)

   mymap.hr <- ggmap(mybasemap) + 
   	    geom_polygon(data = fortify(gpsdata.mcpgeo),  
	    aes(long, lat, colour = id, fill = id),
	    alpha = 0.3) + # alpha sets the transparency
  	    geom_point(data = gpsdata.geo, 
	    aes(x = xdata, y = ydata, colour = id))  +
	    theme(legend.position = c(0.15, 0.80)) +
	    labs(x = "Longitude", y = "Latitude")

   return(mymap.hr)
}
