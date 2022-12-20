minConvexPolygon <- function(gpsdata, utm.zone, datum, ids) {

   # Input
   # gpsdata: Data frame containing animal GPS data
   # utm.zone: Zone for UTM coordinates (with or without N/S designation)
   #
   # Output
   # Minimum convex polygon data superimposed on map
   #
   # Usage example
   # minConvexPolygon(gpsdata, 11, "WGS84", c(269, 284))
   # minConvexPolygon(gpsdata, 12, "NAD83, c(123, 234, 456))
   #
   # Note - to get MCP/map to display, call function without saving results to variable

   library(sp)
   library(adehabitatHR)
   library(scales)
   library(ggmap) 

   # convert animal id to string as needed by geom_polygon
   gpsdata$id <- as.character(gpsdata$id)

   # Generate the basemap using all data
   gpsdata.sp <- gpsdata[, c("id", "xdata", "ydata")]
   coordinates(gpsdata.sp) <- c("xdata", "ydata")
   crsstr <- paste("+proj=utm +zone=", utm.zone, " +datum=", datum, " +units=m +no_defs", sep="")
   proj4string(gpsdata.sp) <- CRS(crsstr)
   gpsdata.spgeo <- spTransform(gpsdata.sp, CRS("+proj=longlat"))
   mybasemap <- get_stamenmap(bbox = c(left = min(gpsdata.spgeo@coords[,1])-0.005, 
                                    bottom = min(gpsdata.spgeo@coords[,2])-0.005, 
                                    right = max(gpsdata.spgeo@coords[,1])+0.005, 
                                    top = max(gpsdata.spgeo@coords[,2])+0.005), 
                                    zoom = 8)

   # Filter data based on selected animal IDs then plot on basemap
   ids <- as.character(ids)
   gpsdata.sp <- gpsdata.sp[gpsdata.sp$id %in% ids, ]
   gpsdata.mcp <- mcp(gpsdata.sp, percent = 100)
   gpsdata.spgeo <- spTransform(gpsdata.sp, CRS("+proj=longlat"))
   gpsdata.mcpgeo <- spTransform(gpsdata.mcp, CRS("+proj=longlat"))
   gpsdata.geo <- data.frame(gpsdata.spgeo@coords, id = gpsdata.spgeo@data$id)

   # Without minimum convex polygon
   mymap.hr <- ggmap(mybasemap) +
  	       geom_point(data = gpsdata.geo, 
	       aes(x = xdata, y = ydata, colour = id), size = 0.8, alpha = 0.5)  +
	       theme(legend.position = c(-0.2, 0.90)) +
	       labs(x = "Longitude", y = "Latitude")

   # With minimum convex polygon
   mymap.mcp.hr <- mymap.hr +
               geom_polygon(data = fortify(gpsdata.mcpgeo),
	       aes(long, lat, colour = id, fill = id),
	       alpha = 0.3) # alpha sets the transparency

   maps <- list()
   maps <- append(maps, list(mymap.hr))
   maps <- append(maps, list(mymap.mcp.hr))

   return(maps)
}
